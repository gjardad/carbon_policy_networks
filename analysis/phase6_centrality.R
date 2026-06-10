###############################################################################
# phase6_centrality.R  —  firm-level centrality + characterization (centrality-scheme input)
#
# Centrality = the marginal emission effect of targeting a single firm j (at the
# benchmark price), via the solver, decomposed into:
#   technique_j  = j's own within-firm abatement
#   composition_j = reallocation the network does in response to j being targeted
#   total_j      = technique_j + composition_j + scale_j   (= d log Z from targeting j)
#
# Answers:
#   1. Who are the central firms? (ranked list, NACE, emissions, ETS status)
#   2. Does the centrality set (top firms holding emission coverage = ETS) agree
#      with the actual ETS set?
#   3. Is the reallocation-only ranking (composition) very different from the
#      total-emission-reduction ranking (technique + reallocation)?
#
# Cost: one solve per candidate firm. CANDIDATE_N top emitters. ~N solves.
###############################################################################
suppressMessages({ library(dplyr); library(Matrix) })

.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1]); CODE_DIR <- file.path(project_root, "analysis")
source(file.path(CODE_DIR, "model_assembly.R"))
source(file.path(CODE_DIR, "phase5_model_solver.R"))

# ---- config ----
YEAR <- 2019; SCOPE <- "ets_neighbors"
PZ <- 80; SIGMA <- 0.5; RHO <- 0.5; ALPHA <- 4      # benchmark parameters (Option B)
CANDIDATE_N <- 1000                                  # rank the top-N emitters as candidates

# assembled bundle (cached in output_dir, machine-split; shared with cf_common.R)
bundle_file <- file.path(output_dir, sprintf("model_inputs_%d_%s.RData", YEAR, SCOPE))
if (file.exists(bundle_file)) {
  cat("Loading cached bundle:", basename(bundle_file), "\n"); load(bundle_file)
} else {
  cat("Assembling bundle (first run; cached for reuse) ...\n")
  bundle <- assemble_bundle(YEAR, SCOPE, proc_data, out_data); save(bundle, file = bundle_file)
}
bshare <- base_final_shares(bundle)
em <- which(bundle$e_bar > 0)
n  <- length(bundle$gamma)

base0 <- full_solve(0, SIGMA, RHO, ALPHA, bundle, bshare)   # no-policy reference
z0 <- base0$z; Z0 <- sum(z0[em]); Y0 <- sum(base0$x)
cand <- em[order(z0[em], decreasing = TRUE)][seq_len(min(CANDIDATE_N, length(em)))]
cat(sprintf("Candidates: top %d emitters (of %d emitters, %d firms total)\n",
            length(cand), length(em), n))

# 2-point decomposition of (base0 -> target-firm-j) into the three channels
dec1 <- function(sj) {
  Zj <- sum(sj$z[em])
  w  <- logmean(sj$z[em], z0[em]) / logmean(Zj, Z0)
  tech  <- sum(w * (log(sj$e[em]) - log(base0$e[em])))
  quant <- sum(w * (log(sj$x[em]) - log(base0$x[em])))
  scale <- log(sum(sj$x)) - log(Y0)
  c(total = log(Zj) - log(Z0), technique = tech, scale = scale, composition = quant - scale)
}

cat("Computing per-firm marginal targeting effects ...\n")
res <- lapply(seq_along(cand), function(k) {
  j <- cand[k]
  bj <- bundle; bj$tau <- as.integer(seq_len(n) == j)        # target ONLY firm j
  d  <- dec1(full_solve(PZ, SIGMA, RHO, ALPHA, bj, bshare))
  if (k %% 50 == 0) { cat(sprintf("  %d/%d\n", k, length(cand))); gc() }
  data.frame(vat = bundle$firms[j], nace2d = bundle$nace2d[j], z = z0[j],
             ets = bundle$tau[j], total = d["total"], technique = d["technique"],
             scale = d["scale"], composition = d["composition"], row.names = NULL)
}) %>% bind_rows()
write.csv(res, file.path(output_dir, "cf_centrality.csv"), row.names = FALSE)

# ============================ characterization =============================
jac <- function(a, b) length(intersect(a, b)) / length(union(a, b))
K   <- sum(res$ets)                                    # ETS firms among candidates
etsV <- res$vat[res$ets == 1]

cat("\n=== Q3: reallocation-only ranking vs total-reduction ranking ===\n")
cat(sprintf("Spearman(total, composition) = %.3f\n",
            cor(res$total, res$composition, method = "spearman")))
topT <- res$vat[order(res$total)][seq_len(K)]          # biggest total reducers
topC <- res$vat[order(res$composition)][seq_len(K)]    # biggest reallocation reducers
cat(sprintf("top-%d by total vs by composition: %d common (Jaccard %.2f)\n",
            K, length(intersect(topT, topC)), jac(topT, topC)))

cat("\n=== Q2: centrality set at ETS emission coverage vs the ETS set ===\n")
ets_cov <- sum(res$z[res$ets == 1])
for (rankvar in c("total", "composition")) {
  ord <- order(res[[rankvar]])                         # most-reducing first
  nsel <- which(cumsum(res$z[ord]) >= ets_cov)[1]; if (is.na(nsel)) nsel <- length(ord)
  selV <- res$vat[ord][seq_len(nsel)]
  cat(sprintf("  by %-11s: %d firms to match ETS coverage; %d are ETS (Jaccard vs ETS %.2f)\n",
              rankvar, nsel, length(intersect(selV, etsV)), jac(selV, etsV)))
}

cat("\n=== Q1: top 15 central firms (by total reduction) ===\n")
top15 <- res[order(res$total), ][seq_len(15), c("nace2d","z","ets","total","technique","composition")]
print(top15, row.names = FALSE)
cat(sprintf("\nWrote cf_centrality.csv (%d firms) to %s\n", nrow(res), output_dir))
