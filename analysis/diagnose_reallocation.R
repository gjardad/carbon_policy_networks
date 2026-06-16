###############################################################################
# diagnose_reallocation.R  —  WHERE does the reallocation (composition) channel
#   come from? Is it concentrated in a few very-high-intensity firms, even at
#   p_z = 80, as the high-e tail hypothesis claims?
#
# We attribute the composition channel FIRM BY FIRM. The path-integral
# decomposition gives, for each emitting firm i, its emission-weighted output
# change quant_i (path-integrated):
#     quant_i = sum_k  w_i^k ( log x_i^{k+1} - log x_i^k ),   w_i^k = LMDI weight
#     composition = sum_i quant_i  -  scale     (scale = d log gross output, ~small)
# so quant_i is firm i's contribution to the reallocation channel: a dirty firm
# whose buyers substitute away has x_i falling => quant_i < 0 => it CUTS aggregate
# emissions through reallocation. We then bucket firms by their emission COST SHARE
# at p_z, s_i = e_bar_i p_z / (1 + e_bar_i p_z)  (carbon cost / total cost, in (0,1)),
# and ask what share of the composition the high-cost-share firms account for.
#
# Also reports the cap counterfactual (composition when e_bar is winsorized) so the
# local vs RMD picture is comparable. Machine-portable: run on local-1 or RMD.
#   Rscript analysis/diagnose_reallocation.R
###############################################################################
suppressMessages({ library(dplyr); library(Matrix) })

.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1]); CODE_DIR <- file.path(project_root, "analysis")
source(file.path(CODE_DIR, "model_assembly.R"))
source(file.path(CODE_DIR, "phase5_model_solver.R"))

# ---- config (baseline calibration; NACE-4d inner nest) ----
YEAR <- 2019; SCOPE <- "ets_neighbors"; NEST_LEVEL <- "nace4d"
SB <- 0.1; SW <- 2.5; RHO <- 0.7; ALPHA <- 2; PZ <- 80
PATH_STEP <- 10

# ---- bundle (re-assemble if cache absent or predates nace4d) ----
bundle_file <- file.path(output_dir, sprintf("model_inputs_%d_%s.RData", YEAR, SCOPE))
if (file.exists(bundle_file)) { cat("Loading cached bundle\n"); load(bundle_file) }
if (!exists("bundle") || is.null(bundle[[NEST_LEVEL]])) {
  cat("Assembling bundle (first run, or cache lacks ", NEST_LEVEL, ") ...\n", sep = "")
  bundle <- assemble_bundle(YEAR, SCOPE, proc_data, out_data); save(bundle, file = bundle_file)
}
bundle <- build_nest(bundle, NEST_LEVEL)
bshare <- base_final_shares(bundle)
em <- which(bundle$e_bar > 0)
cat(sprintf("%d firms, %d ETS, %d emitters, %d %s sectors\n",
            length(bundle$gamma), sum(bundle$tau), length(em), bundle$nest$G, NEST_LEVEL))

# ---- per-firm path-integral attribution of the composition channel ----
attribute <- function(bdl, ebar = bdl$e_bar) {
  bdl$e_bar <- ebar; bdl <- build_nest(bdl, NEST_LEVEL)
  emk  <- which(bdl$e_bar > 0)
  grid <- sort(unique(c(seq(0, PZ, by = PATH_STEP), PZ)))
  sols <- lapply(grid, function(pz) full_solve(pz, SB, SW, RHO, ALPHA, bdl, bshare))
  Zof <- function(s) sum(s$z[emk]); Yof <- function(s) sum(s$x)
  tech <- 0; quant_i <- numeric(length(emk))
  for (k in seq_len(length(grid) - 1L)) {
    s0 <- sols[[k]]; s1 <- sols[[k + 1L]]
    w  <- logmean(s1$z[emk], s0$z[emk]) / logmean(Zof(s1), Zof(s0))
    tech    <- tech + sum(w * (log(s1$e[emk]) - log(s0$e[emk])))
    quant_i <- quant_i + w * (log(s1$x[emk]) - log(s0$x[emk]))
  }
  scale <- log(Yof(sols[[length(grid)]])) - log(Yof(sols[[1L]]))
  list(em = emk, dlogZ = log(Zof(sols[[length(grid)]])) - log(Zof(sols[[1L]])),
       technique = tech, scale = scale, composition = sum(quant_i) - scale, quant_i = quant_i)
}

cat("\nSolving baseline path 0 ->", PZ, "(actual ETS targeting) ...\n")
btau <- bundle$tau                                   # ETS targeting
b0 <- bundle; b0$tau <- btau
A <- attribute(b0)
cat(sprintf("\n=== AGGREGATE (p_z=%d, sB=%.2f sW=%.2f rho=%.2f alpha=%d) ===\n", PZ, SB, SW, RHO, ALPHA))
cat(sprintf("  dlogZ=%.4f  technique=%.4f  scale=%.4f  composition=%.4f\n",
            A$dlogZ, A$technique, A$scale, A$composition))

# ---- per-firm table: emission cost share + composition contribution ----
ebar_em <- bundle$e_bar[A$em]
ratio   <- ebar_em * PZ                               # carbon cost / baseline cost
cshare  <- ratio / (1 + ratio)                        # carbon cost / TOTAL cost in (0,1)
comp_i  <- A$quant_i                                  # contribution to reallocation (~composition)
df <- data.frame(vat = bundle$firms[A$em], nace4d = bundle$nace4d[A$em], ets = bundle$tau[A$em],
                 z = bundle$z[A$em], ebar = ebar_em, ratio_ebar_pz = ratio,
                 cost_share = cshare, comp_contrib = comp_i)
write.csv(df, file.path(output_dir, "diagnose_reallocation.csv"), row.names = FALSE)

# ---- concentration of the composition channel ----
tot <- sum(comp_i)
ord <- order(comp_i)                                  # most-reducing (most negative) first
cum <- cumsum(comp_i[ord]) / tot
cat("\n=== CONCENTRATION of the composition channel ===\n")
cat(sprintf("  total composition (sum quant_i) = %.4f over %d emitters\n", tot, length(comp_i)))
for (k in c(1,5,10,20,50)) if (k <= length(ord))
  cat(sprintf("  top-%2d firms (by reallocation) account for %5.1f%% of composition\n", k, 100*cum[k]))
n50 <- which(cum >= 0.50)[1]; n90 <- which(cum >= 0.90)[1]
cat(sprintf("  firms needed for 50%% of composition: %d ; for 90%%: %d\n", n50, n90))

# ---- composition by emission-cost-share bucket ----
cat("\n=== composition by emission COST SHARE bucket (carbon cost / total cost at p_z=80) ===\n")
brk <- c(-Inf, 0.05, 0.25, 0.50, 0.90, Inf); lab <- c("<5%","5-25%","25-50%","50-90%",">90%")
g <- cut(cshare, breaks = brk, labels = lab)
tab <- data.frame(bucket = lab,
                  n_firms = as.integer(table(g)[lab]),
                  emis_share = round(100 * tapply(df$z, g, sum)[lab] / sum(df$z), 1),
                  composition = round(tapply(comp_i, g, sum)[lab], 4),
                  pct_of_comp = round(100 * tapply(comp_i, g, sum)[lab] / tot, 1))
tab[is.na(tab)] <- 0; print(tab, row.names = FALSE)

# ---- cap counterfactual: composition when e_bar is winsorized at cost-share C ----
cat("\n=== cap counterfactual: composition when e_bar capped so cost share <= C ===\n")
cat(sprintf("  %-22s dlogZ=%.4f technique=%.4f composition=%.4f\n", "uncapped", A$dlogZ, A$technique, A$composition))
for (C in c(0.90, 0.50, 0.25)) {
  cap_ebar <- pmin(bundle$e_bar, (C/(1-C))/PZ)        # cost share C  <=>  ebar*pz = C/(1-C)
  bc <- bundle; bc$tau <- btau
  R <- attribute(bc, ebar = cap_ebar)
  cat(sprintf("  cap cost share <= %4.0f%%      dlogZ=%.4f technique=%.4f composition=%.4f\n",
              100*C, R$dlogZ, R$technique, R$composition))
}

cat(sprintf("\nWrote per-firm attribution to %s/diagnose_reallocation.csv\n", output_dir))
