###############################################################################
# run_counterfactuals_rmd.R  —  end-to-end runner (assembly -> solve -> calibrate
#                               -> counterfactuals), meant to run on RMD where the
#                               FULL B2B network is available.
#
# Chains: assemble bundle (full B2B) -> source phase5 solver -> calibrate
#   alpha(sigma,rho) to the Belgian within-cell change -> price x targeting matrix.
#
# Self-contained except for phase5_model_solver.R (sourced for the solver fns).
# Paths auto-resolve by machine (utils/paths.R) -- no editing needed; just Rscript it.
###############################################################################

suppressMessages({ library(dplyr); library(stringr); library(Matrix) })

# ---- Paths (auto-detect machine via utils/paths.R: jardang=RMD, jota_=local) ----
.path_candidates <- c(
  "C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
  "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
.p <- .path_candidates[file.exists(.path_candidates)]
if (length(.p) == 0L) stop("Cannot locate utils/paths.R; add a candidate.")
source(.p[1])                   # sets project_root, proc_data, out_data, ...
CODE_DIR <- file.path(project_root, "analysis")
rm(.path_candidates, .p)

# ================================ CONFIG ===================================
YEAR  <- 2019
SCOPE <- "ets_neighbors"        # "ets_neighbors" (tractable, clean) | "full" (whole panel)

# Parameters are set EXTERNALLY (literature-disciplined), NOT calibrated to the
# Belgian aggregate emission change. The benchmark is simply the model at the
# realized price under these parameters -- internally consistent (we don't force
# the model to reproduce one Belgian moment while contradicting others).
#   alpha (abatement elasticity): Martinsson (2024) emission-price elasticity
#         |d log e / d log p| = alpha/(alpha-1).  all-firm ~2.08 -> alpha~1.9;
#         top-decile ~1.3 -> alpha~4.3. Our regulated firms are top emitters.
#   rho (pass-through): Amiti-Itskhoki-Konings own-cost ~0.5-0.7.
#   sigma (substitution): unidentified in Belgium -> SWEPT (the headline).
SIGMA_GRID <- c(0.034, 0.5, 1, 4.7)   # swept
RHO_GRID   <- c(0.5, 0.75, 1)         # AIK realistic interior + full-passthrough robustness
ALPHA_GRID <- c(2, 4)                 # Martinsson all-firm (~2) and top-emitter (~4)
DEF_SIGMA  <- 0.5
DEF_RHO    <- 0.5                      # AIK realistic
DEF_ALPHA  <- 4                       # top-emitter (our regulated firms)

PRICE_GRID <- c(80, 100, 150, 200, 250)   # benchmark = 80 (realized 2022); rest counterfactual
PATH_STEP  <- 10   # EUR/tCO2 step for the path-integral decomposition grid (0 -> max price)
SCHEMES    <- c("T0", "T1", "T2")     # T0 = actual ETS; T1 = universal industrial; T2 = centrality
                                      # (T2 needs cf_centrality.csv from phase6_centrality.R)
FULL_GRID  <- FALSE  # FALSE: sweep sigma at (DEF_RHO, DEF_ALPHA). TRUE: also sweep rho and alpha.
# ===========================================================================

# Assembly (assemble_bundle, get_tau) is shared with profile_solver.R.
source(file.path(CODE_DIR, "model_assembly.R"))

# ================================== RUN ====================================
cat(sprintf("Assembling bundle: year=%d scope=%s ...\n", YEAR, SCOPE))
bundle <- assemble_bundle(YEAR, SCOPE, proc_data, out_data)
cat(sprintf("  %d firms, %d ETS, max rowsum=%.4f\n", bundle$meta$n, bundle$meta$n_ets, bundle$meta$max_rowsum))

source(file.path(CODE_DIR, "phase5_model_solver.R"))        # solver + calibrate_alpha (fns only)
bshare <- base_final_shares(bundle)

# (technique/reallocation come from decompose_path_grid in phase5 — the BF
#  path-integral decomposition; the old base-weight decomp() is retired.)

# ---- timing probe: how long is one solve? ----
cat("\nTiming one full_solve ...\n"); .t0 <- Sys.time()
.probe <- full_solve(80, DEF_SIGMA, DEF_RHO, DEF_ALPHA, bundle, bshare)
cat(sprintf("  one full_solve = %.1f sec  (Z=%.4g)\n",
            as.numeric(difftime(Sys.time(), .t0, units = "secs")), .probe$Z))

# ---- Parameter cells: sweep sigma; alpha and rho set externally (Option B) ----
cells <- data.frame(sigma = SIGMA_GRID, rho = DEF_RHO, alpha = DEF_ALPHA)     # headline: sweep sigma
if (FULL_GRID) cells <- unique(rbind(cells,
  data.frame(sigma = DEF_SIGMA, rho = RHO_GRID, alpha = DEF_ALPHA),           # rho robustness
  data.frame(sigma = DEF_SIGMA, rho = DEF_RHO,  alpha = ALPHA_GRID)))         # alpha robustness

# ---- Targeting vectors for each scheme (T2 reads the centrality ranking) ----
# T2 = target the most-central firms (by total emission reduction) holding the
# emission coverage equal to the actual ETS set. Needs cf_centrality.csv from
# phase6_centrality.R; if missing, T2 is skipped with a warning.
scheme_tau <- list()
for (sch in SCHEMES) {
  if (sch == "T2") {
    f <- file.path(out_data, "cf_centrality.csv")
    if (!file.exists(f)) { warning("cf_centrality.csv not found - run phase6_centrality.R first; skipping T2"); next }
    cf <- read.csv(f); ord <- order(cf$total)                       # most-reducing first
    nsel <- which(cumsum(cf$z[ord]) >= sum(cf$z[cf$ets == 1]))[1]
    if (is.na(nsel)) nsel <- length(ord)
    scheme_tau[["T2"]] <- as.integer(bundle$firms %in% cf$vat[ord][seq_len(nsel)])
    cat(sprintf("  T2: %d central firms (emission coverage matched to ETS)\n", nsel))
  } else scheme_tau[[sch]] <- get_tau(sch, bundle)
}
SCHEMES <- names(scheme_tau)   # drop any skipped scheme

# ---- Counterfactual matrix: (cell x scheme x price), BF path-integral decomposition ----
# d log Z = scale (gross output) + technique (abatement) + composition (reallocation),
# integrated along p_z: 0 -> target. Benchmark = the p_z=80 row (realized 2022); the
# rest are counterfactuals. One fine grid of solves gives all price columns at once.
cat(sprintf("\nRunning %d parameter cell(s) x %d scheme(s) [path-integral decomposition] ...\n",
            nrow(cells), length(SCHEMES)))
path_grid <- sort(unique(c(seq(0, max(PRICE_GRID), by = PATH_STEP), PRICE_GRID)))
results <- list(); k <- 1
for (i in seq_len(nrow(cells))) {
  s <- cells$sigma[i]; r <- cells$rho[i]; a <- cells$alpha[i]
  cat(sprintf("  cell %d/%d: sigma=%.3f rho=%.2f alpha=%.2f ...\n", i, nrow(cells), s, r, a))
  for (sch in SCHEMES) {
    bsch <- bundle; bsch$tau <- scheme_tau[[sch]]
    dec <- decompose_path_grid(path_grid, s, r, a, bsch, bshare)   # cumulative at each node
    dec <- dec[dec$p_z %in% PRICE_GRID, ]                          # keep the requested columns
    results[[k]] <- data.frame(scheme = sch, p_z = dec$p_z, sigma = s, rho = r, alpha = a,
                               dlogZ = dec$dlogZ, scale = dec$scale, technique = dec$technique,
                               composition = dec$composition, realGDP = dec$realGDP,
                               mean_price = dec$mean_price)
    k <- k + 1
  }
  gc()    # release sparse-LU memory between cells
}
results <- bind_rows(results)
write.csv(results, file.path(out_data, "cf_results.csv"), row.names = FALSE)
cat(sprintf("\nDone. Wrote cf_results.csv to %s\n", out_data))
print(results)
