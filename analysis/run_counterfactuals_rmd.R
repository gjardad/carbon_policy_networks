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

SIGMA_GRID <- c(0.034, 0.5, 1, 4.7)
RHO_GRID   <- c(0.25, 0.5, 0.75, 1)
DEF_SIGMA  <- 0.5
DEF_RHO    <- 1

# Calibration target: Belgian within-cell emission change, 2005->2022 = -24pp.
# Framing A (default): endpoints 18 -> 80 EUR/tCO2. Robustness framing B below.
CAL_PLO <- 18; CAL_PHI <- 80; TARGET_DLOGZ <- log(0.76)
ROBUST_FRAMINGS <- list(            # (p_lo, p_hi) variants for the alpha robustness axis
  endpoints_18_80 = c(18, 80),
  nopolicy_term80 = c(0,  80),
  nopolicy_avg19  = c(0,  19)
)
PRICE_GRID <- c(80, 100, 150, 200, 250)     # counterfactual columns
PATH_STEP  <- 10   # EUR/tCO2 step for the path-integral decomposition grid (0 -> max price)
SCHEMES    <- c("T0")                        # add "T1" once industrial NACE confirmed
FULL_GRID  <- FALSE   # FALSE: calibrate only the default (sigma,rho) cell + price sweep
                      #        (fast first run). TRUE: full sigma/rho sweep + robustness.
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

# ---- timing probe: how long is one solve? (calibration does ~25-35 per cell) ----
cat("\nTiming one full_solve ...\n"); .t0 <- Sys.time()
.probe <- full_solve(80, DEF_SIGMA, DEF_RHO, 2, bundle, bshare)
cat(sprintf("  one full_solve = %.1f sec  (Z=%.4g) -> expect ~%.0f min per calibrated cell\n",
            as.numeric(difftime(Sys.time(), .t0, units = "secs")), .probe$Z,
            as.numeric(difftime(Sys.time(), .t0, units = "secs")) * 30 / 60))

# ---- 1. Calibrate alpha(sigma,rho): default cell (+ sweeps if FULL_GRID) ----
cells <- if (FULL_GRID) unique(rbind(
  data.frame(sigma = DEF_SIGMA, rho = DEF_RHO),
  data.frame(sigma = SIGMA_GRID, rho = DEF_RHO),
  data.frame(sigma = DEF_SIGMA, rho = RHO_GRID)
)) else data.frame(sigma = DEF_SIGMA, rho = DEF_RHO)
cat(sprintf("\nCalibrating alpha(sigma,rho) over %d cell(s)%s ...\n",
            nrow(cells), if (FULL_GRID) "" else " [FULL_GRID=FALSE: default cell only]"))
cal <- lapply(seq_len(nrow(cells)), function(i) {
  s <- cells$sigma[i]; r <- cells$rho[i]
  cat(sprintf("  cell %d/%d: sigma=%.3f rho=%.2f -> solving ...\n", i, nrow(cells), s, r))
  res <- calibrate_alpha(s, r, bundle, bshare, CAL_PLO, CAL_PHI, TARGET_DLOGZ)
  cat(sprintf("    alpha=%s bracketed=%s\n",
              ifelse(is.na(res$alpha), "NA", sprintf("%.3f", res$alpha)), res$bracketed))
  gc()    # release sparse-LU memory between cells (avoids SuiteSparse fault over many solves)
  data.frame(sigma = s, rho = r, alpha = res$alpha, bracketed = res$bracketed)
}) %>% bind_rows()
write.csv(cal, file.path(out_data, "cf_calibrated_alpha.csv"), row.names = FALSE)

# ---- 2. Robustness: alpha under alternative price framings (only if FULL_GRID) ----
if (FULL_GRID) {
  robust <- lapply(names(ROBUST_FRAMINGS), function(nm) {
    pr <- ROBUST_FRAMINGS[[nm]]
    res <- calibrate_alpha(DEF_SIGMA, DEF_RHO, bundle, bshare, pr[1], pr[2], TARGET_DLOGZ)
    gc()
    data.frame(framing = nm, p_lo = pr[1], p_hi = pr[2], alpha = res$alpha, bracketed = res$bracketed)
  }) %>% bind_rows()
  print(robust)
  write.csv(robust, file.path(out_data, "cf_alpha_robustness.csv"), row.names = FALSE)
}

# ---- 3. Counterfactual matrix: scheme x price, at each calibrated cell ----
# Technique/reallocation split via the BF path-integral decomposition (exact,
# residual-free; replaces the base-weight Laspeyres split). One fine grid of
# solves 0 -> max(price) gives every price column at once (cumulative along path).
cat("\nRunning counterfactual matrix (path-integral decomposition) ...\n")
path_grid <- sort(unique(c(seq(0, max(PRICE_GRID), by = PATH_STEP), PRICE_GRID)))
results <- list(); k <- 1
for (i in seq_len(nrow(cal))) {
  s <- cal$sigma[i]; r <- cal$rho[i]; a <- cal$alpha[i]
  if (is.na(a)) next
  for (sch in SCHEMES) {
    bsch <- bundle; bsch$tau <- get_tau(sch, bundle)
    dec <- decompose_path_grid(path_grid, s, r, a, bsch, bshare)   # cumulative at each node
    dec <- dec[dec$p_z %in% PRICE_GRID, ]                          # keep the requested columns
    results[[k]] <- data.frame(scheme = sch, p_z = dec$p_z, sigma = s, rho = r, alpha = a,
                               dlogZ = dec$dlogZ, technique = dec$technique,
                               reallocation = dec$reallocation, mean_price = dec$mean_price)
    k <- k + 1
  }
  gc()    # release sparse-LU memory between calibrated cells
}
results <- bind_rows(results)
write.csv(results, file.path(out_data, "cf_results.csv"), row.names = FALSE)
cat(sprintf("\nDone. Wrote: cf_calibrated_alpha.csv, cf_alpha_robustness.csv, cf_results.csv to %s\n", out_data))
print(utils::head(results, 12))
