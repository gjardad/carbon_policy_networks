###############################################################################
# run_one_sigma.R  —  recompute the counterfactuals for a SINGLE sigma_W cell and
#                     splice the result into cf_results.csv (sigma_B / rho / alpha
#                     held at the baseline), instead of re-running everything.
#                     Use when you change one within-sector elasticity anchor.
#
# Replaces the rows for SIG_W (if present) and keeps every other sigma_W row, so
# the file stays consistent across robustness sweeps. Runs all three schemes
# (~3 path-decompositions). Needs the cached bundle and cf_centrality.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])      # cached bundle, solver, get_scheme_tau, path_grid, output_dir

SIG_W <- 2.5                          # the within-sector elasticity to (re)compute
SCHEMES <- c("actual_ets", "universal_industrial", "centrality")
cat(sprintf("Recomputing sigma_W=%.3g for %d schemes (sigma_B=%.2f, rho=%.2f, alpha=%.0f) ...\n",
            SIG_W, length(SCHEMES), SIGMA_B, DEF_RHO, DEF_ALPHA))

new <- bind_rows(lapply(SCHEMES, function(sch) {
  cat(sprintf("  %s ...\n", sch)); gc()
  bsch <- bundle; bsch$tau <- get_scheme_tau(sch)
  dec <- decompose_path_grid(path_grid, SIGMA_B, SIG_W, DEF_RHO, DEF_ALPHA, bsch, bshare)
  dec <- dec[dec$p_z %in% PRICE_GRID, ]
  data.frame(scheme = sch, p_z = dec$p_z, sigma_B = SIGMA_B, sigma_W = SIG_W,
             rho = DEF_RHO, alpha = DEF_ALPHA, dlogZ = dec$dlogZ, scale = dec$scale,
             technique = dec$technique, composition = dec$composition,
             realGDP = dec$realGDP, mean_price = dec$mean_price)
}))

f <- file.path(output_dir, "cf_results.csv")
if (file.exists(f)) {
  old <- read.csv(f)
  res <- rbind(old[abs(old$sigma_W - SIG_W) > 1e-9, ], new)   # drop old SIG_W rows, splice new
} else res <- new
res <- res[order(res$scheme, res$sigma_W, res$p_z), ]
write.csv(res, f, row.names = FALSE)
cat(sprintf("\nWrote cf_results.csv (sigma_W now: %s) to %s\n",
            paste(sort(unique(res$sigma_W)), collapse = ", "), output_dir))
print(res[res$p_z == 80, c("scheme", "sigma_W", "dlogZ", "technique", "composition")], row.names = FALSE)
