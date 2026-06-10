###############################################################################
# run_one_sigma.R  —  recompute the counterfactuals for a SINGLE sigma and splice
#                     the result into cf_results.csv, instead of re-running the
#                     whole grid. Use when you change one sigma anchor.
#
# Keeps the existing rows for every other sigma in the CURRENT grid and drops any
# row whose sigma is no longer in SIGMA_GRID (e.g. a retired anchor), so the file
# stays consistent. Runs all three schemes at SIG (~3 path-decompositions).
# Needs the cached bundle and cf_centrality.csv (for the centrality scheme).
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])      # cached bundle, solver, get_scheme_tau, path_grid, output_dir

SIG <- 0.1                            # the sigma to (re)compute
SCHEMES <- c("actual_ets", "universal_industrial", "centrality")
cat(sprintf("Recomputing sigma=%.3g for %d schemes (rho=%.2f, alpha=%.0f) ...\n",
            SIG, length(SCHEMES), DEF_RHO, DEF_ALPHA))

new <- bind_rows(lapply(SCHEMES, function(sch) {
  cat(sprintf("  %s ...\n", sch)); gc()
  bsch <- bundle; bsch$tau <- get_scheme_tau(sch)
  dec <- decompose_path_grid(path_grid, SIG, DEF_RHO, DEF_ALPHA, bsch, bshare)
  dec <- dec[dec$p_z %in% PRICE_GRID, ]
  data.frame(scheme = sch, p_z = dec$p_z, sigma = SIG, rho = DEF_RHO, alpha = DEF_ALPHA,
             dlogZ = dec$dlogZ, scale = dec$scale, technique = dec$technique,
             composition = dec$composition, realGDP = dec$realGDP, mean_price = dec$mean_price)
}))

f <- file.path(output_dir, "cf_results.csv")
if (file.exists(f)) {
  old <- read.csv(f)
  keep <- old$sigma %in% SIGMA_GRID & abs(old$sigma - SIG) > 1e-9   # current grid, minus SIG
  dropped <- setdiff(unique(old$sigma), unique(old$sigma[old$sigma %in% SIGMA_GRID]))
  if (length(dropped)) cat(sprintf("Dropping retired sigma rows: %s\n", paste(dropped, collapse = ", ")))
  res <- rbind(old[keep, ], new)
} else res <- new
res <- res[order(res$scheme, res$sigma, res$p_z), ]
write.csv(res, f, row.names = FALSE)
cat(sprintf("\nWrote cf_results.csv (sigmas now: %s) to %s\n",
            paste(sort(unique(res$sigma)), collapse = ", "), output_dir))
print(res[res$p_z == 80, c("scheme", "sigma", "dlogZ", "technique", "composition")], row.names = FALSE)
