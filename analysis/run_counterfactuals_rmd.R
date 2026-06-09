###############################################################################
# run_counterfactuals_rmd.R  —  run ALL targeting schemes (T0, T1, T2) and write
#   a single combined cf_results.csv. Meant for RMD (full B2B network).
#
# This is now a thin wrapper over cf_common.R (shared setup + run_scheme). To run
# ONE scheme in isolation use run_T0.R / run_T1.R / run_T2.R instead -- they share
# the same cached bundle, so no re-assembly. Edit CONFIG in cf_common.R.
#
#   T0 = actual EU ETS;  T1 = universal industrial;  T2 = centrality
#   (T2 needs cf_centrality.csv from phase6_centrality.R; skipped with a warning if absent)
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

SCHEMES <- c("T0", "T1", "T2")
res <- bind_rows(lapply(SCHEMES, function(sch) tryCatch(run_scheme(sch), error = function(e) {
  warning(sprintf("scheme %s skipped: %s", sch, conditionMessage(e))); NULL
})))

write.csv(res, file.path(out_data, "cf_results.csv"), row.names = FALSE)
cat(sprintf("\nDone. Wrote cf_results.csv (%d rows, schemes: %s) to %s\n",
            nrow(res), paste(unique(res$scheme), collapse = ", "), out_data))
print(res)
