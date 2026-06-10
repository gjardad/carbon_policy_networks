###############################################################################
# run_counterfactuals_rmd.R  —  run ALL targeting schemes and write
#   a single combined cf_results.csv. Meant for RMD (full B2B network).
#
# This is now a thin wrapper over cf_common.R (shared setup + run_scheme). To run
# ONE scheme in isolation use run_actual_ets.R / run_universal_industrial.R /
# run_centrality.R instead -- they share the same cached bundle, so no re-assembly.
# Edit CONFIG in cf_common.R.
#
#   actual_ets = realized EU ETS;  universal_industrial = all industrial emitters;
#   centrality = most-central firms (needs cf_centrality.csv from phase6_centrality.R;
#   skipped with a warning if absent)
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

SCHEMES <- c("actual_ets", "universal_industrial", "centrality")
res <- bind_rows(lapply(SCHEMES, function(sch) tryCatch(run_scheme(sch), error = function(e) {
  warning(sprintf("scheme %s skipped: %s", sch, conditionMessage(e))); NULL
})))

write.csv(res, file.path(output_dir, "cf_results.csv"), row.names = FALSE)
cat(sprintf("\nDone. Wrote cf_results.csv (%d rows, schemes: %s) to %s\n",
            nrow(res), paste(unique(res$scheme), collapse = ", "), output_dir))
print(res)
