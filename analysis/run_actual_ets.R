###############################################################################
# run_actual_ets.R  —  counterfactual for the ACTUAL EU ETS targeting (benchmark).
# Sources cf_common.R (shared setup); writes cf_results_actual_ets.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

res <- run_scheme("actual_ets")
write.csv(res, file.path(out_data, "cf_results_actual_ets.csv"), row.names = FALSE)
cat(sprintf("\nWrote cf_results_actual_ets.csv to %s\n", out_data)); print(res)
