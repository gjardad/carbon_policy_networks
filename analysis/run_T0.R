###############################################################################
# run_T0.R  —  counterfactual for the ACTUAL EU ETS targeting (benchmark).
# Sources cf_common.R (shared setup), runs scheme T0, writes cf_results_T0.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

res <- run_scheme("T0")
write.csv(res, file.path(out_data, "cf_results_T0.csv"), row.names = FALSE)
cat(sprintf("\nWrote cf_results_T0.csv to %s\n", out_data)); print(res)
