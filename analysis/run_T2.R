###############################################################################
# run_T2.R  —  counterfactual for CENTRALITY targeting (most-central firms,
#              emission coverage matched to the actual ETS set).
# Requires cf_centrality.csv from phase6_centrality.R (run that first).
# Sources cf_common.R (shared setup), runs scheme T2, writes cf_results_T2.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

res <- run_scheme("T2")
write.csv(res, file.path(out_data, "cf_results_T2.csv"), row.names = FALSE)
cat(sprintf("\nWrote cf_results_T2.csv to %s\n", out_data)); print(res)
