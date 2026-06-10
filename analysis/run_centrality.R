###############################################################################
# run_centrality.R  —  counterfactual for CENTRALITY targeting (most-central firms,
#   emission coverage matched to the actual ETS set).
# Requires cf_centrality.csv from phase6_centrality.R (run that first).
# Sources cf_common.R (shared setup); writes cf_results_centrality.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

res <- run_scheme("centrality")
write.csv(res, file.path(output_dir, "cf_results_centrality.csv"), row.names = FALSE)
cat(sprintf("\nWrote cf_results_centrality.csv to %s\n", output_dir)); print(res)
