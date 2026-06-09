###############################################################################
# run_universal_industrial.R  —  counterfactual for UNIVERSAL INDUSTRIAL targeting
#   (every industrial emitter: NACE 05-09 mining, 10-33 manufacturing, 35 energy).
# Sources cf_common.R (shared setup); writes cf_results_universal_industrial.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

res <- run_scheme("universal_industrial")
write.csv(res, file.path(out_data, "cf_results_universal_industrial.csv"), row.names = FALSE)
cat(sprintf("\nWrote cf_results_universal_industrial.csv to %s\n", out_data)); print(res)
