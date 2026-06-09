###############################################################################
# run_T1.R  —  counterfactual for UNIVERSAL INDUSTRIAL targeting
#              (all firms in NACE 05-09, 10-33, 35).
# Sources cf_common.R (shared setup), runs scheme T1, writes cf_results_T1.csv.
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])

res <- run_scheme("T1")
write.csv(res, file.path(out_data, "cf_results_T1.csv"), row.names = FALSE)
cat(sprintf("\nWrote cf_results_T1.csv to %s\n", out_data)); print(res)
