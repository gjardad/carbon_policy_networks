###############################################################################
# execution.R
#
# PURPOSE:
#   One-click execution script for the LOFO/LOOCV workstream.
#   Runs the full pipeline in order:
#     0) load configuration + utilities
#     1) build auxiliary tables (cache)
#     2) build all fuel proxy variants (cache)
#     3) run Model A + Model B evaluations across all proxies
#     4) collect and display top proxies by metric
#
# EXPECTED LOCATION:
#   X:/Documents/JARDANG/carbon_policy_networks/code/loocv/execution.R
#
# OUTPUT:
#   Writes outputs to:
#     X:/Documents/JARDANG/carbon_policy_networks/output/loocv/
###############################################################################

# --- Explicitly set working directory to code/loocv ---
if (tolower(Sys.info()[["user"]]) == "jardang") {
  setwd("X:/Documents/JARDANG/carbon_policy_networks/code/loocv")
} else {
  stop("User not recognized in execution.R – please set working directory manually.")
}

# --- Always run from a clean session ---
rm(list = ls())

# --- Source config (sets paths + creates folders + logging destination) ---
source("00_config.R")

# --- Source utilities (logging + timing helpers) ---
source(file.path(FUN_DIR, "progress_utils.R"))

tic("FULL LOOCV PIPELINE")

# 1) Build aux tables (cache)
tic("STEP 1/4: Build auxiliary tables")
source("01_build_aux_tables.R")
toc()

# 2) Build proxy variants (cache)
tic("STEP 2/4: Build all proxy variants")
source("02_build_proxies.R")
toc()

# 3) Run LOFO evaluation across proxies (long step)
tic("STEP 3/4: Run models across proxies")
source("03_run_lofo_across_proxies.R")
toc()

# 4) Collect results
tic("STEP 4/4: Collect leaderboards")
leaderboards <- source("04_collect_results.R")$value

# Print quick summaries (optional)
log_step("=== Leaderboards computed. Quick preview: ModelA top_nRMSE ===")
print(leaderboards$ModelA$top_nRMSE)

log_step("=== Quick preview: ModelB_LOFO top_nRMSE ===")
print(leaderboards$ModelB_LOFO$top_nRMSE)

if (!is.null(leaderboards$ModelB_LOSO)) {
  log_step("=== Quick preview: ModelB_LOSO top_nRMSE ===")
  print(leaderboards$ModelB_LOSO$top_nRMSE)
}

# Save the leaderboards object for convenience
saveRDS(leaderboards, file.path(RESULTS_DIR, "leaderboards_by_metric.rds"))
log_step("Saved leaderboards_by_metric.rds")

toc()

toc()  # FULL PIPELINE timer
