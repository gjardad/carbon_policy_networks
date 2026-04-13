###############################################################################
# 00_config.R
#
# PURPOSE:
#   Global configuration for the LOFO/LOOCV work stream.
#   - Defines your project folder structure exactly as you use it
#   - Creates output folders under /output/loocv (cache + results)
#   - Defines code folder under /code/loocv (functions live there)
#   - Loads required packages
#   - Defines log file path
#
# EXPECTED LOCATION:
#   This file should live at: /code/loocv/00_config.R
#
# USAGE:
#   Source this at the top of every script and function file.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(fixest)
})

# ---- Directory setup  ----
if (tolower(Sys.info()[["user"]]) == "jardang") {
  folder <- "X:/Documents/JARDANG"
} else {
  stop("User not recognized in 00_config.R. Please add your folder path.")
}

raw_data  <- paste0(folder, "/carbon_policy_networks/data/raw")
int_data  <- paste0(folder, "/carbon_policy_networks/data/intermediate")
proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")
output    <- paste0(folder, "/carbon_policy_networks/output")
code      <- paste0(folder, "/carbon_policy_networks/code")

# ---- LOOCV workstream roots ----
LOOCV_CODE <- file.path(code, "loocv")
LOOCV_OUT  <- file.path(output, "loocv")

# ---- Output subfolders (within /output/loocv) ----
CACHE_DIR   <- file.path(LOOCV_OUT, "cache")
RESULTS_DIR <- file.path(LOOCV_OUT, "results")

dir.create(LOOCV_OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Code subfolder for functions (within /code/loocv) ----
FUN_DIR <- file.path(LOOCV_CODE, "functions")
dir.create(FUN_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Logging ----
LOG_FILE <- file.path(
  RESULTS_DIR,
  paste0("logs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
)
