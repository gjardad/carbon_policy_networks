###############################################################################
# analysis/phase3_dispersion_network.R
#
# PURPOSE
#   Within-sector dispersion of the COST-BASED network-adjusted emission
#   intensity nu_i = (Psi * e^cost)_i, at NACE 4-digit (headline) and 2-digit
#   (literature comparability), under both scenarios (S1 zero / S2 GLO).
#
#   For comparison, also computes dispersion of the COST-BASED direct EI
#   e^cost_i = z_i/total_cost_i on the same firm universe — these two are the
#   first pairwise-comparable statistic (nu vs e^cost, both per EUR of cost).
#
# INPUT  {PROJ}/data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUT {PROJ}/data/processed/dispersion_network_{s1,s2}.RData
#          (nu_stats4d, nu_stats2d, ecost_stats4d, ecost_stats2d per scenario)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

if (tolower(Sys.info()[["user"]]) == "jardang") {
  project_root <- "X:/Documents/JARDANG/carbon_policy_networks"
} else {
  project_root <- "c:/Users/jota_/Documents/carbon_policy_networks"
}
proc <- file.path(project_root, "data", "processed")
source(file.path(project_root, "utils", "sector_conventions.R"))

cat("== phase3_dispersion_network (cost-based nu and e^cost) ==\n")

# Load and stack the per-year build output for a scenario.
load_build <- function(scn) {
  d <- file.path(proc, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  if (length(fs) == 0L) stop("no build output in ", d)
  L <- list()
  for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L)
}

compute_disp <- function(firms, value_col, group_col) {
  firms %>%
    mutate(grp = .data[[group_col]], val = .data[[value_col]]) %>%
    filter(!is.na(grp), is.finite(val), val > 0) %>%
    group_by(grp, year) %>%
    group_modify(~ disp_stats(.x$val)) %>%
    ungroup() %>%
    filter(n_firms >= MIN_N_STATS) %>%
    rename(!!group_col := grp)
}

for (scn in c("s1", "s2")) {
  firms <- load_build(scn)
  cat(sprintf("\n[%s] build firm-years loaded: %d (yrs %d-%d)\n",
              toupper(scn), nrow(firms), min(firms$year), max(firms$year)))

  nu_stats4d    <- compute_disp(firms, "nu",     "nace4d")
  nu_stats2d    <- compute_disp(firms, "nu",     "nace2d")
  ecost_stats4d <- compute_disp(firms, "e_cost", "nace4d")
  ecost_stats2d <- compute_disp(firms, "e_cost", "nace2d")

  save(nu_stats4d, nu_stats2d, ecost_stats4d, ecost_stats2d,
       file = file.path(proc, sprintf("dispersion_network_%s.RData", scn)))

  cat(sprintf("   nu     4d: %d sec-yrs | mean 90-10 log = %.2f (ratio %.1fx)\n",
              nrow(nu_stats4d), mean(nu_stats4d$p9010_log, na.rm = TRUE),
              exp(mean(nu_stats4d$p9010_log, na.rm = TRUE))))
  cat(sprintf("   nu     2d: %d sec-yrs | mean 90-10 log = %.2f (ratio %.1fx)\n",
              nrow(nu_stats2d), mean(nu_stats2d$p9010_log, na.rm = TRUE),
              exp(mean(nu_stats2d$p9010_log, na.rm = TRUE))))
  cat(sprintf("   e^cost 4d: %d sec-yrs | mean 90-10 log = %.2f (ratio %.1fx)\n",
              nrow(ecost_stats4d), mean(ecost_stats4d$p9010_log, na.rm = TRUE),
              exp(mean(ecost_stats4d$p9010_log, na.rm = TRUE))))
  cat(sprintf("   e^cost 2d: %d sec-yrs | mean 90-10 log = %.2f (ratio %.1fx)\n",
              nrow(ecost_stats2d), mean(ecost_stats2d$p9010_log, na.rm = TRUE),
              exp(mean(ecost_stats2d$p9010_log, na.rm = TRUE))))
}

cat("\nDone.\n")
