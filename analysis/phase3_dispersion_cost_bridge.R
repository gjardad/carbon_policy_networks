###############################################################################
# analysis/phase3_dispersion_cost_bridge.R
#
# PURPOSE
#   Two cost-related results (require the network build for total_cost):
#
#   (1) COST-BASED direct emission intensity dispersion e^cost_i = z_i/cost_i
#       at NACE 4d + 2d, both scenarios. Pair with phase3_dispersion_network's
#       nu dispersion (also cost-based) for the internally-consistent network-
#       vs-direct comparison.
#
#   (2) BRIDGE statistic: firm-year correlation cor(log e^cost, log e^rev)
#       — pooled and within-sector — to quantify how much the cost-vs-revenue
#       denominator choice distorts the direct measure.
#
# INPUT  {PROJ}/data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
#          (firms: includes scope1, total_cost, revenue, e_cost, nace4d/2d)
# OUTPUT {PROJ}/data/processed/dispersion_direct_cost_bridge_{s1,s2}.RData
#          (ecost_stats4d, ecost_stats2d, bridge_pooled, bridge_4d, bridge_2d)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

if (Sys.info()[["user"]] == "JARDANG") {
  project_root <- "X:/Documents/JARDANG/carbon_policy_networks"
} else {
  project_root <- "c:/Users/jota_/Documents/carbon_policy_networks"
}
proc <- file.path(project_root, "data", "processed")
source(file.path(project_root, "utils", "sector_conventions.R"))

cat("== phase3_dispersion_cost_bridge ==\n")

load_build <- function(scn) {
  d <- file.path(proc, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
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

bridge_within <- function(firms, group_col) {
  firms %>%
    mutate(grp = .data[[group_col]],
           le_cost = log(e_cost), le_rev = log(e_rev)) %>%
    filter(!is.na(grp), is.finite(le_cost), is.finite(le_rev)) %>%
    group_by(grp, year) %>%
    summarise(n_firms     = n(),
              spearman    = if (n() >= 2L) suppressWarnings(cor(le_cost, le_rev, method = "spearman")) else NA_real_,
              pearson     = if (n() >= 2L) suppressWarnings(cor(le_cost, le_rev, method = "pearson")) else NA_real_,
              .groups     = "drop") %>%
    filter(n_firms >= MIN_N_CORR) %>%
    rename(!!group_col := grp)
}

for (scn in c("s1", "s2")) {
  firms <- load_build(scn)
  firms <- firms %>%
    filter(!is.na(revenue), revenue > 0, total_cost > 0, scope1 > 0) %>%
    mutate(e_rev = scope1 / revenue)
  cat(sprintf("\n[%s] firms with z>0 & rev>0 & cost>0: %d\n",
              toupper(scn), nrow(firms)))

  # (1) cost-based direct EI dispersion
  ecost_stats4d <- compute_disp(firms, "e_cost", "nace4d")
  ecost_stats2d <- compute_disp(firms, "e_cost", "nace2d")

  # (2) bridge correlation cor(log e^cost, log e^rev)
  bridge_pooled <- with(firms,
    data.frame(n_firms = nrow(firms),
               spearman = suppressWarnings(cor(log(e_cost), log(e_rev), method = "spearman")),
               pearson  = suppressWarnings(cor(log(e_cost), log(e_rev), method = "pearson"))))
  bridge_4d <- bridge_within(firms, "nace4d")
  bridge_2d <- bridge_within(firms, "nace2d")

  save(ecost_stats4d, ecost_stats2d, bridge_pooled, bridge_4d, bridge_2d,
       file = file.path(proc, sprintf("dispersion_direct_cost_bridge_%s.RData", scn)))

  cat(sprintf("   e^cost 4d: %d sec-yrs | mean 90-10 log = %.2f (ratio %.1fx)\n",
              nrow(ecost_stats4d), mean(ecost_stats4d$p9010_log, na.rm = TRUE),
              exp(mean(ecost_stats4d$p9010_log, na.rm = TRUE))))
  cat(sprintf("   e^cost 2d: %d sec-yrs | mean 90-10 log = %.2f (ratio %.1fx)\n",
              nrow(ecost_stats2d), mean(ecost_stats2d$p9010_log, na.rm = TRUE),
              exp(mean(ecost_stats2d$p9010_log, na.rm = TRUE))))
  cat(sprintf("   bridge pooled: n=%d | Spearman=%.3f | Pearson(log)=%.3f\n",
              bridge_pooled$n_firms, bridge_pooled$spearman, bridge_pooled$pearson))
  cat(sprintf("   bridge 4d (mean across sec-yrs, %d cells): Spearman=%.3f, Pearson=%.3f\n",
              nrow(bridge_4d), mean(bridge_4d$spearman, na.rm = TRUE),
              mean(bridge_4d$pearson, na.rm = TRUE)))
  cat(sprintf("   bridge 2d (mean across sec-yrs, %d cells): Spearman=%.3f, Pearson=%.3f\n",
              nrow(bridge_2d), mean(bridge_2d$spearman, na.rm = TRUE),
              mean(bridge_2d$pearson, na.rm = TRUE)))
}

cat("\nDone.\n")
