###############################################################################
# diagnose_sector_thresholds.R
#
# PURPOSE:
#   Shows how many firms/obs are dropped by requiring at least N firms per sector
#   (to identify sector fixed effects in LOFO).
#
#   Compares:
#     - sector FE = nace5d vs nace2d
#     - thresholds = 2 vs 3
#
# OUTPUT:
#   Writes CSV to: /output/loocv/results/sector_threshold_diagnostics.csv
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))

tic("diagnose_sector_thresholds")

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))

df <- amount_spent_on_fuel_by_firm_year %>%
  filter(emissions > 0, !is.na(amount_spent_on_fuel)) %>%
  mutate(
    nace5d = as.character(nace5d),
    nace2d = substr(gsub("\\D", "", nace5d), 1, 2)
  )

sector_report <- function(sector, min_firms) {
  counts <- df %>%
    group_by(.data[[sector]]) %>%
    summarise(n_firms = n_distinct(vat_j_ano), .groups = "drop")
  
  valid <- counts %>% filter(n_firms >= min_firms) %>% pull(.data[[sector]])
  kept <- df %>% filter(.data[[sector]] %in% valid)
  
  tibble(
    sector = sector,
    min_firms = min_firms,
    n_firms_total = n_distinct(df$vat_j_ano),
    n_firms_kept  = n_distinct(kept$vat_j_ano),
    n_firms_dropped = n_firms_total - n_firms_kept,
    share_firms_dropped = n_firms_dropped / n_firms_total,
    n_obs_total = nrow(df),
    n_obs_kept  = nrow(kept),
    n_obs_dropped = n_obs_total - n_obs_kept,
    share_obs_dropped = n_obs_dropped / n_obs_total
  )
}

report <- bind_rows(
  sector_report("nace5d", 2),
  sector_report("nace5d", 3),
  sector_report("nace2d", 2),
  sector_report("nace2d", 3)
) %>% arrange(sector, min_firms)

print(report)
write_csv(report, file.path(RESULTS_DIR, "sector_threshold_diagnostics.csv"))
log_step("Wrote sector_threshold_diagnostics.csv")

toc()
