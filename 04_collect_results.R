###############################################################################
# 04_collect_results.R
#
# PURPOSE:
#   Loads /output/loocv/results/model_comparison_by_proxy.rds and produces
#   leaderboards for three models across four metrics:
#     - sector5d: year FE + nace5d FE + log(fuel_proxy)
#     - sector2d: year FE + nace2d FE + log(fuel_proxy)
#     - nosector: year FE only      + log(fuel_proxy)
#
#   Metrics:
#     - nRMSE (lower is better)
#     - MAPD  (lower is better)
#     - R2_LOO (higher is better)
#     - Rho_Spearman (higher is better)
#
# OUTPUT:
#   Saves:
#     - /output/loocv/results/leaderboards_sector5d_sector2d_nosector.rds
#     - /output/loocv/results/leaderboards_sector5d_sector2d_nosector.csv
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))

library(dplyr)
library(readr)
library(tidyr)

res <- readRDS(file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))

topk <- function(df, metric_col, k = 20, decreasing = FALSE) {
  if (decreasing) df %>% arrange(desc(.data[[metric_col]])) %>% slice(1:k)
  else df %>% arrange(.data[[metric_col]]) %>% slice(1:k)
}

make_leaderboards <- function(res, prefix, k = 20) {
  tibble(
    model = prefix,
    metric = c("nRMSE","MAPD","R2_LOO","Rho_Spearman"),
    top = list(
      topk(res, paste0(prefix, "_nRMSE"), k = k, decreasing = FALSE),
      topk(res, paste0(prefix, "_MAPD"),  k = k, decreasing = FALSE),
      topk(res, paste0(prefix, "_R2_LOO"),k = k, decreasing = TRUE),
      topk(res, paste0(prefix, "_Rho_Spearman"), k = k, decreasing = TRUE)
    )
  )
}

lbs <- bind_rows(
  make_leaderboards(res, "sector5d", k = 20),
  make_leaderboards(res, "sector2d", k = 20),
  make_leaderboards(res, "nosector", k = 20)
)

# Save nested version
saveRDS(lbs, file.path(RESULTS_DIR, "leaderboards_sector5d_sector2d_nosector.rds"))

# Save flattened CSV (one row per proxy per leaderboard)
lbs_flat <- lbs %>%
  unnest(top) %>%
  select(
    model, metric,
    name,
    use_siec_all, supplier_non_euets, buyer_sector_siec, emissions_weighted, supplier_nace_filter,
    ends_with("_nRMSE"), ends_with("_MAPD"), ends_with("_R2_LOO"), ends_with("_Rho_Spearman"), ends_with("_n_obs")
  )

write_csv(lbs_flat, file.path(RESULTS_DIR, "leaderboards_sector5d_sector2d_nosector.csv"))

lbs
