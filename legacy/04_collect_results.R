###############################################################################
# 04_collect_results.R
#
# PURPOSE:
#   Builds leaderboards for ALL models in model_comparison_by_proxy.rds.
#   Auto-detects model prefixes from columns ending with "__nRMSE".
#
# OUTPUT:
#   - leaderboards_all_models.rds / .csv
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

res <- readRDS(file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))

# Detect model prefixes from metric columns
metric_suffixes <- c("nRMSE","MAPD","R2_LOO","Rho_Spearman","n_obs")
metric_cols <- unlist(lapply(metric_suffixes, function(s) paste0("__", s)))

model_prefixes <- names(res) %>%
  .[str_detect(., "__nRMSE$")] %>%
  str_replace("__nRMSE$", "")

stopifnot(length(model_prefixes) > 0)

topk <- function(df, col, k = 20, decreasing = FALSE) {
  if (decreasing) df %>% arrange(desc(.data[[col]])) %>% slice(1:k)
  else df %>% arrange(.data[[col]]) %>% slice(1:k)
}

lbs <- purrr::map_dfr(model_prefixes, function(m) {
  tibble(
    model = m,
    metric = c("nRMSE","MAPD","R2_LOO","Rho_Spearman"),
    top = list(
      topk(res, paste0(m, "__nRMSE"), k = 20, decreasing = FALSE),
      topk(res, paste0(m, "__MAPD"),  k = 20, decreasing = FALSE),
      topk(res, paste0(m, "__R2_LOO"),k = 20, decreasing = TRUE),
      topk(res, paste0(m, "__Rho_Spearman"), k = 20, decreasing = TRUE)
    )
  )
})

saveRDS(lbs, file.path(RESULTS_DIR, "leaderboards_all_models.rds"))

lbs_flat <- lbs %>%
  unnest(top) %>%
  select(
    model, metric,
    name,
    use_siec_all, supplier_non_euets, buyer_sector_siec, emissions_weighted, supplier_nace_filter,
    matches("__nRMSE$|__MAPD$|__R2_LOO$|__Rho_Spearman$|__n_obs$")
  )

write_csv(lbs_flat, file.path(RESULTS_DIR, "leaderboards_all_models.csv"))

lbs
