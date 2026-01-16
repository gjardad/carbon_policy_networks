###############################################################################
# 04_collect_results.R
#
# PURPOSE:
#   Loads the proxy-by-model performance table and reports top-performing
#   proxy definitions for each model (A, B_LOFO, B_LOSO) and each metric:
#     - nRMSE (min)
#     - MAPD  (min)
#     - R2_LOO (max)
#     - Rho_Spearman (max)
#
# OUTPUT:
#   Returns a named list of tibbles you can print / inspect.
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))

res <- readRDS(file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))

# Detect whether LOSO columns exist
has_loso <- any(grepl("^B_LOSO_", names(res)))

top_k <- function(df, metric_col, k = 10, decreasing = FALSE) {
  if (!metric_col %in% names(df)) return(NULL)
  out <- df %>%
    arrange(if (decreasing) desc(.data[[metric_col]]) else .data[[metric_col]]) %>%
    slice(1:k) %>%
    select(name,
           starts_with("use_"),
           starts_with("supplier_"),
           starts_with("buyer_"),
           starts_with("emissions_"),
           starts_with("A_"),
           starts_with("B_LOFO_"),
           if (has_loso) starts_with("B_LOSO_") else NULL)
  out
}

make_leaderboards <- function(prefix) {
  list(
    top_nRMSE = top_k(res, paste0(prefix, "nRMSE"), k = 10, decreasing = FALSE),
    top_MAPD  = top_k(res, paste0(prefix, "MAPD"),  k = 10, decreasing = FALSE),
    top_R2    = top_k(res, paste0(prefix, "R2_LOO"), k = 10, decreasing = TRUE),
    top_Rho   = top_k(res, paste0(prefix, "Rho_Spearman"), k = 10, decreasing = TRUE)
  )
}

out <- list(
  ModelA = make_leaderboards("A_"),
  ModelB_LOFO = make_leaderboards("B_LOFO_")
)

if (has_loso) out$ModelB_LOSO <- make_leaderboards("B_LOSO_")

out
