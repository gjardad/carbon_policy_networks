###############################################################################
# lofo_metrics.R
#
# PURPOSE:
#   Implements your exact performance metrics:
#     - nRMSE (levels, normalized by sd(emissions))
#     - MAPD
#     - LOO R-squared (common SST on levels)
#     - Spearman rank correlation (levels)
#
# EXPECTED LOCATION:
#   /code/loocv/functions/lofo_metrics.R
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))

summarise_perf <- function(df,
                           emissions_col = "emissions",
                           pred_col = "pred") {
  emissions <- df[[emissions_col]]
  pred <- df[[pred_col]]
  
  sst <- sum((emissions - mean(emissions, na.rm = TRUE))^2, na.rm = TRUE)
  
  err <- pred - emissions
  err_sq <- err^2
  
  RMSE <- sqrt(mean(err_sq, na.rm = TRUE))
  nRMSE <- RMSE / sd(emissions, na.rm = TRUE)
  MAPD <- mean(abs(err / emissions), na.rm = TRUE)
  SSE <- sum(err_sq, na.rm = TRUE)
  R2 <- 1 - SSE / sst
  Rho <- suppressWarnings(cor(emissions, pred, method = "spearman", use = "complete.obs"))
  
  tibble(
    nRMSE = nRMSE,
    MAPD = MAPD,
    R2_LOO = R2,
    Rho_Spearman = Rho,
    n_obs = sum(complete.cases(emissions, pred))
  )
}
