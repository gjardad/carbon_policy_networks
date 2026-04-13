###############################################################################
# run_lofo_fixest.R
#
# PURPOSE:
#   Runs LOFO CV (leave one firm out) for the model:
#     log(emissions) ~ log(fuel_proxy) | sector FE + year FE
#
#   Features:
#     - filters emissions > 0 and non-missing proxy
#     - drops sectors with < 3 firms (so training FE always identified)
#     - smearing factor on training residuals for log-to-level predictions
#     - progress + ETA printing
#
# EXPECTED LOCATION:
#   /code/loocv/functions/run_lofo_fixest.R
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))
source(file.path(FUN_DIR, "lofo_metrics.R"))

drop_small_sectors <- function(df, firm_col, sector_col, min_firms = 3) {
  sector_counts <- df %>%
    group_by(.data[[sector_col]]) %>%
    summarise(n_firms = n_distinct(.data[[firm_col]]), .groups = "drop")
  
  valid <- sector_counts %>%
    filter(n_firms >= min_firms) %>%
    pull(.data[[sector_col]])
  
  df %>% filter(.data[[sector_col]] %in% valid)
}

run_lofo_fixest_log_fuel <- function(df,
                                     firm_col = "firm_id",
                                     year_col = "year",
                                     sector_col = "sector",
                                     emissions_col = "emissions",
                                     proxy_col = "fuel_proxy",
                                     min_firms_per_sector = 3,
                                     verbose = TRUE) {
  
  df0 <- df %>%
    filter(.data[[emissions_col]] > 0,
           !is.na(.data[[proxy_col]])) %>%
    mutate(
      year_factor = factor(.data[[year_col]]),
      sector_factor = factor(.data[[sector_col]])
    )
  
  df0 <- drop_small_sectors(df0, firm_col, "sector_factor", min_firms_per_sector)
  
  firm_ids <- unique(df0[[firm_col]])
  n_firms <- length(firm_ids)
  
  if (verbose) {
    log_step(paste0(
      "LOFO sample after filters: n_obs=", nrow(df0),
      ", n_firms=", n_firms,
      ", n_sectors=", n_distinct(df0$sector_factor),
      ", n_years=", n_distinct(df0$year_factor)
    ))
  }
  
  out_list <- vector("list", n_firms)
  loop_start <- Sys.time()
  
  for (k in seq_along(firm_ids)) {
    if (verbose) progress_eta(k, n_firms, loop_start, every = 50, prefix = "  ")
    
    firm_k <- firm_ids[[k]]
    train <- df0 %>% filter(.data[[firm_col]] != firm_k, .data[[proxy_col]] > 0)
    test  <- df0 %>% filter(.data[[firm_col]] == firm_k, .data[[proxy_col]] > 0)
    
    fit <- feols(
      as.formula(paste0("log(", emissions_col, ") ~ log(", proxy_col, ") | year_factor + sector_factor")),
      data = train,
      warn = FALSE
    )
    
    smear <- mean(exp(resid(fit)), na.rm = TRUE)
    test$pred <- exp(predict(fit, newdata = test)) * smear
    
    out_list[[k]] <- test %>%
      transmute(
        firm_id = .data[[firm_col]],
        year = .data[[year_col]],
        sector = .data[[sector_col]],
        emissions = .data[[emissions_col]],
        fuel_proxy = .data[[proxy_col]],
        pred = pred
      )
  }
  
  df_lofo <- bind_rows(out_list)
  perf <- summarise_perf(df_lofo)
  
  list(df_used = df0, df_lofo = df_lofo, perf = perf)
}
