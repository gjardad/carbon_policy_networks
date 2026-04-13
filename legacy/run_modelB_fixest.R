###############################################################################
# run_modelB_fixest.R
#
# PURPOSE:
#   Implements Model B ("portable") estimation + validation:
#     log(emissions_it) = year FE + beta * log(fuel_proxy_it) + e_it
#
#   Provides:
#     - LOFO CV (leave one firm out)
#     - LOSO CV (leave one sector out)  [recommended stress test]
#
# NOTES:
#   - Filters emissions > 0, proxy not NA, and proxy > 0 (for logs).
#   - Uses smearing factor on training residuals for level predictions.
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))
source(file.path(FUN_DIR, "lofo_metrics.R"))

run_lofo_modelB <- function(df,
                            firm_col = "firm_id",
                            year_col = "year",
                            emissions_col = "emissions",
                            proxy_col = "fuel_proxy",
                            verbose = TRUE) {
  
  df0 <- df %>%
    filter(.data[[emissions_col]] > 0,
           !is.na(.data[[proxy_col]]),
           .data[[proxy_col]] > 0) %>%
    mutate(year_factor = factor(.data[[year_col]]))
  
  firm_ids <- unique(df0[[firm_col]])
  n_firms <- length(firm_ids)
  
  if (verbose) {
    log_step(paste0("Model B LOFO sample: n_obs=", nrow(df0),
                    ", n_firms=", n_firms,
                    ", n_years=", n_distinct(df0$year_factor)))
  }
  
  out_list <- vector("list", n_firms)
  loop_start <- Sys.time()
  
  for (k in seq_along(firm_ids)) {
    if (verbose) progress_eta(k, n_firms, loop_start, every = 50, prefix = "  ")
    
    f <- firm_ids[[k]]
    train <- df0 %>% filter(.data[[firm_col]] != f)
    test  <- df0 %>% filter(.data[[firm_col]] == f)
    
    fit <- feols(
      as.formula(paste0("log(", emissions_col, ") ~ log(", proxy_col, ") | year_factor")),
      data = train,
      warn = FALSE
    )
    
    smear <- mean(exp(resid(fit)), na.rm = TRUE)
    test$pred <- exp(predict(fit, newdata = test)) * smear
    
    out_list[[k]] <- test %>%
      transmute(
        firm_id = .data[[firm_col]],
        year = .data[[year_col]],
        emissions = .data[[emissions_col]],
        fuel_proxy = .data[[proxy_col]],
        pred = pred
      )
  }
  
  df_lofo <- bind_rows(out_list)
  perf <- summarise_perf(df_lofo)
  
  list(df_used = df0, df_lofo = df_lofo, perf = perf)
}

run_loso_modelB <- function(df,
                            sector_col = "sector",   # here sector means nace2d
                            year_col = "year",
                            emissions_col = "emissions",
                            proxy_col = "fuel_proxy",
                            min_firms_per_sector = 3,
                            verbose = TRUE) {
  
  df0 <- df %>%
    filter(.data[[emissions_col]] > 0,
           !is.na(.data[[proxy_col]]),
           .data[[proxy_col]] > 0) %>%
    mutate(
      year_factor = factor(.data[[year_col]]),
      sector_factor = factor(.data[[sector_col]])
    )
  
  # Optional: drop tiny sectors for more stable LOSO folds
  sector_counts <- df0 %>%
    group_by(sector_factor) %>%
    summarise(n_firms = n_distinct(firm_id), .groups = "drop")
  
  valid_sectors <- sector_counts %>%
    filter(n_firms >= min_firms_per_sector) %>%
    pull(sector_factor)
  
  df0 <- df0 %>% filter(sector_factor %in% valid_sectors)
  
  sectors <- unique(df0$sector_factor)
  n_sectors <- length(sectors)
  
  if (verbose) {
    log_step(paste0("Model B LOSO sample: n_obs=", nrow(df0),
                    ", n_sectors=", n_sectors,
                    ", n_years=", n_distinct(df0$year_factor)))
  }
  
  out_list <- vector("list", n_sectors)
  loop_start <- Sys.time()
  
  for (k in seq_along(sectors)) {
    if (verbose) progress_eta(k, n_sectors, loop_start, every = 10, prefix = "  ")
    
    s <- sectors[[k]]
    train <- df0 %>% filter(sector_factor != s)
    test  <- df0 %>% filter(sector_factor == s)
    
    fit <- feols(
      as.formula(paste0("log(", emissions_col, ") ~ log(", proxy_col, ") | year_factor")),
      data = train,
      warn = FALSE
    )
    
    smear <- mean(exp(resid(fit)), na.rm = TRUE)
    test$pred <- exp(predict(fit, newdata = test)) * smear
    
    out_list[[k]] <- test %>%
      transmute(
        sector = as.character(.data[[sector_col]]),
        year = .data[[year_col]],
        emissions = .data[[emissions_col]],
        fuel_proxy = .data[[proxy_col]],
        pred = pred
      )
  }
  
  df_loso <- bind_rows(out_list)
  perf <- summarise_perf(df_loso)
  
  list(df_used = df0, df_loso = df_loso, perf = perf)
}
