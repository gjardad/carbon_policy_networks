###############################################################################
# run_lofo_fixest_generic.R
#
# PURPOSE:
#   Generic LOFO runner for log(emissions) models estimated with fixest.
#   Supports:
#     - RHS: log(fuel_proxy) and/or log(revenue)
#     - FE: additive (year + sector) or interactive (year^sector)
#     - sector can be NULL (year FE only)
#
# RETURNS:
#   list(perf = tibble(...), df_lofo = firm-year predictions)
###############################################################################

run_lofo_fixest_generic <- function(df,
                                    firm_col,
                                    year_col,
                                    emissions_col,
                                    fuel_col = NULL,
                                    revenue_col = NULL,
                                    sector_col = NULL,
                                    fe_structure = c("additive", "interactive"),
                                    min_firms_per_sector = 3,
                                    verbose = TRUE) {
  fe_structure <- match.arg(fe_structure)
  
  stopifnot(all(c(firm_col, year_col, emissions_col) %in% names(df)))
  if (!is.null(fuel_col)) stopifnot(fuel_col %in% names(df))
  if (!is.null(revenue_col)) stopifnot(revenue_col %in% names(df))
  if (!is.null(sector_col)) stopifnot(sector_col %in% names(df))
  
  # --- basic filters (consistent with your old code philosophy) ---
  d0 <- df %>%
    dplyr::mutate(
      .firm = .data[[firm_col]],
      .year = .data[[year_col]],
      .y    = .data[[emissions_col]]
    ) %>%
    dplyr::filter(.y > 0, !is.na(.firm), !is.na(.year))
  
  if (!is.null(fuel_col)) {
    d0 <- d0 %>%
      dplyr::mutate(.fuel = as.numeric(.data[[fuel_col]])) %>%
      dplyr::filter(!is.na(.fuel), .fuel > 0)
  } else {
    d0 <- d0 %>% dplyr::mutate(.fuel = NA_real_)
  }
  
  if (!is.null(revenue_col)) {
    d0 <- d0 %>%
      dplyr::mutate(.rev = as.numeric(.data[[revenue_col]])) %>%
      dplyr::filter(!is.na(.rev), .rev > 0)
  } else {
    d0 <- d0 %>% dplyr::mutate(.rev = NA_real_)
  }
  
  if (!is.null(sector_col)) {
    d0 <- d0 %>%
      dplyr::mutate(.sector = as.character(.data[[sector_col]])) %>%
      dplyr::filter(!is.na(.sector) & .sector != "")
    
    # Drop singleton / small sectors (training identifiability)
    sector_counts <- d0 %>%
      dplyr::group_by(.sector) %>%
      dplyr::summarise(n_firms = dplyr::n_distinct(.firm), .groups = "drop")
    
    keep_sectors <- sector_counts %>%
      dplyr::filter(n_firms >= min_firms_per_sector) %>%
      dplyr::pull(.sector)
    
    d0 <- d0 %>% dplyr::filter(.sector %in% keep_sectors)
  } else {
    d0 <- d0 %>% dplyr::mutate(.sector = NA_character_)
  }
  
  # --- build formula strings for fixest ---
  # y part:
  y_part <- "log(.y)"
  
  # x part:
  x_terms <- c()
  if (!is.null(fuel_col))    x_terms <- c(x_terms, "log(.fuel)")
  if (!is.null(revenue_col)) x_terms <- c(x_terms, "log(.rev)")
  x_part <- if (length(x_terms) == 0) "1" else paste(x_terms, collapse = " + ")
  
  # FE part:
  fe_part <- if (is.null(sector_col)) {
    ".year"
  } else {
    if (fe_structure == "additive") ".year + .sector" else ".year^.sector"
  }
  
  fml <- stats::as.formula(paste0(y_part, " ~ ", x_part, " | ", fe_part))
  
  # --- LOFO loop ---
  firm_ids <- sort(unique(d0$.firm))
  n_firms  <- length(firm_ids)
  if (verbose) message("LOFO: ", n_firms, " firms")
  
  cv_list <- vector("list", n_firms)
  start_all <- Sys.time()
  
  for (k in seq_along(firm_ids)) {
    if (verbose && (k %% 25 == 0 || k == 1 || k == n_firms)) {
      elapsed <- as.numeric(difftime(Sys.time(), start_all, units = "secs"))
      rate <- elapsed / max(1, k)
      eta  <- rate * (n_firms - k)
      message(sprintf("  firm %d/%d | elapsed %.1f min | ETA %.1f min",
                      k, n_firms, elapsed/60, eta/60))
    }
    
    firm_k <- firm_ids[k]
    train  <- d0 %>% dplyr::filter(.firm != firm_k)
    test   <- d0 %>% dplyr::filter(.firm == firm_k)
    
    # estimate
    mod <- fixest::feols(fml, data = train, warn = FALSE)
    
    # smearing factor (log-normal correction)
    smear <- mean(exp(residuals(mod)), na.rm = TRUE)
    
    # predict log then level
    log_pred <- stats::predict(mod, newdata = test)
    pred     <- exp(log_pred) * smear
    
    cv_list[[k]] <- test %>%
      dplyr::transmute(
        firm_id = .firm,
        year    = .year,
        emissions = .y,
        pred = as.numeric(pred)
      )
  }
  
  df_lofo <- dplyr::bind_rows(cv_list) %>%
    dplyr::mutate(err = pred - emissions, sq_err = err^2, abs_err = abs(err))
  
  # performance metrics (same definitions you used)
  sst <- sum((df_lofo$emissions - mean(df_lofo$emissions, na.rm = TRUE))^2, na.rm = TRUE)
  rmse <- sqrt(mean(df_lofo$sq_err, na.rm = TRUE))
  perf <- tibble::tibble(
    nRMSE = rmse / stats::sd(df_lofo$emissions, na.rm = TRUE),
    MAPD  = mean(abs(df_lofo$err / df_lofo$emissions), na.rm = TRUE),
    R2_LOO = 1 - sum(df_lofo$sq_err, na.rm = TRUE) / sst,
    Rho_Spearman = stats::cor(df_lofo$emissions, df_lofo$pred,
                              method = "spearman", use = "complete.obs"),
    n_obs = nrow(df_lofo)
  )
  
  list(perf = perf, df_lofo = df_lofo, formula = fml)
}
