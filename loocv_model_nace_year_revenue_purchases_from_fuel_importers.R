#### HEADER -------

## This code performs leave-one-firm-out (LOFO) validation for emissions predictive model

# The model is the simplest possible: emissions ~ sector FE + year FE + + revenue + fuel_consumption
# where fuel consumption is simply the amount purchased from firms that are fuel importers

# I compare model in levels and logs

#####################

## Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Import and clean data --------

  load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))

  load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
  
  df <- amount_spent_on_fuel_by_firm_year %>%
    left_join(firm_year_belgian_euets %>% select(vat, year, revenue),
              by = c("vat_j_ano" = "vat", "year")
    ) %>%
    mutate(amount_spent_on_fuel_from_non_euets_importers = amount_spent_on_fuel - purchases_from_importers_euets) %>% 
    filter(emissions > 0,
           !is.na(amount_spent_on_fuel)) %>%
    mutate(
      year   = as.factor(year),
      nace5d = as.factor(nace5d)
    ) 
  
  # Check how many firms have observed emissions
  length(unique(df$vat_j_ano))
  
# Drop singleton firms -------
  
  #obs: there are some sectors with only one firm among the firms with emissions > 0
  # this means it's not possible to estimate the NACE fixed-effect in the LOFO regs
  # so we build training data excluding singleton firms
  
  sector_firm_counts <- df %>%
    group_by(nace5d) %>%
    summarise(n_firms = n_distinct(vat_j_ano), .groups = "drop")
  
  valid_sectors <- sector_firm_counts %>%
    filter(n_firms >= 2) %>%
    pull(nace5d)
  
  df_no_singletons <- df %>% filter(nace5d %in% valid_sectors)
  
  df_no_singletons <- df_no_singletons %>%
    mutate(year = factor(year),
           nace5d = factor(nace5d))

#### Leave-one-out cross-validation --------
  
  firm_ids <- unique(df_no_singletons$vat_j_ano)
  n_firms  <- length(firm_ids)
  
  cv_results <- vector("list", n_firms)
  
  for (k in seq_along(firm_ids)) {
    firm_k <- firm_ids[k]
    
    # Progress message
    message("LOFO: leaving out firm ", k, " of ", n_firms, 
            " (vat_j_ano = ", firm_k, ")")
    
    # Training set: all OTHER firms
    train_data <- df_no_singletons %>%
      filter(vat_j_ano != firm_k)
    
    # Test set: the left-out firm (all its years)
    test_data <- df_no_singletons %>%
      filter(vat_j_ano == firm_k)
    
    ## Fit model in levels ------
    
      # FEs + revenue
      model_level_FE <- lm(
        emissions ~ year + nace5d + revenue,
        data = train_data
      )
      test_data$pred_level_FE <- predict(model_level_FE, newdata = test_data)
    
      # FEs + revenue + fuel consumption
      model_level_fuel <- lm(
        emissions ~ year + nace5d + revenue + amount_spent_on_fuel,
        data = train_data
      )
      test_data$pred_level_fuel <- predict(model_level_fuel, newdata = test_data)
    
    ## Fit log-log model
      
      # FEs + fuel consumption
      model_log_fuel <- lm(
        log(emissions) ~ year + nace5d + log(amount_spent_on_fuel),
        data = train_data
      )
      smearing_factor <- mean(exp(residuals(model_log_fuel)), na.rm = TRUE)
      test_data$log_pred_fuel <- predict(model_log_fuel, newdata = test_data)
      test_data$pred_log_fuel <- exp(test_data$log_pred_fuel) * smearing_factor
      
      # FEs + fuel consumption from non-EUETS importers
      model_log_fuel_non_euets <- lm(
        log(emissions) ~ year + nace5d + log(amount_spent_on_fuel_from_non_euets_importers),
        data = train_data
      )
      smearing_factor <- mean(exp(residuals(model_log_fuel)), na.rm = TRUE)
      test_data$log_pred_fuel_non_euets <- predict(model_log_fuel_non_euets, newdata = test_data)
      test_data$pred_log_fuel_non_euets <- exp(test_data$log_pred_fuel_non_euets) * smearing_factor
    
      # FEs + revenue + fuel consumption
      model_log_rev_fuel <- lm(
        log(emissions) ~ year + nace5d + log(revenue) + log(amount_spent_on_fuel),
        data = train_data
      )
      smearing_factor <- mean(exp(residuals(model_log_rev_fuel)), na.rm = TRUE)
      test_data$log_pred_rev_fuel <- predict(model_log_rev_fuel, newdata = test_data)
      test_data$pred_log_rev_fuel <- exp(test_data$log_pred_rev_fuel) * smearing_factor
      
      # FEs + revenue + fuel consumption from non-EUETS importers
      model_log_rev_fuel_non_euets <- lm(
        log(emissions) ~ year + nace5d + log(revenue) + log(amount_spent_on_fuel_from_non_euets_importers),
        data = train_data
      )
      smearing_factor <- mean(exp(residuals(model_log_rev_fuel_non_euets)), na.rm = TRUE)
      test_data$log_pred_rev_fuel_non_euets <- predict(model_log_rev_fuel_non_euets, newdata = test_data)
      test_data$pred_log_rev_fuel_non_euets <- exp(test_data$log_pred_rev_fuel_non_euets) * smearing_factor
    
    # Store results for this firm
    cv_results[[k]] <- test_data
  }
  
### Combine all predictions into one data frame -----
  
  df_lofo <- bind_rows(cv_results) %>%
    mutate(
      # errors: level FE + revenue
      err_level_FE     = pred_level_FE - emissions,
      abs_err_level_FE = abs(err_level_FE),
      sq_err_level_FE  = err_level_FE^2,
      
      # errors: level FE + revenue + fuel
      err_level_fuel     = pred_level_fuel - emissions,
      abs_err_level_fuel = abs(err_level_fuel),
      sq_err_level_fuel  = err_level_fuel^2,
      
      # errors: FE + log fuel
      err_log_fuel     = pred_log_fuel - emissions,
      abs_err_log_fuel = abs(err_log_fuel),
      sq_err_log_fuel  = err_log_fuel^2,
      
      # errors: FE + log fuel from non-EUETS importers
      err_log_fuel_non_euets     = pred_log_fuel_non_euets - emissions,
      abs_err_log_fuel_non_euets = abs(err_log_fuel_non_euets),
      sq_err_log_fuel_non_euets  = err_log_fuel_non_euets^2,
      
      # errors: FE + log revenue + log fuel
      err_log_rev_fuel     = pred_log_rev_fuel - emissions,
      abs_err_log_rev_fuel = abs(err_log_rev_fuel),
      sq_err_log_rev_fuel  = err_log_rev_fuel^2,
      
      # errors: FE + log revenue + log fuel from non-EUETS importers
      err_log_rev_fuel_non_euets     = pred_log_rev_fuel_non_euets - emissions,
      abs_err_log_rev_fuel_non_euets = abs(err_log_rev_fuel_non_euets),
      sq_err_log_rev_fuel_non_euets  = err_log_rev_fuel_non_euets^2
    )
  
### Overall performance metrics -------
  
  # Common SST for R2
  sst <- sum((df_lofo$emissions - mean(df_lofo$emissions))^2, na.rm = TRUE)
  
  metrics <- function(err, err_sq, err_abs, pred, emissions) {
    RMSE <- sqrt(mean(err_sq, na.rm = TRUE))
    normalized_RMSE <- RMSE / sd(emissions)
    MAPD <- mean(abs(err/emissions))
    MAE  <- mean(err_abs, na.rm = TRUE)
    SSE  <- sum(err_sq, na.rm = TRUE)
    R2   <- 1 - SSE / sst
    Rho  <- cor(df_lofo$emissions, pred,
                method = "spearman", use = "complete.obs")
    c(nRMSE = normalized_RMSE, MAPD = MAPD,
      R2_LOO = R2, Rho_Spearman = Rho)
  }
  
  m_level_FE    <- metrics(df_lofo$err_level_FE,
                           df_lofo$sq_err_level_FE,
                           df_lofo$abs_err_level_FE,
                           df_lofo$pred_level_FE,
                           df_lofo$emissions)
  
  m_level_fuel  <- metrics(df_lofo$err_level_fuel,
                           df_lofo$sq_err_level_fuel,
                           df_lofo$abs_err_level_fuel,
                           df_lofo$pred_level_fuel,
                           df_lofo$emissions)
  
  m_log_fuel    <- metrics(df_lofo$err_log_fuel,
                           df_lofo$sq_err_log_fuel,
                           df_lofo$abs_err_log_fuel,
                           df_lofo$pred_log_fuel,
                           df_lofo$emissions)
  
  m_log_fuel_non_euets    <- metrics(df_lofo$err_log_fuel_non_euets,
                           df_lofo$sq_err_log_fuel_non_euets,
                           df_lofo$abs_err_log_fuel_non_euets,
                           df_lofo$pred_log_fuel_non_euets,
                           df_lofo$emissions)
  
  m_log_rev_fuel    <- metrics(df_lofo$err_log_rev_fuel,
                           df_lofo$sq_err_log_rev_fuel,
                           df_lofo$abs_err_log_rev_fuel,
                           df_lofo$pred_log_rev_fuel,
                           df_lofo$emissions)
  
  m_log_rev_fuel_non_euets  <- metrics(df_lofo$err_log_rev_fuel_non_euets,
                               df_lofo$sq_err_log_rev_fuel_non_euets,
                               df_lofo$abs_err_log_rev_fuel_non_euets,
                               df_lofo$pred_log_rev_fuel_non_euets,
                               df_lofo$emissions)
  
  perf_comparison <- rbind(
    "Level: FEs + revenue"              = m_level_FE,
    "Level: FEs + revenue + fuel"       = m_level_fuel,
    "Log: FEs + fuel"                   = m_log_fuel,
    "Log: FEs + fuel from non-EUETS importers"                   = m_log_fuel_non_euets,
    "Log: FEs + revenue + fuel"         = m_log_rev_fuel,
    "Log: FEs + revenue + fuel from non-EUETS importers"         = m_log_rev_fuel_non_euets
  )
  
  perf_comparison
  
# Save it ----------
df_training_data <- df_no_singletons
save(df_training_data, file = paste0(proc_data, "/df_used_to_train_model_to_infer_emissions.RData"))
save(df_lofo, file = paste0(int_data, "/results_from_loocv_model_nace_year_revenue_purchases_from_fuel_importers.RData"))
  