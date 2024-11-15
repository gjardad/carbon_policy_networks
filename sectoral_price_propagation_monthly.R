#### HEADER -------

## Regressions on price pass-through of carbon pricing shocks
# at the monthly level

#####################

## Setup ------
rm(list = ls())

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------

  library(readxl)
  carbon_shocks <- read_excel(paste0(raw_data,"/carbonPolicyShocks.xlsx"))
  
  emission_price <- read_csv(paste0(raw_data,"/icap_price.csv")) %>% 
    select(1,3) %>% 
    slice(-1) %>% 
    rename(date = 1, price = 2) %>% 
    mutate(price = as.numeric(price))
  
  load(paste0(proc_data,"/sector_net_exposure_emissions.RData"))
  
  load(paste0(proc_data,"/ippi_by_country_nace_month.RData"))

# Clean data for regressions ------
  
  # sector categories in each data set
  split_columns <- strsplit(as.character(net_exposure$country_sector), "_")
  
  net_exposure$country <- sapply(split_columns, `[`, 1)
  net_exposure$sector <- sapply(split_columns, `[`, 2)
  
  sectors_from_figaro <- unique(net_exposure$sector)
  
  sectors_from_ippi <- unique(ippi$sector)
  
  ippi_B <- ippi %>%
    filter(sector %in% c("B05", "B06", "B07", "B08", "B09")) %>%
    group_by(country, month) %>%
    summarize(prices = mean(prices, na.rm = TRUE), .groups = 'drop') %>%
    mutate(sector = "B") %>% 
    select(sector, country, month, prices)
  
  ippi_C10T12 <- ippi %>%
    filter(sector %in% c("C10", "C11", "C12")) %>%
    group_by(country, month) %>%
    summarize(prices = mean(prices, na.rm = TRUE), .groups = 'drop') %>%
    mutate(sector = "C10T12") %>% 
    select(sector, country, month, prices)
  
  ippi_C13T15 <- ippi %>%
    filter(sector %in% c("C13", "C14", "C15")) %>%
    group_by(country, month) %>%
    summarize(prices = mean(prices, na.rm = TRUE), .groups = 'drop') %>%
    mutate(sector = "C13T15") %>% 
    select(sector, country, month, prices)
  
  sectors <- c("B", "C10T12", "C13T15", intersect(sectors_from_figaro, sectors_from_ippi))
  
  del_ippi <- ippi %>% 
    filter(sector %in% intersect(sectors_from_figaro, sectors_from_ippi)) %>% 
    bind_rows(ippi_B, ippi_C10T12, ippi_C13T15) %>% 
    mutate(log_prices = log(prices)) %>% 
    arrange(sector, country, month) %>% 
    group_by(sector, country) %>% 
    mutate(del_log_prices = log_prices - lag(log_prices)) %>% 
    ungroup()
  
  del_pz <- emission_price %>% 
    mutate(date = as.Date(date),                     # Ensure 'date' is Date class
           month = as.Date(format(date, "%Y-%m-01"))) %>%  # Format month as the first day of the month (Date format)
    group_by(month) %>%
    summarize(avg_price = mean(price, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(log_pz = log(avg_price),                  # Calculate log of average price
           del_log_pz = log_pz - lag(log_pz)) %>%    # Calculate first difference of log prices
    ungroup()
  
  carbon_shocks <- carbon_shocks %>% 
    mutate(month = as.Date(paste0(substring(Date, 1, 4), "-", substring(Date, 6, 7), "-01")))
  
  df_regression <- del_ippi %>% 
    select(sector, country, month, del_log_prices) %>% 
    left_join(net_exposure %>% select(country, sector, net_exposure),
              by = c("country", "sector")) %>% 
    left_join(del_pz %>% select(month, del_log_pz),
              by = c("month")) %>% 
    left_join(carbon_shocks %>% select(month, Surprise, Shock),
              by = c("month")) %>%
    rename(net_emissions_share = net_exposure) %>% 
    mutate(exposure_to_pz = net_emissions_share*del_log_pz) %>% 
    mutate(month = as.factor(month)) %>% 
    mutate(exposure_to_surprise = net_emissions_share*Surprise,
           exposure_to_shock = net_emissions_share*Shock)
  
  # robustness
  df_regression_no_electricity <- df_regression %>% 
    filter(sector != "D35")
  
  euets <- c("D35", "C19", "B", "C23", "C24", "C20", "C17")
  noneuets <- setdiff(unique(df_regression$sector), euets)
  
  europe <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "DE", "EE", "EL", "ES", "FI",
              "FR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO",
              "SK", "SE", "SI")
  
  df_regression_noneuets <- df_regression %>% 
    filter(sector %in% noneuets)
  
  df_regression_europe <- df_regression %>% 
    filter(country %in% europe)
  
  df_regression_europe_noneuets <- df_regression %>% 
    filter(country %in% europe & sector %in% noneuets)
  
  df_regression_europe_euets <- df_regression %>% 
    filter(country %in% europe & sector %in% euets)
  
  df_regression_europe_energy <- df_regression %>% 
    filter(country %in% europe & sector == "D35")
  
# Regressions -----
  
  library(ivreg)
  
  # OLS
  ols_model <- lm(del_log_prices ~ exposure_to_pz, data = df_regression)
  summary(ols_model)
  
  ols_model_fe <- lm(del_log_prices ~ month + exposure_to_pz, data = df_regression)
  summary(ols_model_fe)
  
  # TSLS regression with surprise as an instrument
  iv_model_surprise_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_surprise, data = df_regression)
  summary(iv_model_surprise)
  
  # TSLS regression with shock as an instrument
  iv_model_shock <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_shock, data = df_regression)
  summary(iv_model_shock)
  
  iv_model_shock_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression)
  summary(iv_model_shock_fe)
  
  # Robustness: only non-EUETS sectors
  
    # OLS
    ols_model <- lm(del_log_prices ~ exposure_to_pz, data = df_regression_noneuets)
    summary(ols_model)
    
    ols_model_fe <- lm(del_log_prices ~ month + exposure_to_pz, data = df_regression_noneuets)
    summary(ols_model_fe)
    
    # TSLS regression with surprise as an instrument
    iv_model_surprise <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_surprise, data = df_regression_noneuets)
    summary(iv_model_surprise)
    
    # TSLS regression with shock as an instrument
    iv_model_shock <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_shock, data = df_regression_noneuets)
    summary(iv_model_shock)
    
    iv_model_shock_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression_noneuets)
    summary(iv_model_shock_fe)
    
  # Robustness: only Europe
    
    # OLS
    ols_model <- lm(del_log_prices ~ exposure_to_pz, data = df_regression_europe)
    summary(ols_model)
    
    ols_model_fe <- lm(del_log_prices ~ month + exposure_to_pz, data = df_regression_europe)
    summary(ols_model_fe)
    
    # IV w/ all sectors
    iv_model_surprise_europe <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_surprise, data = df_regression_europe)
    summary(iv_model_surprise_europe)
    
    iv_model_surprise_europe_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_surprise, data = df_regression_europe)
    summary(iv_model_surprise_europe_fe)
    
    iv_model_shock_europe <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_shock, data = df_regression_europe)
    summary(iv_model_shock_europe)
    
    iv_model_shock_europe_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression_europe)
    summary(iv_model_shock_europe_fe)
    
    # OLS w/ only targeted
    ols_model_euets <- lm(del_log_prices ~ exposure_to_pz, data = df_regression_europe_euets)
    summary(ols_model_euets)
    
    ols_model_euets_fe <- lm(del_log_prices ~ month + exposure_to_pz, data = df_regression_europe_euets)
    summary(ols_model_euets_fe)
    
    # IV w/ only targeted
    iv_model_surprise_europe <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_surprise, data = df_regression_europe)
    summary(iv_model_surprise_europe)
    
    iv_model_surprise_europe_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_surprise, data = df_regression_europe)
    summary(iv_model_surprise_europe_fe)
    
    iv_model_shock <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_shock, data = df_regression_europe_euets)
    summary(iv_model_shock)
  
    iv_model_shock_euets_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression_europe_euets)
    summary(iv_model_shock_euets_fe)
    
    # OLS w/ only non-targeted
    ols_model_noneuets <- lm(del_log_prices ~ exposure_to_pz, data = df_regression_europe_noneuets)
    summary(ols_model_noneuets)
    
    ols_model_noneuets_fe <- lm(del_log_prices ~ month + exposure_to_pz, data = df_regression_europe_noneuets)
    summary(ols_model_noneuets_fe)
    
    # IV w/ only non-targeted
    iv_model_shock <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_shock, data = df_regression_europe_noneuets)
    summary(iv_model_shock)
    
    iv_model_shock_noneuets_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression_europe_noneuets)
    summary(iv_model_shock_noneuets_fe)
    
    # OLS with only energy
    ols_model_energy_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression_europe_energy)
    summary(iv_model_shock_energy_fe)    
    
    # IV w/ only energy
    iv_model_shock_energy_fe <- ivreg(del_log_prices ~ month + exposure_to_pz | month + exposure_to_shock, data = df_regression_europe_energy)
    summary(iv_model_shock_energy_fe)
    
    
    
    
    
  
