#### HEADER -------

## Regressions on price pass-through of carbon pricing shocks
# at the yearly level

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
  mutate(year = year(month)) %>% 
  group_by(sector, country, year) %>% 
  summarise(avg_yearly_prices = mean(log_prices, na.rm = T)) %>% 
  mutate(del_log_prices = avg_yearly_prices - lag(avg_yearly_prices)) %>% 
  ungroup()

del_pz <- emission_price %>% 
  mutate(date = as.Date(date),                     # Ensure 'date' is Date class
         month = as.Date(format(date, "%Y-%m-01")),
         year = year(month)) %>% 
  mutate(log_pz = log(price)) %>%
  group_by(year) %>%
  summarize(avg_yearly_log_pz = mean(log_pz, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(del_log_pz = avg_yearly_log_pz - lag(avg_yearly_log_pz)) %>%    # Calculate first difference of log prices
  ungroup()

carbon_shocks <- carbon_shocks %>% 
  mutate(month = as.Date(paste0(substring(Date, 1, 4), "-", substring(Date, 6, 7), "-01")),
         year = year(month)) %>%
  group_by(year) %>% 
  mutate(yearly_shock = sum(Shock, na.rm = TRUE)) %>% 
  select(year, yearly_shock) %>% 
  distinct()

df_regression <- del_ippi %>% 
  select(sector, country, year, del_log_prices) %>% 
  left_join(net_exposure %>% select(country, sector, net_exposure),
            by = c("country", "sector")) %>% 
  left_join(del_pz %>% select(year, del_log_pz),
            by = c("year")) %>% 
  left_join(carbon_shocks %>% select(year, yearly_shock),
            by = c("year")) %>%
  rename(net_emissions_share = net_exposure) %>% 
  mutate(exposure_to_pz = net_emissions_share*del_log_pz) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(exposure_to_shock = net_emissions_share*yearly_shock)

# Regressions -----

library(ivreg)

# OLS
ols_model <- lm(del_log_prices ~ exposure_to_pz, data = df_regression)
summary(ols_model)

ols_model_fe <- lm(del_log_prices ~ year + exposure_to_pz, data = df_regression)
summary(ols_model_fe)

# TSLS regression with shock as an instrument
iv_model_shock <- ivreg(del_log_prices ~ exposure_to_pz | exposure_to_shock, data = df_regression)
summary(iv_model_shock)

iv_model_shock_fe <- ivreg(del_log_prices ~ year + exposure_to_pz | year + exposure_to_shock, data = df_regression)
summary(iv_model_shock_fe)




