#### HEADER -------

## Create data set with info on firm-product-month prices, NACE, and EUETS treatment

# Code meant to be ran within Stata

#####################

## Setup -----
rm(list = ls())

## Recover globals defined in Stata -------

  # Capture command-line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Assign the first argument to jarda_folder
  jarda_folder <- args[1]
  
  gert_folder <- args[2]
  
  prodcom_output <- args[3]

## Setup file paths -----

raw_data <- paste0(jarda_folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(jarda_folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(jarda_folder, "/carbon_policy_networks/data/processed")

output <- paste0(jarda_folder, "/carbon_policy_networks/output")

code <- paste0(jarda_folder, "/carbon_policy_networks/code")

# Libraries ----

.libPaths("E:/Documents/JARDANG/r_packages/win-library/4.2.1")

library(tidyverse)
library(dplyr)
library(lubridate)

# Import data -------

load(paste0(prodcom_output,"/prodcom.RData"))
load(paste0(proc_data, "/df_national_accounts_with_5digits.RData"))
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

# Clean and merge data -----

firm_year_belgian_euets <- firm_year_belgian_euets %>% 
  rename(vat_ano = vat) %>% 
  mutate(euets = 1)

firm_product_month_prodcom <- prodcom %>% 
  filter(q > 0, v > 0) %>% 
  mutate(price = v/q) %>% 
  left_join(df_national_accounts %>% select(vat_ano, nace5d), by = "vat_ano") %>% 
  left_join(firm_year_belgian_euets %>% select(vat_ano, euets, year), by = c("vat_ano", "year")) %>%
  mutate(date = make_date(year, month, 1)) %>% 
  group_by(vat_ano, date) %>% 
  mutate(sales_share = v/sum(v, na.rm = T)) %>% 
  ungroup() %>%
  arrange(vat_ano, pc8, date) %>%
  group_by(vat_ano, pc8) %>%
  mutate(
    lag_sales_share = lag(sales_share),
    tornqvist_weight = (sales_share + lag_sales_share) / 2,
    lag_price = lag(price),
    price_ratio = price / lag_price
  ) %>%
  ungroup() %>% 
  filter(!is.na(price_ratio), !is.na(tornqvist_weight)) %>% 
  group_by(vat_ano, date) %>%
  mutate(
    log_firm_price_variation = sum(log(price_ratio) * tornqvist_weight),
    firm_price_variation = exp(log_firm_price_variation),
  ) %>%
  ungroup() %>% 
  arrange(vat_ano, date) %>%
  group_by(vat_ano) %>%
  mutate(
    firm_price_index = cumprod(firm_price_variation)
  ) %>%
  ungroup()

# save it 
save(firm_product_month_prodcom, file = paste0(prodcom_output,"/firm_product_month_prodcom.RData"))

# Version with mock prodcom ---------

mock <- F
if(mock == T){
  
  load(paste0(proc_data,"/mock_prod.RData"))

  firm_product_month_mock_prod <- mock_prod %>% 
    filter(q > 0, v > 0) %>% 
    mutate(price = v/q) %>% 
    left_join(df_national_accounts %>% select(vat_ano, nace5d), by = "vat_ano") %>% 
    left_join(firm_year_belgian_euets %>% select(vat_ano, euets, year), by = c("vat_ano", "year")) %>%
    mutate(date = make_date(year, month, 1)) %>% 
    group_by(vat_ano, date) %>% 
    mutate(sales_share = v/sum(v, na.rm = T)) %>% 
    ungroup() %>%
    arrange(vat_ano, pc8, date) %>%
    group_by(vat_ano, pc8) %>%
    mutate(
      lag_sales_share = lag(sales_share),
      tornqvist_weight = (sales_share + lag_sales_share) / 2,
      lag_price = lag(price),
      price_ratio = price / lag_price
    ) %>%
    ungroup() %>% 
    filter(!is.na(price_ratio), !is.na(tornqvist_weight)) %>% 
    group_by(vat_ano, date) %>%
    mutate(
      log_firm_price_variation = sum(log(price_ratio) * tornqvist_weight),
      firm_price_variation = exp(log_firm_price_variation),
    ) %>%
    ungroup() %>% 
    arrange(vat_ano, date) %>%
    group_by(vat_ano) %>%
    mutate(
      firm_price_index = cumprod(firm_price_variation)
    ) %>%
    ungroup()

  save(firm_product_month_mock_prod, file = paste0(proc_data,"/firm_product_month_mock_prod.RData"))
}



  
