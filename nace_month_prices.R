#### HEADER -------

## Create data set with descriptive statistics from prodcom data

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

load(paste0(gert_folder,"/firm_product_month_prodcom.RData"))

# Count number of EUETS and non-EUETS firms by NACE ------

  firm_product_month_prodcom <- firm_product_month_prodcom %>% 
    mutate(
      nace2d = substr(nace5d, 1, 2),
      nace3d = substr(nace5d, 1, 3),
      nace4d = substr(nace5d, 1, 4)
    )
  
  count_firms_by_nace <- function(data, nace_level) {
    data %>%
      group_by(date, !!sym(nace_level), euets) %>%
      summarise(n_firms = n_distinct(vat_ano), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = euets, values_from = n_firms,
                         names_prefix = "n_firms_euets_", values_fill = 0)
  }
  
  nace2d_counts <- count_firms_by_nace(firm_product_month_prodcom, "nace2d")
  save(nace2d_counts, file = paste0(int_data, "/number_of_firms_by_nace2d_euets_in_prodcom.RData"))
  
  nace3d_counts <- count_firms_by_nace(firm_product_month_prodcom, "nace3d")
  save(nace3d_counts, file = paste0(int_data, "/number_of_firms_by_nace3d_euets_in_prodcom.RData"))
  
  nace4d_counts <- count_firms_by_nace(firm_product_month_prodcom, "nace4d")
  save(nace4d_counts, file = paste0(int_data, "/number_of_firms_by_nace4d_euets_in_prodcom.RData"))

# Build price index for EUETS, non-EUETS firms by nace-month --------
  
  price_index_nace4d_prodcom <- firm_product_month_prodcom %>%
    select(vat_ano, date, pc8, v, price, nace4d, euets, firm_price_variation) %>% 
    group_by(vat_ano, date) %>%
    mutate(firm_total_sales = sum(v, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(vat_ano, date) %>% 
    group_by(vat_ano) %>%
    mutate(
      lag_sales = lag(firm_total_sales),
      tornqvist_weight = (firm_total_sales + lag_sales) / 2
    ) %>%
    ungroup() %>% 
    filter(!is.na(firm_price_variation), !is.na(tornqvist_weight)) %>%
    group_by(nace4d, date, euets) %>% 
    mutate(group_total_weight = sum(tornqvist_weight),
           normalized_weight = tornqvist_weight / group_total_weight) %>%
    ungroup() %>% 
    group_by(date, nace4d, euets) %>%
    summarise(
      log_index = sum(log(firm_price_variation) * normalized_weight),
      price_index = exp(log_index),
      .groups = "drop"
    ) %>% 
    group_by(nace4d, euets) %>%
    mutate(price_index_level = cumprod(price_index)) %>%
    ungroup()
  
  save(price_index_nace4d_prodcom, file = paste0(gert_folder,"/price_index_nace4d_prodcom.RData"))

# Version with mock prodcom ---------

mock <- F
if(mock == T){
  
  load(paste0(proc_data,"/firm_product_month_mock_prod.RData"))
  
  # count number of euets and non-euets firms by nace
  firm_product_month_mock_prod <- firm_product_month_mock_prod %>% 
    mutate(
      nace2d = substr(nace5d, 1, 2),
      nace3d = substr(nace5d, 1, 3),
      nace4d = substr(nace5d, 1, 4)
    )
  
  count_firms_by_nace <- function(data, nace_level) {
    data %>%
      group_by(date, !!sym(nace_level), euets) %>%
      summarise(n_firms = n_distinct(vat_ano), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = euets, values_from = n_firms,
                         names_prefix = "n_firms_euets_", values_fill = 0)
  }
  
  nace2d_counts <- count_firms_by_nace(firm_product_month_mock_prod, "nace2d")
  save(nace2d_counts, file = paste0(int_data, "/number_of_firms_by_nace2d_euets_in_prodcom.RData"))
  
  nace3d_counts <- count_firms_by_nace(firm_product_month_mock_prod, "nace3d")
  save(nace3d_counts, file = paste0(int_data, "/number_of_firms_by_nace3d_euets_in_prodcom.RData"))
  
  nace4d_counts <- count_firms_by_nace(firm_product_month_mock_prod, "nace4d")
  save(nace4d_counts, file = paste0(int_data, "/number_of_firms_by_nace4d_euets_in_prodcom.RData"))
  
  # build price index for euets, non-euets firms by nace-month
  
  price_index_nace4d_mock_prod <- firm_product_month_mock_prod %>%
    select(vat_ano, date, pc8, v, price, nace4d, euets, firm_price_variation) %>% 
    group_by(vat_ano, date) %>%
    mutate(firm_total_sales = sum(v, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(vat_ano, date) %>% 
    group_by(vat_ano) %>%
    mutate(
      lag_sales = lag(firm_total_sales),
      tornqvist_weight = (firm_total_sales + lag_sales) / 2
    ) %>%
    ungroup() %>% 
    filter(!is.na(firm_price_variation), !is.na(tornqvist_weight)) %>%
    group_by(nace4d, date, euets) %>% 
    mutate(group_total_weight = sum(tornqvist_weight),
           normalized_weight = tornqvist_weight / group_total_weight) %>%
    ungroup() %>% 
    group_by(date, nace4d, euets) %>%
    summarise(
      log_index = sum(log(firm_price_variation) * normalized_weight),
      price_index = exp(log_index),
      .groups = "drop"
    ) %>% 
    group_by(nace4d, euets) %>%
    mutate(price_index_level = cumprod(price_index)) %>%
    ungroup()
  
  save(price_index_nace4d_mock_prod, file = paste0(proc_data,"/price_index_nace4d_mock_prod.RData"))
}