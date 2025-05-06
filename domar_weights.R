#### HEADER -------

## Generates Domar weights for firms in I-O matrix
# The numerator is firms' turnover and the denominator the sum of firms' sales
# to final demand across all firms in I-O matrix

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
library(dplyr)

# Import data -----

load(paste0(proc_data, "/firm_year_balance_sheet_selected_sample.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))

# Generate vector of Domar weights ------

i <- 0
ordered_domar_weights <- list()

for(y in 2005:2022){
  
  i <- i + 1
  
  # step 1: compute GDP in sample
  firms_ids <- vats_as_ordered_in_io_matrix[[i]]
  
  gdp <- firm_year_balance_sheet_selected_sample %>%
    filter(year == y, vat_ano %in% firms_ids) %>%
    summarise(gdp = sum(sales_final_demand, na.rm = TRUE)) %>%
    pull(gdp)
  
  # step 2: compute domar weights
  df_domar_weights <- firm_year_balance_sheet_selected_sample %>%
    filter(year == y, vat_ano %in% firms_ids) %>%
    mutate(domar_weights = turnover / gdp) %>%
    select(vat_ano, domar_weights)
  
  ordered_domar_weights[[i]] <- df_domar_weights$domar_weights[match(firms_ids, df_domar_weights$vat_ano)]
  
}

# save it
# save(ordered_domar_weights, file = paste0(proc_data, "/ordered_domar_weights_by_year.RData"))