#### HEADER -------

## Code that generates table with coverage of selected sample

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
library(readxl)

# Import data ----

  load(paste0(proc_data,"/firm_year_balance_sheet_selected_sample.RData"))
  load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
  load(paste0(proc_data,"/df_belgian_national_agg.RData"))
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))

# Get agg. numbers from firms in sample ----
  
  total_selected_sample <- firm_year_balance_sheet_selected_sample %>% 
    group_by(year) %>% 
    summarise(
      total_turnover = sum(turnover, na.rm = TRUE),
      total_value_added = sum(value_added, na.rm = TRUE),
      total_wage_bill = sum(wage_bill, na.rm = T),
      total_sales = sum(total_sales, na.rm = T),
      total_network_sales = sum(network_sales, na.rm = T),
      total_imports = sum(imports, na.rm = T),
      total_exports = sum(exports, na.rm = T),
      unique_vat_ano_count = n_distinct(vat_ano)
    ) %>% 
    filter(year >= 2002)
  
  # number of firms in EUETS
  
  n_firms_euets <- c(NA)
    
  for(y in c(2012,2022)){
    
    i <- y - 2004
    
    euets_firms <- firm_year_belgian_euets %>%
      filter(year == y) %>% 
      select(vat, emissions)

    # vat of firms in B2B + annual accounts sample
    firms <- vats_as_ordered_in_io_matrix[[i]]
    index_of_euets_firms <- match(euets_firms$vat, firms)
    valid_indices <- na.omit(index_of_euets_firms)
    
    n_firms_euets <- c(n_firms_euets, length(valid_indices))
  }  

# Build table -----
  
  data_for_table <- bind_cols(df_belgian_national_agg, total_selected_sample %>%
                                filter(year %in% c(2002,2012,2022)) %>% select(-year),
                              n_firms_euets)
  
  data_for_table <- data_for_table %>% 
    mutate(
      across(10:16, ~ round(as.numeric(.x) / 10^9))
    )
  
  115/212
  

