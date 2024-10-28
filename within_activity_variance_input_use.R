#### HEADER -------

## This code checks for within-activity variance in input use

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

load(paste0(proc_data,"/firm_year_input_bundle_euets.RData"))

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

load(paste0(proc_data,"/firm_year_real_output_2005.RData"))

# Clean data set -----

  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
    filter(year >= 2005)
  
  # select expenditure on NACE codes that belong to
  # sections B, C, D
  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>%
    select(matches("^real_exp_(0[5-9]|[1-2][0-9]|3[0-5])"), c("vat", "year", "nace5d"))
  
  # are there nace5d codes for which everyone is zero?
  exp_col_sum <- colSums(firm_year_input_bundle_euets[sapply(firm_year_input_bundle_euets, is.numeric)])
  zero_sum_columns <- names(exp_col_sum)[which(exp_col_sum == 0)]
  firm_year_input_bundle_euets <- firm_year_input_bundle_euets[,setdiff(names(firm_year_input_bundle_euets), zero_sum_columns)]
  
  # add output
  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
    rename(vat_ano = vat) %>% 
    left_join(firm_year_real_output_2005 %>% select(vat_ano, year, real_output),
              by = c("vat_ano", "year"))
  
  # add emissions and activity_id
  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
    rename(vat = vat_ano) %>% 
    left_join(firm_year_belgian_euets %>% select(vat, year, emissions, activity_id),
              by = c("vat", "year"))
  
  # input expenditure relative to output
  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
    mutate(emission_intensity = emissions/real_output,
           across(starts_with("real_exp_"), 
                  ~ . / real_output, 
                  .names = "exp_share_{str_remove(.col, 'real_exp_')}")) %>% 
    select(vat, year, nace5d, emission_intensity, activity_id, starts_with("exp_share"))
  
  # clean data set
  df_regression <- firm_year_input_bundle_euets %>% 
    filter(!is.na(real_output) & !is.na(emissions) & emission_intensity > 0)
  
# Within activity variance in input use

    # inputs relevant for activity 20
  
    # inputs relevant for activity 21
  
    # inputs relevant for activity 24
  
    # inputs relevant for activity 25
  
    # inputs relevant for activity 28
  
    # inputs relevant for activity 29
  
    # inputs relevant for activity 30
  
    # inputs relevant for activity 32
  
    # inputs relevant for activity 33
  
    # inputs relevant for activity 34
    
    # inputs relevant for activity 36
    
    # inputs relevant for activity 37
    
    # inputs relevant for activity 38
    
    # inputs relevant for activity 42
    
    # inputs relevant for activity 43
  
  
  