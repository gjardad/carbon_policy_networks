#### HEADER -------

## This code runs regressions that decomposes firm-level emissions into input bundle
# More specificaly, it
#1. creates data set to run the regressions
#2. produces the regressions
#3. stores the results of the regressions

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

load(paste0(proc_data,"/firm_year_domestic_input_bundle_euets.RData"))

load(paste0(proc_data,"/firm_year_imports_value_euets.RData"))

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

load(paste0(proc_data,"/firm_year_real_output_2005.RData"))

# Clean data set -----

  firm_year_domestic_input_bundle_euets <- firm_year_domestic_input_bundle_euets %>% 
    filter(year >= 2005)

  # select expenditure on NACE codes that belong to
  # sections B, C, D
  #firm_year_domestic_input_bundle_euets <- firm_year_domestic_input_bundle_euets %>%
  #  select(matches("^real_exp_(0[5-9]|[1-2][0-9]|3[0-5])"), c("vat", "year", "nace5d"))

  # are there nace5d codes for which everyone is zero?
  #exp_col_sum <- colSums(firm_year_domestic_input_bundle_euets[sapply(firm_year_domestic_input_bundle_euets, is.numeric)])
  #zero_sum_columns <- names(exp_col_sum)[which(exp_col_sum == 0)]
  #firm_year_domestic_input_bundle_euets <- firm_year_domestic_input_bundle_euets[,setdiff(names(firm_year_domestic_input_bundle_euets), zero_sum_columns)]
  
  # add output
  firm_year_inputs_euets <- firm_year_domestic_input_bundle_euets %>% 
    rename(vat_ano = vat) %>% 
    left_join(firm_year_real_output_2005 %>% select(vat_ano, year, real_output),
              by = c("vat_ano", "year"))
  
  # add imports
  firm_year_inputs_euets <- firm_year_inputs_euets %>% 
    left_join(firm_year_imports_value_euets, by = c("vat_ano", "year"))
  
  # do all euets firms have imports data?
  setdiff(unique(firm_year_domestic_input_bundle_euets$vat), unique(firm_year_imports_value_euets$vat_ano))
  
  # make imports = 0 for firms wout imports data
  firm_year_inputs_euets <- firm_year_inputs_euets %>% 
    mutate(across(starts_with("real_exp_"), 
                  ~ ifelse(is.na(.), 0, .)))
  
  # add emissions and activity_id
  firm_year_inputs_euets <- firm_year_inputs_euets %>% 
    rename(vat = vat_ano) %>% 
    left_join(firm_year_belgian_euets %>% select(vat, year, emissions, activity_id),
              by = c("vat", "year"))
  
  # input expenditure relative to output
  firm_year_inputs_euets <- firm_year_inputs_euets %>% 
    mutate(emission_intensity = emissions/real_output,
           across(starts_with("real_exp_"), 
                  ~ . / real_output, 
                  .names = "exp_share_{str_remove(.col, 'real_exp_')}")) %>% 
    select(vat, year, nace5d, emission_intensity, activity_id, starts_with("exp_share"), real_output)
  
  # clean data set
  df_regression <- firm_year_inputs_euets %>% 
    filter(!is.na(real_output) & !is.na(emission_intensity) & emission_intensity > 0)

# Regresssion --------
  
  model <- lm(emission_intensity ~ activity_id, data = df_regression)
  
  expenditure_vars <- df_regression %>%
    select(starts_with("exp_")) %>%
    names()
  
  formula <- as.formula(paste("emission_intensity ~ activity_id + ", paste(expenditure_vars, collapse = " + ")))
  
  model2 <- lm(formula, data = df_regression)

  library(lfe)
  
  expenditure_vars <- firm_year_domestic_input_bundle_euets %>%
    select(starts_with("gap_")) %>%
    names()
  
  # Split the data by year
  data_by_year <- split(firm_year_domestic_input_bundle_euets, firm_year_domestic_input_bundle_euets$year)
  
  # Create a list to store models
  models <- list()
  
  # Loop through each year and run the regression
  for (year in names(data_by_year)) {
    # Subset data for the current year
    df_year <- data_by_year[[year]]
    
      # Create the formula for the current year's data
      options(expressions = 50000)
      formula <- as.formula(paste("emissions ~", paste(expenditure_vars, collapse = " + "), "| nace2d"))
      
      # Run the regression
      model <- feols(formula, data = df_year)
    
    # Store the model in the list
    models[[year]] <- model
  }
  

  
