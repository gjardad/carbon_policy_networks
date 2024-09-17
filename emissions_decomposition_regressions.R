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

load(paste0(proc_data,"/firm_year_input_bundle_euets.RData"))

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

# Clean data set -----

  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
    mutate(nace2d = str_sub(nace5d, 1, 2)) %>% 
    filter(year >= 2005)

  # create within-sector average expenditure in year 2005
  nace2d_averages_2005 <- firm_year_input_bundle_euets %>%
    filter(year == 2005) %>% # Filter for year 2005
    group_by(nace2d) %>%      # Group by nace2d
    summarize(across(starts_with("exp_"), mean, na.rm = TRUE)) %>% 
    rename_with(~ gsub("^exp_", "avg2005_exp_", .), starts_with("exp_"))
  
  firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
    left_join(nace2d_averages_2005, by = "nace2d") %>% 
    left_join(firm_year_belgian_euets %>% select(vat, year, emissions),
              by = c("vat", "year")) %>%
    mutate(across(starts_with("exp_"), 
                  .fns = ~ . - get(paste0("avg2005_", cur_column())), 
                  .names = "gap_{.col}")) %>% 
    select(vat, year, emissions, starts_with("gap"), starts_with("nace")) %>% 
    ungroup()

# Regresssion --------

  library(lfe)
  
  expenditure_vars <- firm_year_input_bundle_euets %>%
    select(starts_with("gap_")) %>%
    names()
  
  # Split the data by year
  data_by_year <- split(firm_year_input_bundle_euets, firm_year_input_bundle_euets$year)
  
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
  

  
