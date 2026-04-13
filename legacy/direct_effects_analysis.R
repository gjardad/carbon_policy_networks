##### HEADER -------

## This code creates figures showing
# 1. evolution of average allowance shortage within activity over time
# 2. evolution of average allowance shortage within sector over time
# 3. scatter plot Delta sales and allowance shortage in different time horizons
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

load(paste0(proc_data, "/df_direct_euets_effects.RData"))

# Clean data ------

  test <- df_direct_euets_effects %>% 
    filter(input_cost > 0)

  # why input cost missing?

  # why output missing?

  # what to do with firms for which labor cost is missing?

# Graphs


