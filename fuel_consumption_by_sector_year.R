#### HEADER -------

## Creates time series for fossil fuel consumption by NACE sector in Belgium

# Unit of consumption is in TJ (Terajoule)

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

# Import data -----

fuel_consumption_by_sector_year <- read_csv(paste0(raw_data, "/Eurostat/fossil_fuel_consumption_by_sector_year.csv"))

# Clean data ------

fuel_consumption_by_sector_year <- fuel_consumption_by_sector_year %>% 
  select(4,5,6,9,10) %>% 
  rename(use = 1, nace = 2, fuel = 3, year = 4, consumption = 5)

save(fuel_consumption_by_sector_year, file = paste0(proc_data, "/fuel_consumption_by_sector_year.RData"))
  
