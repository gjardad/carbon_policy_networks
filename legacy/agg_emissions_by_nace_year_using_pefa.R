#### HEADER -------

## Creates time series of agg. emissions by NACE sector using shares of CRF

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

load(paste0(proc_data, "/emissions_from_energy_by_nace_year_using_pefa.RData"))
load(paste0(proc_data, "/emissions_from_ind_process_by_nace_year.RData"))

# Clean data -----

agg_emissions_by_nace_year_using_pefa <- emissions_from_energy_by_nace_year %>%
  rename(emissions_from_energy = emissions) %>% 
  left_join(emissions_from_ind_process_by_nace_year, by = c("year", "nace")) %>% 
  mutate(agg_emissions = replace_na(emissions_from_energy, 0) + replace_na(emissions, 0)) %>% 
  select(-c(emissions_from_energy, emissions)) %>% 
  mutate(nace = ifelse(nace == "D", "D35", nace)) # make it comparable to others datasets

# save it
# save(agg_emissions_by_nace_year_using_pefa, file = paste0(proc_data, "/agg_emissions_by_nace_year_using_pefa.RData"))

