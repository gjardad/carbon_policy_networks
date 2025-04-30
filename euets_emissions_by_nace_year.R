#### HEADER -------

## Creates time series of ETS emissions by NACE sector
# using installation-level NACE and firm-level NACE

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

load(paste0(proc_data, "/installation_year_emissions.RData"))
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

# Clean data -----

euets_emissions_by_nace_using_installation <- installation_year_emissions %>% 
  filter(country_id == "BE") %>%
  mutate(nace_id = ifelse(str_detect(nace_id, "^\\d\\..*"), 
                          paste0("0", nace_id),
                          nace_id), 
         nace2d = substr(nace_id, 1, 2)) %>% 
  group_by(nace2d, year) %>% 
  summarise(emissions = sum(verified, na.rm = T))

euets_emissions_by_nace_using_firms <- firm_year_belgian_euets %>% 
  mutate(nace5d = ifelse(nchar(nace5d) == 4, 
                         paste0("0", nace5d), 
                         nace5d),
           nace2d = substr(nace5d, 1, 2)) %>% 
  group_by(nace2d, year) %>% 
  summarise(emissions = sum(emissions, na.rm = T))

# save it
#save(euets_emissions_by_nace_using_firms, file = paste0(proc_data, "/euets_emissions_by_nace_using_firms.RData"))
#save(euets_emissions_by_nace_using_installation, file = paste0(proc_data, "/euets_emissions_by_nace_using_installation.RData"))




