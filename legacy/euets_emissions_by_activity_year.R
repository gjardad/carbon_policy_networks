#### HEADER -------

## Creates time series of ETS emissions by activity

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

load(paste0(proc_data, "/installation_year_emissions.RData"))

activities_to_nace <- read_excel(paste0(raw_data, "/crf_to_activities_to_nace.xlsx"),
                                 sheet = "activities_to_nace")

# Clean data -----

euets_emissions_by_activity <- installation_year_emissions %>% 
  filter(country_id == "BE") %>%
  group_by(activity_id, year) %>% 
  summarise(emissions = sum(verified, na.rm = T))

# save it
save(euets_emissions_by_activity, file = paste0(proc_data, "/euets_emissions_by_activity.RData"))

