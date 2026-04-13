#### HEADER -------

## Calculate emissions from energy use using Statbel series on fuel consumption

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

# Import data ----------

energy_consumption <- read_csv(paste0(raw_data, "/statbel_energy_consumption.csv"))

# Clean data -------

test <- energy_consumption %>%
  select(-c(1,5,7)) %>% 
  rename(category = 1, sector_group = 2, sector = 3, year = 4, product_group = 5, product = 6, consumption = 7) %>% 
  filter(category == "Final consumption", !is.na(product)) %>% 
  select(-c(category, sector_group)) %>% 
  mutate(consumption = ifelse(is.na(consumption), 0, consumption*41.868)) # %>%  # report in TJ (originally in ktoe)
  #left_join(emission_factors, by = "product") %>% 
  #mutate(emissions = consumption * ef)


load(paste0(proc_data,"/installation_year_emissions.RData"))

be <- installation_year_emissions %>% filter(country_id == "BE")

be_total <- be %>% group_by(year) %>% summarise(total_emissions = sum(verified, na.rm = T))
