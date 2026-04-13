#### HEADER -------

## Compares emissions by sector from two vintages of EUTL data

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

# Import data ------

installation_old <- read.csv(paste0(raw_data,"/EUTL/May_2024_version/installation.csv"))
installation_new <- read.csv(paste0(raw_data,"/EUTL/Oct_2024_version/installation.csv"))

compliance_old <- read.csv(paste0(raw_data,"/EUTL/May_2024_version/compliance.csv"))
compliance_new <- read.csv(paste0(raw_data,"/EUTL/Oct_2024_version/compliance.csv"))

# Clean data

## Clean data -------

installation_old <- installation_old %>% 
  rename(installation_id = id) %>% 
  select(c(installation_id, country_id, activity_id, nace_id)) %>% 
  filter(nace_id != 51.00) # exclude avitation

installation_new <- installation_new %>% 
  rename(installation_id = id) %>% 
  select(c(installation_id, country_id, activity_id, nace_id)) %>% 
  filter(nace_id != 51.00) # exclude avitation

installation_year_emissions_old <- installation_old %>% 
  left_join(compliance_old, by = "installation_id") %>% 
  select(c(installation_id, year, country_id, activity_id, nace_id,
           allocatedFree, allocatedTotal, verified)) %>% 
  #filter(year <= 2023) %>% 
  mutate(allocatedFree = ifelse(is.na(allocatedFree),0, allocatedFree),
         allocatedTotal = ifelse(is.na(allocatedTotal),0, allocatedTotal),
         verified = ifelse(is.na(verified),0, verified)) %>% 
  # make activity_ids consistent
  # in the first two phases of EUETS installations were classified into 1-10 activity ids
  # in phase 3 onwards, installations were classified into 20-99
  # crosswalk between the two is in Table C1 in Abrell's documentation of EUETS.INFO
  mutate(activity_id = case_when(
    activity_id >= 1 & activity_id <= 5 ~ activity_id + 19,
    activity_id == 6 ~ 29, # ambiguous: activity 6 could become 29 or 30
    activity_id == 7 ~ 31,
    activity_id == 8 ~ 32,
    activity_id == 9 ~ 35, # ambiguous: activity 9 could become 35 or 36
    TRUE ~ activity_id  # Leave unchanged if it doesn't match any condition
  ))

installation_year_emissions_new <- installation_new %>% 
  left_join(compliance_new, by = "installation_id") %>% 
  select(c(installation_id, year, country_id, activity_id, nace_id,
           allocatedFree, allocatedTotal, verified)) %>% 
  #filter(year <= 2023) %>% 
  mutate(allocatedFree = ifelse(is.na(allocatedFree),0, allocatedFree),
         allocatedTotal = ifelse(is.na(allocatedTotal),0, allocatedTotal),
         verified = ifelse(is.na(verified),0, verified)) %>% 
  # make activity_ids consistent
  # in the first two phases of EUETS installations were classified into 1-10 activity ids
  # in phase 3 onwards, installations were classified into 20-99
  # crosswalk between the two is in Table C1 in Abrell's documentation of EUETS.INFO
  mutate(activity_id = case_when(
    activity_id >= 1 & activity_id <= 5 ~ activity_id + 19,
    activity_id == 6 ~ 29, # ambiguous: activity 6 could become 29 or 30
    activity_id == 7 ~ 31,
    activity_id == 8 ~ 32,
    activity_id == 9 ~ 35, # ambiguous: activity 9 could become 35 or 36
    TRUE ~ activity_id  # Leave unchanged if it doesn't match any condition
  ))

# Compare difference in emissions by sector -----

euets_emissions_by_sector_old <- installation_year_emissions_old %>%
  filter(country_id == "BE") %>%
  mutate(nace_id = ifelse(str_detect(nace_id, "^\\d\\..*"), 
                          paste0("0", nace_id),
                          nace_id), 
         nace2d = substr(nace_id, 1, 2)) %>% 
  group_by(nace2d, year) %>% 
  summarise(emissions = sum(verified, na.rm = T))

euets_emissions_by_sector_new <- installation_year_emissions_new %>% 
  filter(country_id == "BE") %>%
  mutate(nace_id = ifelse(str_detect(nace_id, "^\\d\\..*"), 
                          paste0("0", nace_id),
                          nace_id), 
         nace2d = substr(nace_id, 1, 2)) %>% 
  group_by(nace2d, year) %>% 
  summarise(emissions = sum(verified, na.rm = T))

