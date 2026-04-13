#### HEADER -------

## Compare ETS emissions with Belgian national inventory

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

crf_to_activities_crosswalk <- read_excel(paste0(raw_data, "/NIR/crf_to_ets_activities.xlsx")) %>%
  mutate(crf_code = sub(" .*", "", crf_category))

inventory <- read_excel(paste0(raw_data, "/NIR/BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx"))

load(paste0(proc_data, "/installation_year_emissions.RData"))

# Clean data -----

be_installations_2022 <- installation_year_emissions %>% 
  filter(country_id == "BE", year == 2022) %>% 
  group_by(activity_id, year) %>% 
  summarise(emissions = sum(verified)) %>% 
  mutate(activity_id = as.character((activity_id)),
         emissions = emissions/10^6) %>% # emissions measured in mi of tons
  ungroup()

# split installations with activity id 20 into multiple categories based on NACE
be_installations_2022_act20 <- installation_year_emissions %>% 
  filter(country_id == "BE", year == 2022, activity_id == 20) %>% 
  mutate(nace_id = ifelse(str_detect(nace_id, "^[0-9]{1}\\."), 
                          str_replace(nace_id, "^", "0"),
                          nace_id), # add zero to the left
         nace2d = substr(nace_id, 1, 2)) %>% 
  group_by(nace2d) %>% 
  summarise(emissions = sum(verified)) %>% 
  mutate(activity_id = "20",
         emissions = emissions/10^6) %>% # emissions measured in mi of tons
  ungroup()


crf_expanded <- crf_to_activities_crosswalk %>%
  mutate(activities = strsplit(as.character(activity), ", ")) %>%
  unnest(activities) %>%
  left_join(be_installations_2022, by = c("activities" = "activity_id")) %>% 
  rename(ets_emissions = emissions)

colnames(inventory) <- c("crf_category","inventory_emissions", "ets_emissions_in_inventory", "ratio", "comment")
inventory <- inventory[-c(1:14, 74:nrow(inventory)),-c(4:5)]
inventory <- inventory %>% 
  mutate(crf_name = str_remove(crf_category, "^[^ ]+ "),
         crf_code = str_extract(crf_category, "^[^ ]+"),
         across(c(2,3), ~ as.numeric(.x)/10^3)) %>% # emissions measured in mi of tons
  filter(!is.na(crf_category)) %>% 
  select(-crf_category) %>% 
  select(crf_name, crf_code, inventory_emissions, ets_emissions_in_inventory) %>% 
  mutate(crf_code = paste0(crf_code, "."))

  # Emissions by CRF category
  crf_to_activities_emissions <- crf_expanded %>% 
    left_join(inventory %>% select(crf_code, inventory_emissions, ets_emissions_in_inventory),
              by = "crf_code")


