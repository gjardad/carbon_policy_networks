#### HEADER -------

## Creates time series of emissions from energy consumption by NACE economic sector-year
# using PEFA to calculate shares of consumption within CRF, and using CRF data on consumption
# of fuels

# Obs: the data set still includes emissions from biomass and treats R29 fuel
# (from Eurostat data) as "biomass" instead of "other"

# Both things should be updated

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

load(paste0(proc_data, "/fuel_consumption_by_nace_year.RData")) # in TJ
load(paste0(proc_data, "/emissions_from_energy_consumption_by_crf_year.RData"))

crf_to_nace_crosswalk <- read_excel(paste0(raw_data, "/NIR/Annex-I-(Correspondence-between-CRF-NFR-NACE-Rev.-2)-to-Manual-for-Air-Emissions-Accounts-(2015-edition).xlsx"),
                                    sheet = "Correspondence-CRF-NFR-NACE")

fuel_classification <- read_csv(paste0(raw_data, "/ipcc_fuels_classification.csv"))[1:18,]

# Clean data -----

# map between CRF and NACE
crf_to_nace_crosswalk <- crf_to_nace_crosswalk[c(6:229),c(9,15)]
crf_to_nace_crosswalk <- crf_to_nace_crosswalk %>%
  rename(crf_code = 1, nace = 2) %>% 
  filter(nace != "-", crf_code != "-") %>%
  mutate(crf_code = case_when(
    crf_code %in% c("1.A.2.g.i.", "1.A.2.g.ii.", "1.A.2.g.iii.", "1.A.2.g.iv.",
                    "1.A.2.g.v.") ~ "1.A.2.g.", # CRF tables don't discriminate between those categories
    TRUE ~ crf_code
  ),
  nace = case_when(
    crf_code == "1.A.4.b." ~ "Household Total",
    crf_code == "1.A.4.b.i." ~ "Household Heating",
    crf_code == "1.A.4.b.ii." ~ "Household Transportation + Other",
    crf_code == "1.A.4.b.iii." ~ "Household Other",
    TRUE ~ nace)) %>% 
  mutate(nace = strsplit(as.character(nace), ", ")) %>%
  unnest(nace) %>% 
  filter(!nace %in% c("01-99", "Br", "CS")) %>% 
  mutate(nace = ifelse(nace == "C31_32", "C31_C32", nace)) %>% 
  distinct()

# filter out "super" categories (e.g. 1.A.1.c. since we have data on 1.A.1.c.i. - 1.A.1.c.iii.)
crf_to_nace_crosswalk <- crf_to_nace_crosswalk %>%
  filter(!crf_code %in% c("1.A.1.c.", "1.A.3.b.", "1.A.4.a.", "1.A.4.b.",
                          "1.A.4.c.", "1.B.1.a.", "2.", "2.B.", "2.B.4.", "2.C.", "2.E.", "2.G.2.", "5.B.", 	
                          "5.C.", "5.D."))

# fuel consumption by type, nace
fuel_consumption_by_type_nace_year <- fuel_consumption_by_nace_year %>% 
  left_join(fuel_classification, by = c("fuel" = "fuel_code")) %>% 
  group_by(nace, fuel_type, year) %>% 
  summarise(consumption = sum(consumption, na.rm = T)) %>% 
  filter(fuel_type != "biomass")
  # biomass emissions is not included in EUETS (see Annex IV in Directive 2003/87/EC)

# create df with crf_code of "parent" naces
parent_nace_crf_code <- crf_to_nace_crosswalk %>% 
  filter(nace %in% c("J", "K", "L", "M", "N", "Q", "R", "S")) %>% 
  mutate(parent_nace = substr(nace, 1, 1)) %>% 
  ungroup() %>%
  rename(parent_crf_code = crf_code) %>% 
  select(-nace)

  # E37-E39, and H49 - H51 are not assigned a energy-related crf category
  # however, PEFA says they do consume fuels
  # leave them out for now - i.e. their emissions from energy consumption will be zero

emissions_from_energy_by_nace_year <- fuel_consumption_by_type_nace_year %>%
  left_join(crf_to_nace_crosswalk, by = "nace") %>%
  mutate(parent_nace = substr(nace, 1, 1)) %>% 
  left_join(parent_nace_crf_code, by = "parent_nace") %>%
  mutate(crf_code = ifelse(is.na(crf_code), parent_crf_code, crf_code)) %>% 
  group_by(year, crf_code, fuel_type) %>%
  mutate(share = consumption / sum(consumption, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(emissions_from_energy_consumption, by = c("year", "crf_code", "fuel_type" = "fuel")) %>% 
  rename(crf_emissions = emissions) %>% 
  mutate(emissions = crf_emissions * share) %>% 
  filter(substr(crf_code, 1, 1) == "1" | nace == "E37-E39") %>% #E37-E39 excluded otwise 
  group_by(year, nace) %>% 
  summarise(emissions = sum(emissions, na.rm=T)) %>% 
  filter(!nace %in% c("ROW_ACT", "SD_SU", "NRG_FLOW"))
# it's in kt of CO2

# TO-DO: interpolate for years before 2008

# save it
# save(emissions_from_energy_by_nace_year, file = paste0(proc_data, "/emissions_from_energy_by_nace_year_using_shares_crf.RData"))



