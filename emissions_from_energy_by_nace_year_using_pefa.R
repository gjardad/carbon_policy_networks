#### HEADER -------

## Creates time series of emissions from energy consumption by NACE economic sector-year
# using fuel consumption from PEFA

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

crf_to_nace_crosswalk <- read_excel(paste0(raw_data, "/NIR/Annex-I-(Correspondence-between-CRF-NFR-NACE-Rev.-2)-to-Manual-for-Air-Emissions-Accounts-(2015-edition).xlsx"),
                                    sheet = "Correspondence-CRF-NFR-NACE")

load(paste0(proc_data, "/emission_factors_from_energy_consumption.RData"))

fuel_classification <- read_csv(paste0(raw_data, "/ipcc_fuels_classification.csv"))[1:18,]
# fuel classification follows table 1.1 in vol. 2, ch. 1 in IPCC 2006 guidelines

# Clean data ----

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
  distinct()

# filter out "super" categories (e.g. 1.A.1.c. since we have data on 1.A.1.c.i. - 1.A.1.c.iii.)
crf_to_nace_crosswalk <- crf_to_nace_crosswalk %>%
  filter(!crf_code %in% c("1.A.1.c.", "1.A.3.b.", "1.A.4.a.", "1.A.4.b.",
                          "1.A.4.c.", "1.B.1.a.", "2.", "2.B.", "2.B.4.", "2.C.", "2.E.", "2.G.2.", "5.B.", 	
                          "5.C.", "5.D."))

emission_factors_by_nace <- emission_factors_from_energy_consumption %>% 
  left_join(crf_to_nace_crosswalk, by = "crf_code") %>% 
  filter(!is.na(nace)) %>% 
  group_by(nace, year, fuel) %>% 
  summarise(mean_ef = mean(emission_factor, na.rm=T)) %>% 
  mutate(mean_ef = ifelse(fuel == "biomass", 0, mean_ef))
  # biomass emissions is not included in EUETS (see Annex IV in Directive 2003/87/EC)

# create df with mean_ef for parent naces
parent_nace_ef <- emission_factors_by_nace %>% 
  filter(nace %in% c("E36", "H52", "J", "K", "L", "M", "N", "Q", "R", "S")) %>% 
  mutate(parent_nace = substr(nace, 1, 1)) %>% 
  filter(!is.na(mean_ef)) %>% 
  rename(parent_ef = mean_ef) %>% 
  ungroup() %>% 
  select(-nace)

fuel_consumption_by_type_nace_year <- fuel_consumption_by_nace_year %>% 
  left_join(fuel_classification, by = c("fuel" = "fuel_code")) %>% 
  group_by(nace, fuel_type, year) %>% 
  summarise(consumption = sum(consumption, na.rm = T))

emissions_by_nace_year <- fuel_consumption_by_type_nace_year %>% 
  left_join(emission_factors_by_nace, by = c("nace", "year", "fuel_type" = "fuel")) %>% 
  filter(!is.na(fuel_type),
         !nace %in% c("A", "C", "CH_INV_PA", "E", "ENV", "G", "G-U_X_H",
                      "HH", "HH_HEAT", "HH_OTH", "HH_TRA", "NRG_FLOW", "ROW_ACT",
                      "SD_SU", "TOTAL")) %>% 
  mutate(parent_nace = substr(nace, 1, 1)) %>% 
  left_join(parent_nace_ef, by = c("parent_nace", "year", "fuel_type" = "fuel")) %>% 
  mutate(mean_ef = ifelse(!is.na(parent_ef) & is.na(mean_ef), parent_ef, mean_ef)) %>% 
  select(-c(parent_nace, parent_ef)) %>% 
  mutate(emissions = consumption * mean_ef/1000) %>% 
  group_by(nace, year) %>% 
  summarise(emissions = sum(emissions, na.rm=T)) %>% 
  filter(year >= 2008) # fuel consumption only available 2008 onwards

emissions_from_energy_by_nace_year <- emissions_by_nace_year

# save it
# save(emissions_from_energy_by_nace_year, file = paste0(proc_data, "/emissions_from_energy_by_nace_year_using_pefa.RData"))







  
