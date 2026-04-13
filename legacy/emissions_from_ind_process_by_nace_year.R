#### HEADER -------

## Creates time series of emissions frum industrial process by NACE economic sector-year

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

crf_to_nace_crosswalk <- read_excel(paste0(raw_data, "/NIR/Annex-I-(Correspondence-between-CRF-NFR-NACE-Rev.-2)-to-Manual-for-Air-Emissions-Accounts-(2015-edition).xlsx"),
                                    sheet = "Correspondence-CRF-NFR-NACE")

load(paste0(proc_data, "/value_added_by_sector_year.RData"))

load(paste0(proc_data, "/emissions_from_ind_process_crf.RData"))

# Clean data -----

# map between CRF and NACE
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
                          "1.A.4.c.", "1.B.1.a.", "2.", "2.B.", "2.B.4.", "2.B.8.",
                          "2.C.", "2.E.", "2.G.2.", "5.B.", 	
                          "5.C.", "5.D."))

emissions_from_ind_process_by_nace_year <- emissions_from_ind_process %>% 
  left_join(crf_to_nace_crosswalk, by = "crf_code") %>% 
  filter(!is.na(nace)) %>% 
  left_join(value_added_by_sector_year, by = c("nace", "year")) %>%
  group_by(year, crf_code) %>%
  mutate(share = value_added / sum(value_added, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(!nace %in% c("Household", "O", "Household O")) %>%
  mutate(crf_emissions = emissions,
         emissions = crf_emissions * share) %>% 
  group_by(year, nace) %>% 
  summarise(emissions = sum(emissions, na.rm=T))
# it's in kt of CO2

# save it
# save(emissions_from_ind_process_by_nace_year, file = paste0(proc_data, "/emissions_from_ind_process_by_nace_year.RData"))





