#### HEADER -------

## Compute emissions by NACE group from Annex XII

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

crf_to_nace_crosswalk <- read_excel(paste0(raw_data, "/NIR/Annex-I-(Correspondence-between-CRF-NFR-NACE-Rev.-2)-to-Manual-for-Air-Emissions-Accounts-(2015-edition).xlsx"),
                                    sheet = "Correspondence-CRF-NFR-NACE")

annex_xii <- read_excel(paste0(raw_data, "/NIR/BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx"))[15:73, -c(5)] %>% 
  rename(crf = 1, crf_emissions = 2, ets_emissions = 3, ratio = 4) %>% 
  mutate(crf_code = str_extract(crf, "^[^ ]+")) %>% 
  select(-crf) %>% 
  select(crf_code, crf_emissions, ets_emissions, ratio) %>% 
  filter(!is.na(crf_code), !crf_emissions %in% c("IE (2B10 other)", "NO"),
         crf_code != "Iron") %>% 
  mutate(crf_emissions = as.numeric(crf_emissions),
         ets_emissions = as.numeric(ets_emissions),
         ratio = as.numeric(ratio)) %>% 
  mutate(type = case_when(
    substr(crf_code, 1, 1) == "1" ~ "energy",
    substr(crf_code, 1, 1) == "2" ~ "ind_process",
    substr(crf_code, 1, 1) == "5" ~ "waste"
  ))

# Assign NACE sectors to Annex XII CRF categories -----

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

annex_xii_energy <- annex_xii %>%
  filter(type == "energy") %>% 
  mutate(crf_code = paste0(crf_code, ".")) %>% 
  left_join(crf_to_nace_crosswalk, by = "crf_code") %>%
  filter(!is.na(nace))

annex_xii_energy <- annex_xii_energy %>% 
  group_by(crf_code) %>%
  summarise(nace_list = paste(unique(nace), collapse = ","), .groups = 'drop') %>%
  left_join(annex_xii_energy, by = "crf_code") %>% 
  select(-c(nace, crf_code, ratio, type)) %>% 
  distinct() %>% 
  group_by(nace_list) %>% 
  summarise(crf_emissions = sum(crf_emissions, na.rm = T),
            ets_emissions = sum(ets_emissions, na.rm = T)) %>%
  mutate(nace_list = ifelse(nace_list == "D", "D35", nace_list)) %>% 
  filter(nace_list != "B,C19,C24,D")
# exclude this category because it makes it harder to compute emissions for D, C19, and C24,C25 separately

annex_xii_ind_process <- annex_xii %>%
  filter(type == "ind_process") %>% 
  mutate(crf_code = paste0(crf_code, ".")) %>% 
  left_join(crf_to_nace_crosswalk, by = "crf_code") %>%
  filter(!is.na(nace)) %>% 
  filter(crf_code %in% c("2.A.1.", "2.A.2.", "2.A.3.", "2.B.", "2.C.")) %>% 
  # only include "parent" sectors otherwise double counting
  select(-c(type,ratio, crf_code)) %>% 
  group_by(nace) %>% 
  summarise(crf_emissions = sum(crf_emissions, na.rm = T),
            ets_emissions = sum(ets_emissions, na.rm = T)) %>% 
  mutate(nace = case_when(
    nace == "C20" ~ "C20,C21",
    nace == "C24" ~ "C24,C25",
    TRUE ~ nace
  ))

# create version of annex_xii data that is comparable to other data sets
nace_sectors <- c("C10-C12", "C17,C18", "C19", "C20,C21", "C23", "C24,C25", "D35")
annex_xii_comparable <- data.frame(nace = nace_sectors) %>% 
  left_join(annex_xii_energy, by = c("nace" = "nace_list")) %>% 
  rename(crf_emissions_eng = 2, ets_emissions_eng = 3) %>% 
  left_join(annex_xii_ind_process, by = "nace") %>% 
  mutate(crf = ifelse(is.na(crf_emissions_eng), 0, crf_emissions_eng) +
           ifelse(is.na(crf_emissions), 0, crf_emissions),
         ets_emissions = ifelse(is.na(ets_emissions_eng), 0, ets_emissions_eng) +
           ifelse(is.na(ets_emissions), 0, ets_emissions)) %>% 
  select(-c(crf_emissions_eng, crf_emissions, ets_emissions_eng)) %>%
  rename(euets_crf = ets_emissions) %>% 
  mutate(ratio_crf = euets_crf/crf) %>% 
  mutate(nace = case_when(
    nace == "C17,C18" ~ "C17-C18",
    nace == "C20,C21" ~ "C20-C21",
    nace == "C24,C25" ~ "C24-C25",
    TRUE ~ nace
  ))

emissions_by_nace_annex_xii_24 <- annex_xii_comparable %>% 
  mutate(year = 2022)

# save it
# save(emissions_by_nace_annex_xii_24, file = paste0(proc_data, "/emissions_by_nace_group_annex_xii_2024.RData"))
