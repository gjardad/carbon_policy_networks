#### HEADER -------

## Compare sectoral emissions with Belgian national inventory

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

load(paste0(proc_data,"/firm_year_balance_sheet_selected_sample.RData"))
load(paste0(proc_data,"/firm_year_belgian_euets.RData"))
load(paste0(output,"/sector_year_emissions_euets_noneuets.RData"))
inventory <- read_excel(paste0(raw_data, "/BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx"))
crf_to_nace <- read_excel(paste0(raw_data, "/Annex-I-(Correspondence-between-CRF-NFR-NACE-Rev.-2)-to-Manual-for-Air-Emissions-Accounts-(2015-edition).xlsx"),
                          sheet = "Correspondence-CRF-NFR-NACE")

# Clean data -----

colnames(inventory) <- c("sector","inventory", "ets", "ratio", "comment")
inventory <- inventory[-c(1:14, 74:nrow(inventory)),-c(4:5)]
inventory <- inventory %>% 
  mutate(sector_name = str_remove(sector, "^[^ ]+ "),
         sector_code = str_extract(sector, "^[^ ]+"),
         across(c(2,3), ~ as.numeric(.x)/10^3)) %>% # emissions measured in mi of tons
  filter(!is.na(sector)) %>% 
  select(-sector) %>% 
  select(sector_name, sector_code, inventory, ets)

crf_to_nace <- crf_to_nace[, c(9,12,15,16)]
colnames(crf_to_nace) <- c("crf_before_2015", "crf_after_2015", "nace_rev2", "comment")
crf_to_nace <- crf_to_nace %>% 
  filter(!is.na(crf_before_2015), nace_rev2 != "-")
crf_to_nace <- crf_to_nace[-c(1),]


# Compare national accounts NACE codes with EUETS NACE codes (from EUETS.INFO)

