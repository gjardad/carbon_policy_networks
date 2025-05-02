#### HEADER -------

## Distributes non-covered emissions to non-covered firms 

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
load(paste0(proc_data, "/euets_emissions_by_nace_using_installation.RData"))
load(paste0(proc_data, "/agg_emissions_by_nace_year_using_shares_crf.RData"))
load(paste0(proc_data, "/firm_year_balance_sheet_selected_sample.RData"))

# Clean data -----

be_installations <- installation_year_emissions %>% 
  filter(country_id == "BE") %>% 
  mutate(nace_id = ifelse(str_detect(nace_id, "^[0-9]\\."),
                           paste0("0", nace_id),
                           nace_id))

nace_covered <- sort(unique(be_installations$nace_id))

# Fix aggregate emissions for C17, C18, and C19

  # OBS: As documented in plot_ratio_eutl_to_agg_by_sector_over_time.R aggregate emissions for C17-C18 and C19 are
  # smaller than EUETS emissions for those sectors across the entire time period,
  # and for sectors B and C16 agg emissions < EUETS emissions for the years before 2012.
  # This is probably due to some misaligned classification of emissions: emissions from B/C16/C17/C19 installations
  # in EUETS are allocated to some other sector in National Inventories.
  # As of now, strategy is to artificially bump up aggregate emissions in those sectors such that
  # aggregate emissions equals EUETS emissions

  # Other than sectors B, C16, C17, and C19, there are some cases for which agg < EUETS for which the values
  # are negligible. Also adjust such that agg = EUETS.

  # Additionally, split between C17 and C18 seems flawed. In particular, disaggregated final energy consumption data set
  # in Eurostat indicates that more than 90% of fuel consumption in sector C17-C18 is due to C17.

working_version_agg_emissions_by_nace_year <- agg_emissions_by_nace_year_using_shares_crf %>%
  left_join(euets_emissions_by_nace_using_installation, by = c("nace", "year")) %>% 
  mutate(emissions = emissions/10^3) %>% 
  mutate(agg_emissions = ifelse(nace %in% c("C17","C19"), emissions,
                                ifelse(nace == "C18", 0, agg_emissions))) %>%
  mutate(agg_emissions = ifelse(nace %in% c("B","C16") & year <= 2011, emissions, agg_emissions)) %>% 
  rename(euets_emissions = emissions) %>%
  mutate(agg_emissions = ifelse(agg_emissions < euets_emissions, euets_emissions, agg_emissions)) %>% 
  mutate(noneuets_emissions = agg_emissions - euets_emissions) %>% 
  mutate(across(everything(), ~replace_na(., 0)))















