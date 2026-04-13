#### HEADER -------

## Imputes emissions to non-EUETS firms proportionally to firm size

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
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
nace_number_to_letter <- read_csv(paste0(raw_data, "/nace_codes_number_to_letter.csv"))

# NACE codes covered by EUETS in Europe -----

nace4d_covered_by_euets <- sort(unique(installation_year_emissions$nace_id))
nace2d_covered_by_euets <- unique(substr(nace4d_covered_by_euets, 1, 2))

# Fix aggregate emissions -------

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

# Augment balance sheet data with emissions ------

  firm_year_balance_sheet_and_emissions_using_firm_size <- firm_year_balance_sheet_selected_sample %>%
  select(vat_ano, year, turnover, value_added, total_sales, nace5d) %>%
  left_join(firm_year_belgian_euets %>% select(vat, year, emissions), by = c("vat_ano" = "vat", "year")) %>%
  rename(obs_emissions = emissions) %>% 
  mutate(obs_emissions = obs_emissions/10^3,
         nace2d = substr(nace5d, 1, 2),
         nace4d = paste0(substr(nace5d, 1, 2), ".", substr(nace5d, 3, 4)),
         imputation_group = case_when(
           !is.na(obs_emissions) ~ 1,
           is.na(obs_emissions) & nace4d %in% nace4d_covered_by_euets ~ 2,
           is.na(obs_emissions) & !nace4d %in% nace4d_covered_by_euets & nace2d %in% nace2d_covered_by_euets ~ 3,
           is.na(obs_emissions) & !nace2d %in% nace2d_covered_by_euets ~ 4,
           TRUE ~ NA_real_)) %>%
  group_by(year, nace2d) %>%
  mutate(turnover_share = case_when(
    imputation_group %in% c(1,3) ~ 0,
    imputation_group %in% c(2,4) ~ turnover / sum(turnover[imputation_group %in% c(2,4)], na.rm = TRUE))) %>%
  ungroup() %>% 
  left_join(nace_number_to_letter, by = "nace2d") %>% 
  mutate(nace = A64_code) %>% 
  left_join(working_version_agg_emissions_by_nace_year, by = c("nace", "year")) %>% 
  mutate(imputed_emissions = ifelse(is.na(obs_emissions), turnover_share * noneuets_emissions, 0)) %>% 
  mutate(emissions = replace_na(obs_emissions, 0) + imputed_emissions) %>% 
  mutate(emissions = emissions*10^3)
  # emissions in ton of CO2

firm_year_obs_and_imputed_emissions_using_firm_size <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
  select(vat_ano, year, turnover, nace5d, obs_emissions, imputed_emissions, emissions) %>% 
  filter(year >= 2008)

# save it
#save(firm_year_obs_and_imputed_emissions_using_firm_size, file = paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))
#save(firm_year_balance_sheet_and_emissions_using_firm_size, file = paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))














