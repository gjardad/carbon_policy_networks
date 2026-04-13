#### HEADER -------

## This code creates df that identifies the fuels used in activities
# that generate GHG from stationary industrial installations

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import and clean data --------

load(paste0(proc_data,"/energy_balance_by_fuel_year.RData"))

energy_balance_filtered <- energy_balance %>%
  filter(str_starts(flow, "FC_IND_E") | str_starts(flow, "TI_E")) %>% 
  filter(energy_balance_value > 0) %>%
  # exclude SIEC codes that represent umbrella categories or
  # fuels that are not used to generate GHG in stationary industrial installations
  filter(!siec_fullcode %in% c("E7000", "H8000", "FE", "C0000X0350-0370",
                        "C0350-0370", "P1000", "O4000XBIO", "R5110-5150_W6000RI",
                        "R5210B", "R5210P", "R5220B", "R5220P", "R5230P",
                        "R5230B", "R5290", "R5300"))

unique(energy_balance_filtered$siec_fullcode)

pefa <- read_csv(paste0(raw_data,"/Eurostat/pefa_emission_relevant_use_by_fuel_nace_year.csv")) %>% 
  rename(flow = stk_flow, year = TIME_PERIOD, nace_code = nace_r2,
         fuel = prod_nrg, pefa_value = OBS_VALUE) %>% 
  select(flow, year, nace_code, fuel, pefa_value)

pefa_filtered <- pefa %>% 
  filter(!is.na(year) & !is.na(fuel) & !is.na(pefa_value)) %>% 
  filter(pefa_value > 0) %>% 
  #only include NACE codes for industries
  filter(!nace_code %in% c("NRG_FLOW:Energy flows over all activities",
                           "TOTAL:Total - all NACE activities",
                           "ENV:Environment")) %>% 
  # only include product flows 
  filter(startsWith(fuel, "P"))
  
