#### HEADER -------

## This code creates a df with I, X, and domestic consumption,
# and consumption for generation of energy in the industrial sector
# by fuel-year in in Belgium

# It also creates list of unique SIEC codes present in the energy balance in Belgium

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

  energy_balance <- read_csv(paste0(raw_data, "/Eurostat/energy_balance_by_fuel_year.csv")) %>%
    select(nrg_bal, siec, TIME_PERIOD, OBS_VALUE) %>% 
    rename(flow = nrg_bal, year = TIME_PERIOD, value = OBS_VALUE) %>%
    # exclude total, bioenergy, renewables, and waste
    filter(!substr(siec, 1, 2) %in% c("RA", "TO", "BI", "W6")) %>% 
    tidyr::separate(
      siec,
      into = c("siec_fullcode", "siec_name"),
      sep = ":", 
      remove = TRUE
    ) %>%
    mutate(
      siec_code = sub("^[A-Za-z]", "", siec_fullcode),
      siec_code = sub("_.*$", "", siec_code),
      value = value %>%
        gsub("\\.", "", .) %>%     # remove all dots
        as.numeric() / 1000        # correct decimal placement
    ) %>% 
  rename(energy_balance_value = value)
  # remember: this is in TJ

# List of unique SIEC codes covered in Eurostat energy balance for Belgium ------

unique_siec_eurostat_energy_balance_belgium <- unique(energy_balance$siec_code)

# Save it ------
save(energy_balance, file = paste0(proc_data,"/energy_balance_by_fuel_year.RData"))
save(unique_siec_eurostat_energy_balance_belgium, file = paste0(int_data,"/unique_siec_eurostat_energy_balance_belgium.RData"))
