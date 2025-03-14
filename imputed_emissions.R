#### HEADER -------

## Creates data set for firms' imputed emissions using
# Eurostat sector-year-level data on emissions

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

  sector_emissions <- read_csv(paste0(raw_data,"/Eurostat/eurostat_emissions_by_sector_belgium.csv"))
  sector_emission_intensity <- read_csv(paste0(raw_data,"/Eurostat/eurostat_emission_intensity_by_sector_belgium.csv"))
  sector_output <- read_csv(paste0(raw_data,"/Eurostat/eurostat_output_basic_prices_by_sector_belgium.csv"))
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))

# Build data frame -----
  
  firm_year_belgian_euets <- firm_year_belgian_euets %>% filter(in_sample == 1)
  firm_year_belgian_euets$nace2d <- substr(firm_year_belgian_euets$nace5d, 1, 2)
  
  euets_firms_by_sector <- firm_year_belgian_euets %>% 
    # make nace codes compatible with eurostat's
    mutate(nace2d = case_when(
      nace2d == "10" | nace2d == "11" ~ "C10-C12", # no firms in sector 12
      nace2d == "13" | nace2d == "15" ~ "C13-C15", # no firms in sector 14
      nace2d %in% as.character(16:33) ~ paste0("C", nace2d),
      nace2d == "31" ~ "C31_C32", # no firms in sector 32
      nace2d == "35" ~ "D",
      nace2d == "38" ~ "E37-E39", # no firms in sector 37, 39
      nace2d == "42" | nace2d == "43" ~ "F",
      nace2d %in% as.character(45:47) ~ paste0("G", nace2d),
      nace2d %in% as.character(49:53) ~ paste0("H", nace2d),
      nace2d == "63" ~ "J62_J63", # no firms in sector 62
      nace2d == "68" ~ "L68A",
      nace2d == "70" ~ "M69_M70",
      nace2d == "71" ~ "M71",
      nace2d %in% as.character(80:82) ~ "N80-N82"
      )
    ) %>% 
    group_by(year,nace2d) %>% 
    summarize(
      euets_output = sum(revenue, na.rm = TRUE),     
      euets_emissions = sum(emissions, na.rm = TRUE),
      n_euets_firms = n(), 
      .groups = 'drop'
    )
  
  
  sector_emissions <- sector_emissions %>%
    select(c(4,5,8,9)) %>% 
    rename(gas = 1, nace2d = 2, year = 3, emissions = 4) %>% 
    mutate(gas = sub(":.*", "", gas),
           nace2d = sub(":.*", "", nace2d)) %>% 
    group_by(nace2d) %>%
    filter(nchar(nace2d) > 1 | nace2d %in% c("B", "D", "F", "I", "L", "O", "P"))
  
  sector_emission_intensity <- sector_emission_intensity %>% 
    select(c(4,5,9,10)) %>% 
    rename(gas = 1, nace2d = 2, year = 3, emission_intensity = 4) %>% 
    mutate(gas = sub(":.*", "", gas),
           nace2d = sub(":.*", "", nace2d)) %>% 
    group_by(nace2d) %>%
    filter(nchar(nace2d) > 1 | nace2d %in% c("B", "D", "F", "I", "L", "O", "P"))
  
  sector_output <- sector_output %>% 
    select(c(5,9,10)) %>% 
    rename(nace2d = 1, year = 2, output_eurostat = 3) %>% 
    mutate(nace2d = sub(":.*", "", nace2d))
  
  df_sectoral_emissions <- sector_emissions %>% 
    left_join(sector_emission_intensity, by = c("gas", "year", "nace2d")) %>%
    filter(gas == "CO2") %>%
    select(-gas) %>% 
    mutate(emission_intensity = emission_intensity/10^3, # express emission intensity in ton/EUR
           output = ifelse(emission_intensity == 0, NA,
                                   emissions/emission_intensity)) %>% 
    left_join(euets_firms_by_sector, by = c("nace2d", "year")) %>% 
    mutate(noneuets_emissions = emissions - euets_emissions,
           noneuets_output = output - euets_output,
           noneuets_emission_intensity = noneuets_emissions/noneuets_output,
           euets_emission_intensity = euets_emissions/euets_output) %>% 
    filter(!is.na(euets_output))
  
  # Obs: remember units!
  # emissions are in ton of CO2-eq
  # output is in EUR in current prices
  # emission intensity is in ton/EUR
  
  # save it
  save(df_sectoral_emissions, file = paste0(output,"/sector_year_emissions_euets_noneuets.RData"))
  
  
 
   
  
  
  
  





