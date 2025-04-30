#### HEADER -------

## Creates time series of ETS emissions by NACE sector
# using installation-level NACE and firm-level NACE

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
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

# Clean data -----

euets_emissions_by_nace_using_installation <- installation_year_emissions %>% 
  filter(country_id == "BE") %>%
  mutate(nace_id = ifelse(str_detect(nace_id, "^\\d\\..*"), 
                          paste0("0", nace_id),
                          nace_id), 
         nace2d = substr(nace_id, 1, 2)) %>% 
  group_by(nace2d, year) %>% 
  summarise(emissions = sum(verified, na.rm = T))

euets_emissions_by_nace_using_firms <- firm_year_belgian_euets %>% 
  mutate(nace5d = ifelse(nchar(nace5d) == 4, 
                         paste0("0", nace5d), 
                         nace5d),
           nace2d = substr(nace5d, 1, 2)) %>% 
  group_by(nace2d, year) %>% 
  summarise(emissions = sum(emissions, na.rm = T))

# Make NACE sectors comparable ------

euets_emissions_by_nace_using_firms <- euets_emissions_by_nace_using_firms %>% 
  rename(nace = nace2d) %>% 
  mutate(nace = case_when(
    nace %in% c("07", "08") ~ "B",                 
    nace %in% c("10", "11", "12") ~ "C10-C12",    
    nace %in% c("13", "14", "15") ~ "C13-C15",    
    nace %in% c("16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "33") ~ paste0("C", nace),  
    nace == "35" ~ "D35",
    nace == "36" ~ "E36",
    nace %in% c("37", "38", "39") ~ "E37-E39",
    nace %in% c("41", "42", "43") ~ "F",
    nace %in% c("45", "46", "47") ~ paste0("G", nace), 
    nace %in% c("49", "50", "51", "52", "53") ~ paste0("H", nace),
    nace == "63" ~ "J62_J63", # no firms classified as 62,
    nace == "64" ~ "K64",
    nace == "68" ~ "L68A",
    nace == "70" ~"M69_M70",
    nace %in% c("71", "72") ~ paste0("M", nace),
    nace %in% c("81", "82") ~ "N80-N82",
    nace == "84" ~ "O",
    nace == "86" ~"Q86",
    TRUE ~ nace                                   
  )) %>%
  group_by(nace, year) %>%                        
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")  # Summarise emissions

euets_emissions_by_nace_using_installation <- euets_emissions_by_nace_using_installation %>% 
  rename(nace = nace2d) %>% 
  mutate(nace = case_when(
    nace %in% c("07", "08") ~ "B",                 
    nace %in% c("10", "11", "12") ~ "C10-C12",    
    nace %in% c("13", "14", "15") ~ "C13-C15",    
    nace %in% c("16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "33") ~ paste0("C", nace),  
    nace == "35" ~ "D35",
    nace == "36" ~ "E36",
    nace %in% c("37", "38", "39") ~ "E37-E39",
    nace %in% c("41", "42", "43") ~ "F",
    nace %in% c("45", "46", "47") ~ paste0("G", nace), 
    nace %in% c("49", "50", "51", "52", "53") ~ paste0("H", nace),
    nace == "63" ~ "J62_J63", # no firms classified as 62,
    nace == "64" ~ "K64",
    nace == "68" ~ "L68A",
    nace == "70" ~"M69_M70",
    nace %in% c("71", "72") ~ paste0("M", nace),
    nace %in% c("81", "82") ~ "N80-N82",
    nace == "84" ~ "O",
    nace == "86" ~"Q86",
    TRUE ~ nace                                   
  )) %>%
  group_by(nace, year) %>%                        
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")  # Summarise emissions

# save it
save(euets_emissions_by_nace_using_firms, file = paste0(proc_data, "/euets_emissions_by_nace_using_firms.RData"))
save(euets_emissions_by_nace_using_installation, file = paste0(proc_data, "/euets_emissions_by_nace_using_installation.RData"))




