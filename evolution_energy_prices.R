#### HEADER -------

## Graph of energy prices by country over time

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
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------

library(readxl)
carbon_shocks <- read_excel(paste0(raw_data,"/carbonPolicyShocks.xlsx"))

emission_price <- read_csv(paste0(raw_data,"/icap_price.csv")) %>% 
  select(1,3) %>% 
  slice(-1) %>% 
  rename(date = 1, price = 2) %>% 
  mutate(price = as.numeric(price))

load(paste0(proc_data,"/ippi_by_country_nace_month.RData"))

# Clean data ----

europe <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "DE", "EE", "EL", "ES", "FI",
            "FR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO",
            "SK", "SE", "SI")

energy_prices <- ippi %>% 
  filter(sector == "D35" & country %in% europe) %>% 
  select(-c(sector))
  

