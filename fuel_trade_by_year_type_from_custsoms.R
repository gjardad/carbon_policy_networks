#### HEADER -------

## This code creates measure of fossil fuel consumption by year and fuel type
# for Belgium from customs data

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

# Import data -------

load(paste0(proc_data,"/df_trade.RData"))
#load(paste0(proc_data,"/trade_random_sample.RData"))
load(paste0(proc_data,"/hs_codes_for_fossil_fuels.RData"))

# Calculate total imports and exports by fuel category ------------

fuel_trade <- df_trade %>% 
  mutate(cncode_6digit = substr(cncode, 1, 6)) %>% 
  left_join(hs_codes_for_fossil_fuels, by = c("cncode_6digit" = "hs_code")) %>%
  filter(!is.na(description)) %>% 
  group_by(year, cncode_6digit, flow) %>%
  summarise(
    total_cn_value  = sum(cn_value,  na.rm = TRUE),
    total_cn_weight = sum(cn_weight, na.rm = TRUE),
    total_cn_units  = sum(cn_units,  na.rm = TRUE),
    # description/state are constant within a 6-digit code per your note.
    # Take the first non-missing value in each group:
    description = dplyr::first(na.omit(description)),
    state       = dplyr::first(na.omit(state)),
    .groups = "drop"
  )
  



