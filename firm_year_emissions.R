#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. emissions
# 2. BvD id
# 3. NACE code

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

df_installation <- load(paste0(proc_data,"/installation_year_emissions.RData"))

df_account <- read.csv(paste0(raw_data,"/EUTL/account.csv"))

# Clean data ------

df_account <- df_account %>% 
  rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
         firm_id = companyRegistrationNumber) %>% 
  select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
  filter(account_type %in% c("100-7","120-0"))

firm_year_emissions  <- installation_year_emissions %>% 
  left_join(df_account, by = "installation_id") %>% 
  group_by(firm_id, year) %>%
  summarise(
    allocated_free = sum(allocatedFree, na.rm = T),
    allocated_total = sum(allocatedTotal, na.rm = T),
    emissions = sum(verified, na.rm = TRUE),  # Total verified emissions
    pct_combustion = sum(verified[activity_id == 20], na.rm = TRUE),  # Emissions where activity_id == 20
    bvd_id = first(bvd_id),  # Include bvd_id (constant within each group)
    country_id = first(country_id)  # Include country_id (constant within each group)
    # obs: country_id and bvd_id are only inconsistent for two firms
  ) %>%
  ungroup() %>% 
  filter(firm_id != "") %>% 
  mutate(pct_combustion = ifelse(emissions == 0, 0, pct_combustion/emissions))

# Save it -------
save(firm_year_emissions, file = paste0(proc_data,"/firm_year_emissions.RData"))
