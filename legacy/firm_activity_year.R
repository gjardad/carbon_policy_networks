#### HEADER -------

## This code creates data set at the firm-activity-year level with info on
# 1. pct of firm's emissions that come installations of the corresponding activity

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

load(paste0(proc_data,"/installation_year_emissions.RData"))

df_account <- read.csv(paste0(raw_data,"/EUTL/account.csv"))

# Clean data ------

df_account <- df_account %>% 
  rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
         firm_id = companyRegistrationNumber) %>% 
  select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
  filter(account_type %in% c("100-7","120-0"))

merged_data <- merge(installation_year_emissions, df_account, by = "installation_id")

firm_activity_year_emissions <- merged_data %>%
  group_by(firm_id, activity_id, year, country_id) %>%
  summarize(activity_emissions = sum(verified, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(firm_id != "")

firm_year_total_emissions <- firm_activity_year_emissions %>%
  group_by(firm_id, year, country_id) %>%
  summarize(total_emissions = sum(activity_emissions, na.rm = TRUE))

firm_activity_year_emissions <- firm_activity_year_emissions %>% 
  left_join(firm_year_total_emissions,
            by = c("firm_id", "year", "country_id")) %>% 
  mutate(share_emissions = ifelse(total_emissions == 0, 0, activity_emissions/total_emissions))

# Save it -------
save(firm_activity_year_emissions, file = paste0(proc_data,"/firm_activity_year_emissions.RData"))
