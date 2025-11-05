#### HEADER -------

## This code creates data frame that stores all valid NACE codes
# for each Belgian bvd_id in EUETS.
# NACE codes come from the annual accounts and EUETS.INFO.
# The latter is originally recorder at the level of the installation

#####################

## Setup ------
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
library(dplyr)

# Import data ----

load(file.path(proc_data,"/installation_year_emissions.RData"))
df_account <- read.csv(paste0(raw_data,"/EUTL/Oct_2024_version/account.csv"))
load(file.path(proc_data,"/firm_year_belgian_euets.RData"))

# Create data set that contains all NACE codes for each bvd_id -----

  ## --- 1) From firm_year_belgian_euets: make nace_accounts_1,2,3,... ---
  accounts_wide <- firm_year_belgian_euets %>%
    select(bvd_id, nace5d) %>%
    distinct() %>%                      # keep unique (bvd_id, nace5d) pairs
    filter(!is.na(nace5d)) %>%          # only valid values
    arrange(bvd_id, nace5d) %>%         # ascending order within id
    group_by(bvd_id) %>%
    mutate(k = row_number()) %>%        # 1,2,3,... per id
    pivot_wider(
      names_from  = k,
      values_from = nace5d,
      names_prefix = "nace_accounts_"
    ) %>%
    ungroup()
  
  ## --- 2) From installation data: make nace_installation_1,2,3,... ---

  ## Match installations with firms ------
  df <- df_account %>% 
    rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
           firm_id = companyRegistrationNumber) %>% 
    select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
    filter(account_type %in% c("100-7","120-0"))
  
  matched_installation_firm <- installation_year_emissions %>% 
    left_join(df %>% select(bvd_id, installation_id),
              by = "installation_id") %>% 
    distinct()

  install_wide <- matched_installation_firm %>%
    filter(bvd_id != "", country_id == "BE") %>% 
    select(bvd_id, nace_id) %>%
    distinct() %>%                      # unique (bvd_id, nace_id)
    filter(!is.na(nace_id)) %>%
    arrange(bvd_id, nace_id) %>%
    group_by(bvd_id) %>%
    mutate(k = row_number()) %>%
    pivot_wider(
      names_from  = k,
      values_from = nace_id,
      names_prefix = "nace_installation_"
    ) %>%
    ungroup()
  
  ## --- 3) Final dataset: one row per bvd_id, with both sets of columns ---
  all_naces_for_each_firm <- full_join(accounts_wide, install_wide, by = "bvd_id") %>%
    arrange(bvd_id)

# save it -----
save(all_naces_for_each_firm, file = paste0(proc_data,"/all_naces_for_each_firm.RData")) 