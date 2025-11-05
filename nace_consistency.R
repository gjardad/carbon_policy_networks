#### HEADER -------

## This code checks consistency of NACE codes

  # 1. across installations within firm in EUETS.INFO
  # 2. across years within installation in EUETS.INFO
  # 3. across years within firm in annual accounts
  # 4. across data sets (EUETS.INFO and annual accounts) within firm

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

# Check consistency using EUETS.INFO -----

  # Import data
  load(file.path(proc_data,"/installation_year_emissions.RData"))
  df_account <- read.csv(paste0(raw_data,"/EUTL/Oct_2024_version/account.csv"))

  # are NACE codes consistent across years within installation?
  # YES because df below has no obs with nace_consistent == 0
  df <- installation_year_emissions %>% 
    group_by(installation_id) %>%
    mutate(
      nace_consistent = as.integer(length(unique(nace_id)) == 1)
    ) %>%
    ungroup()
  
  # are NACE codes consistent across installations within firm id?
  # NO. data set "inconsistent" below stores obs for which nace_id is not consistent
  df <- df_account %>% 
    rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
           firm_id = companyRegistrationNumber) %>% 
    select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
    filter(account_type %in% c("100-7","120-0"))
  
  installation_firm <- installation_year_emissions %>% 
    left_join(df %>% select(bvd_id, installation_id),
              by = "installation_id") %>% 
    distinct()
  
  nace_consistent_across_installations <- installation_firm %>% 
    group_by(bvd_id) %>%
    mutate(
      nace_consistent = as.integer(length(unique(nace_id)) == 1)
    ) %>%
    ungroup()
  
  nace_consistent_across_installations_be <- consistency %>% filter(country_id == "BE")

# Check consistency using annual accounts -----
  
  load(file.path(proc_data,"/firm_year_belgian_euets.RData"))
  
  # are NACE codes consistent across years within firm?
  # NO. The variable nace5d_consistent takes value of oldest (in time) valid value for nace5d within bvd_id
  
  nace_consistent_across_firms_be <- firm_year_belgian_euets %>% 
    group_by(bvd_id) %>%
    mutate(
      nace_consistent = as.integer(length(unique(nace5d)) == 1)
    ) %>%
    ungroup()    
  
  nace5d_summary_by_bvd_id <- firm_year_belgian_euets %>% 
    group_by(bvd_id) %>%
    summarise(
      n_valid = n_distinct(nace5d[!is.na(nace5d)]),
      any_valid = any(!is.na(nace5d)),
      single_valid = if (n_valid == 1 && any_valid) first(na.omit(nace5d)) else NA_character_,
      val_2005 = {
        v <- na.omit(nace5d[year == 2005])
        if (length(v) > 0) as.character(v[1]) else NA_character_
      },
      nace5d_consistent = dplyr::coalesce(single_valid, val_2005),
      .groups = "drop"
    ) #%>%
    #select(bvd_id, nace5d_consistent)
  
  