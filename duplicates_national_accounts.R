#### HEADER -------

## This code eliminates duplicates within VAT-year in national accounts

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

library(haven)
df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

  # address duplicates in df_national_accounts
  duplicates <- df_national_accounts %>%
    group_by(vat_ano, year) %>%
    filter(n() > 1) %>%
    ungroup()
  
  duplicate_vat <- duplicates[1,1]
  
  # choose the one with largest turnover
  no_duplicates <- df_national_accounts %>%
    filter(vat_ano != "6F5574B5EF9F3B0EE321A7A0A22DD936478D5C6F3C47C0B8D88A729C4C2C6B66" |
             vat_ano == "6F5574B5EF9F3B0EE321A7A0A22DD936478D5C6F3C47C0B8D88A729C4C2C6B66" & year > 2002)
  
  fix_duplicates <- duplicates %>% 
    group_by(year) %>%
    filter(v_0000070 == max(v_0000070, na.rm = TRUE)) %>%
    slice(1) %>%
    ungroup()
  
  df_national_accounts <- rbind(no_duplicates, fix_duplicates)

# save it ------
  save(df_national_accounts, file = paste0(proc_data,"/national_accounts_no_duplicates.RData"))
  