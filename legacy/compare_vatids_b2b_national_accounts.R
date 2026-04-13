#### HEADER -------

## Compare unique VAT ids in B2B and national accounts

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

# Import data ----

load(paste0(proc_data,"/df_national_accounts_with_5digits.RData"))

load(paste0(proc_data,"/df_b2b.RData"))

# Compute unique VAT ids for year 2012 ----

  b2b_2012 <- df_b2b %>% filter(year == 2012)

  national_accounts_2012 <- df_national_accounts %>% 
    filter(year == 2012)
  
  length(unique(b2b_2012$vat_i_ano))
  length(unique(b2b_2012$vat_j_ano))
  length(unique(national_accounts_2012$vat_ano))
  
  # in Trade and Domestic Production Networks they report 896,000 unique VAT ids in the year 2012
  # The reason for the discrepancy is the fact that the new vintage doesnt cover financial institutions and self-employed
  
  