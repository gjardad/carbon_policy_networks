#### HEADER -------

## This code creates data set at the firm-year level with info on
# nominal expenditure (in Euros) on each NACE5d code from domestic sources

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
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))
# NOTATION: i is the supplier, j is the buyer

#load(paste0(proc_data, "/b2b_sample.RData"))
#df_b2b <- b2b_sample

df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

load(paste0(proc_data,"/firm_year_nominal_output.RData"))

# Create nominal expenditure at nace5d level -----

  # include "0" in front of nace5d
  library(stringr)
  df_national_accounts <- df_national_accounts %>% 
    mutate(nace5d = str_pad(nace5d, width = 5, pad = "0"))
  
  # create nominal expenditure at firm-year-nace5d-level
  firm_year_nominal_domestic_input_bundle <- df_b2b %>%
    rename(vat_ano = vat_i_ano) %>% 
    left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
              by = c("vat_ano", "year")) %>% # find nace of supplier
    rename(vat_i_ano = vat_ano) %>% 
    group_by(vat_j_ano, year, nace5d) %>%
    summarize(expenditure = sum(corr_sales_ij, na.rm = TRUE)) %>%  # total expenditure on each nace code
    rename(nace5d_supplier = nace5d) %>% 
    left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
              by = c("vat_j_ano" = "vat_ano","year")) %>% # find nace of buyer
    rename(nace5d_buyer = nace5d) %>% 
    left_join(firm_year_nominal_output %>% select(vat_ano, year, output),
              by = c("vat_j_ano" = "vat_ano","year")) %>%
    filter(!is.na(nace5d_supplier) & !is.na(nace5d_buyer) & !is.na(output)) #%>%
    
    test <- firm_year_nominal_domestic_input_bundle_2005 %>% 
      group_by(vat_j_ano) %>% 
      mutate(total_expenditure = sum(expenditure, na.rm = TRUE),
           expenditure_share = expenditure/total_expenditure,
           .groups = 'drop')
  
  firm_year_nominal_domestic_input_bundle_2005 <- firm_year_nominal_domestic_input_bundle %>% 
    filter(year == 2005)
  
# save it
save(firm_year_nominal_domestic_input_bundle, file = paste0(proc_data,"/firm_year_nominal_domestic_input_bundle.RData"))
save(firm_year_nominal_domestic_input_bundle_2005, file = paste0(proc_data,"/firm_year_nominal_domestic_input_bundle_2005.RData"))