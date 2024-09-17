#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. expenditure (in Euros) on each NACE code

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

df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

load(paste0(proc_data,"/nace_hicp_data.RData"))

load(paste0(proc_data,"/ppi_final.RData"))

library(readxl)
hicp_allitems <- read_excel(paste0(raw_data,"/Eurostat/eurostat_hicp_coicop_annual_belgium_CP00_CP063.xlsx"),
                            sheet = "Sheet 1",
                            range = "B11:AW11", col_names = FALSE)

df_belgium_vat <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

# Clean data -------

  # include "0" in front of nace5d
  library(stringr)
  df_national_accounts <- df_national_accounts %>% 
    mutate(nace5d = str_pad(nace5d, width = 5, pad = "0"))

  # create nominal expenditure at firm-year-nace5d-level
  long <- df_b2b %>%
    rename(vat_ano = vat_i_ano) %>% 
    left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
              by = c("vat_ano", "year")) %>% # find nace of supplier
    rename(vat_i_ano = vat_ano) %>% 
    group_by(vat_j_ano, year, nace5d) %>%
    summarize(expenditure = sum(corr_sales_ij, na.rm = TRUE)) %>% # total expenditure on each nace code
    mutate(nace4d = substr(nace5d,1,4))

  # create nace4d-year price indices
  nace_hicp_data <- nace_hicp_data %>% 
    rename(nace4d = nace) %>%
    mutate(nace2d = str_sub(nace4d, 1, 2),
           nace1d = str_sub(nace4d, 1, 1))
  
  avg_price_index_nace2d <- nace_hicp_data %>% 
    group_by(nace2d, year) %>% 
    summarise(avg_price_index_nace2d = mean(price_index, na.rm = TRUE), .groups = 'drop')

  avg_price_index_nace1d <- nace_hicp_data %>% 
    group_by(nace1d, year) %>% 
    summarise(avg_price_index_nace1d = mean(price_index, na.rm = TRUE), .groups = 'drop')
  
  # deflate nace5d-year-level expenditure using nace4d-year price index
  
    # merge based on nace4d
    long <- long %>% 
      mutate(nace2d = str_sub(nace4d, 1, 2),
             nace1d = str_sub(nace4d, 1, 1)) %>% 
      left_join(nace_hicp_data %>% select(nace4d, year, price_index), by = c("nace4d", "year"))
  
    # now based on nace2d
    long <- long %>% 
      left_join(avg_price_index_nace2d, by = c("nace2d", "year"))
    
    # now based on nace1d
    long <- long %>% 
      left_join(avg_price_index_nace1d, by = c("nace1d", "year"))
    
    # fill price_index values which are missing
    long <- long %>% 
      mutate(price_index = ifelse(is.na(price_index) & !is.na(avg_price_index_nace2d),
                                  avg_price_index_nace2d,
                                  ifelse(is.na(price_index) & !is.na(avg_price_index_nace1d),
                                         avg_price_index_nace1d,
                                         price_index))) %>%
      
      select(vat_j_ano, nace5d, year, expenditure, price_index)
    
    # for all nace codes which are still missing price index, use all-items HICP
    
      # create df and normalize 2010 = 100
      years <- seq(2000, 2023)
      hicp_allitems <- hicp_allitems[, seq(1, ncol(hicp_allitems), by = 2)]
      hicp_allitems <- data.frame(
        code = "CP00",
        year = years,
        hicp_allitems = as.numeric(hicp_allitems)
      ) %>% 
        mutate(hicp_allitems = hicp_allitems / hicp_allitems[year == 2010] * 100) %>% 
        select(-c(code))
    
      # merge it with long
      long <- long %>% 
        left_join(hicp_allitems, by = "year") %>% 
        mutate(price_index = ifelse(is.na(price_index),
                                    hicp_allitems, price_index)) %>% 
        select(vat_j_ano, nace5d, year, expenditure, price_index)
      
  # real expenditure at nace5d-year level deflating using nace4d-year index
  
    long <- long %>% 
      mutate(expenditure = expenditure/price_index*100)
    
# Create wide format data -----
    
  long <- long %>% 
    select(-c(price_index)) 
  # otwise pivot_wider also considers different price_indexes when creating the unique obs
    
  # input bundle only for EUETS treated firms
    
    df_belgium_vat <- df_belgium_vat %>% 
      rename(vat_j_ano = vat_ano)
    
    long_euts <- long %>% 
      left_join(df_belgium_vat %>% select(vat_j_ano, id),
                by = "vat_j_ano") %>% 
      filter(!is.na(id))
    
    firm_year_input_bundle_euets <- long_euets %>% 
      pivot_wider(names_from = nace5d, 
                  values_from = c(expenditure),
                  values_fill = list(expenditure = 0),
                  names_prefix = "exp_") %>% 
      rename(vat_ano = vat_j_ano) %>% 
      left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
                by = c("vat_ano", "year")) %>% # find nace of buyer
      rename(vat = vat_ano)
    
    save(firm_year_input_bundle_euets, file = paste0(proc_data,"/firm_year_input_bundle_euets.RData"))
    
  # input bundle for all firms

  firm_year_input_bundle <- long %>% 
    pivot_wider(names_from = nace5d, 
                values_from = c(expenditure),
                values_fill = list(expenditure = 0),
                names_prefix = "exp_") %>% 
    rename(vat_ano = vat_j_ano) %>% 
    left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
              by = c("vat_ano", "year")) %>% # find nace of buyer
    rename(vat = vat_ano)

  save(firm_year_input_bundle, file = paste0(proc_data,"/firm_year_input_bundle.RData"))
