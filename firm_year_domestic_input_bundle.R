#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. expenditure (in Euros) on each NACE code

# to-do: input bundle euets includes firms identified as EUETS firms based on
# data Gert provided, but those do not correspond exactly to the Belgian firms
# for which we have data from EUTL for.

# fix this and make input_bundle_euets only include firms for which we have EUTL
# data for

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

df_belgium_euets <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

load(paste0(proc_data,"/ppi_final.RData"))

# Create real expenditure at nace5d level -------

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
    mutate(nace4d = substr(nace5d, 1, 4),
           nace3d = substr(nace5d, 1, 3),
           nace2d = substr(nace5d, 1, 2))

  # deflate nace5d-year-level expenditure using nace4d-year price index
  long <- long %>% 
    left_join(ppi, by = c("nace4d" = "code", "year"))
  
  long <- long %>%
    left_join(ppi, by = c("nace3d" = "code", "year" = "year"), 
              suffix = c("", "_nace3")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index_nace3, price_index)) %>%
    select(-price_index_nace3)
  
  long <- long %>%
    left_join(ppi, by = c("nace2d" = "code", "year" = "year"), 
              suffix = c("", "_nace2")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index_nace2, price_index)) %>%
    select(-price_index_nace2)
  
  # a lot of sectors for which price_index is missing still
  # but probably those are sectors not relevant for emissions-generating function
  
  long <- long %>% 
    mutate(real_expenditure = expenditure/price_index * 100) %>%
    select(-c(nace4d, nace3d, nace2d))

  long_clean <- long %>% 
    select(-c(price_index, expenditure)) 
  # otwise pivot_wider also considers different price_indexes when creating the unique obs
  
# Create wide format data for EUETS firms -----
    
  df_belgium_euets <- df_belgium_euets %>% 
    rename(vat_j_ano = vat_ano) %>%
    distinct(vat_j_ano) %>% 
    mutate(is_euets = 1)
  
  long_euets <- long_clean %>% 
    left_join(df_belgium_euets,
              by = "vat_j_ano") %>% 
    filter(is_euets == 1, !is.na(nace5d)) %>%
    select(-c(is_euets)) %>% 
    ungroup()
  
  firm_year_domestic_input_bundle_euets <- long_euets %>%
    pivot_wider(
      names_from = nace5d,        
      values_from = real_expenditure, 
      names_prefix = "real_exp_",
      values_fill = list(real_expenditure = 0) 
    ) %>% 
    rename(vat_ano = vat_j_ano) %>% 
    left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
              by = c("vat_ano", "year")) %>% # find nace of buyer
    rename(vat = vat_ano)
  
  library(labelled)
  var_label(firm_year_domestic_input_bundle_euets$year) <- NULL
    
  # sparse matrix version of it
  library(Matrix)
  
  long_euets <- long_euets %>%
    mutate(
      row_index = as.integer(factor(paste(vat_j_ano, year, sep = "_"))),
      col_index = as.integer(factor(nace5d))
    )
  
  sparse_firm_year_domestic_input_bundle_euets <- sparseMatrix(
    i = long_euets$row_index,   # Row indices (vat_j_ano-year combinations)
    j = long_euets$col_index,   # Column indices (nace5d)
    x = long_euets$real_expenditure,  # The real expenditure values
    dims = c(length(unique(long_euets$row_index)), length(unique(long_euets$col_index))),
    dimnames = list(
      unique(paste(long_euets$vat_j_ano, long_euets$year, sep = "_")),  # Row names
      unique(long_euets$nace5d)  # Column names
    )
  )
  
  # save it
  save(firm_year_domestic_input_bundle_euets, file = paste0(proc_data,"/firm_year_domestic_input_bundle_euets.RData"))
  save(sparse_firm_year_domestic_input_bundle_euets, file = paste0(proc_data,"/sparse_firm_year_domestic_input_bundle_euets.RData"))
  
# Wide format for all firms -----
    
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
