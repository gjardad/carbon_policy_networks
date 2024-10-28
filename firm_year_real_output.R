#### HEADER -------

## This code creates data set at the firm-year level with
# firm-level output in 2010 prices

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

df_trade <- read_dta(paste0(raw_data,"/NBB/import_export_ANO.dta"))

load(paste0(proc_data,"/ppi_final.RData"))

# Clean data -------

  # include "0" in front of nace5d
  df_national_accounts <- df_national_accounts %>% 
    mutate(nace5d = str_pad(nace5d, width = 5, pad = "0")) 

  # Create firm-level output using national accounts
  
  output_national_accounts <- df_national_accounts %>% 
    rename(turnover = v_0000070) %>% 
    select(turnover, vat_ano, year, nace5d)
  
  # Create firm-level output using B2B
  
  output_b2b <- df_b2b %>%
    group_by(vat_i_ano, year) %>% 
    summarize(sales = sum(corr_sales_ij, na.rm = TRUE)) %>% # total sales
    rename(vat_ano = vat_i_ano)
  
# Create firm-level output using exports
  
  output_exports <- df_trade %>%
    filter(flow == "X") %>% 
    group_by(vat_ano, year) %>% 
    summarize(exports = sum(cn_value, na.rm = TRUE)) # total exports
  
# Create firm-level output combining the three
  
  firm_year_real_output <- output_national_accounts %>% 
    left_join(output_b2b, by = c("vat_ano", "year")) %>% 
    left_join(output_exports, by = c("vat_ano", "year")) %>% 
    mutate(sales_exports = ifelse(is.na(exports), sales, sales + exports)) %>% 
    mutate(output = pmax(turnover, sales_exports, na.rm = TRUE)) %>% 
    select(-c(turnover, exports, sales, sales_exports)) %>%
    mutate(nace4d = substr(nace5d, 1, 4),
           nace3d = substr(nace5d, 1, 3),
           nace2d = substr(nace5d, 1, 2))
  
  firm_year_real_output <- firm_year_real_output %>% 
    left_join(ppi, by = c("nace4d" = "code", "year"))
  
  firm_year_real_output <- firm_year_real_output %>%
    left_join(ppi, by = c("nace3d" = "code", "year" = "year"), 
              suffix = c("", "_nace3")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index_nace3, price_index)) %>%
    select(-price_index_nace3)
  
  firm_year_real_output <- firm_year_real_output %>%
    left_join(ppi, by = c("nace2d" = "code", "year" = "year"), 
              suffix = c("", "_nace2")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index_nace2, price_index)) %>%
    select(-price_index_nace2)
  
  library(labelled)
  var_label(firm_year_real_output$output) <- NULL
  
  firm_year_real_output_2005 <- firm_year_real_output %>% 
    filter(year >= 2005, !is.na(output)) %>% 
    mutate(real_output = output/price_index * 100)
  
# save it --------
save(firm_year_real_output_2005, file = paste0(proc_data,"/firm_year_real_output_2005.RData"))

  
  
  
  