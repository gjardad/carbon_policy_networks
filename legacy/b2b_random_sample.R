#### HEADER -------

## This code creates data set that is random sample of B2B

# Important: data records sales_ij
# which are sales of firm i (supplier) to firm j (buyer)

#####################


# Setup ------
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

load(paste0(proc_data,"/df_b2b.RData"))

load(paste0(proc_data,"/annual_accounts_selected_sample.RData"))

# Create and save sample -----

  # full random sample
  df_b2b_random_sample <- df_b2b %>% sample_n(10000)
  save(df_b2b_random_sample, file = paste0(proc_data,"/b2b_random_sample.RData"))

  # part of random sample that belongs to selected sample from annual accounts
  df_b2b_selected_random_sample <- df_b2b_random_sample %>% 
    filter(vat_i_ano %in% df_annual_accounts_selected_sample$vat_ano,
           vat_j_ano %in% df_annual_accounts_selected_sample$vat_ano)
  save(df_b2b_selected_random_sample, file = paste0(proc_data,"/b2b_selected_random_sample.RData"))
