#### HEADER -------

## This code backs out d log p_z that rationalizes
# an increase of energy prices by 1.6% (as in Kaenzig (2023)) in the spirit
# of section XX in the document "output_effects_of_carbon_tax"

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
library(dplyr)
library(Matrix)

# Import data -----

load(paste0(proc_data, "/firm_year_balance_sheet_selected_sample.RData"))
load(paste0(proc_data,"/firms_exposure_to_euets_treated_firms_by_year.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))

# Clean data ------

# Compute d log p_z -----

dlogpz_list <- list()
i <- 0
for(y in 2005:2022){
  
  i <- i + 1
  
  # step 1: compute vector of output share of each energy source out of energy sector
  df <- firm_year_balance_sheet_selected_sample %>%
    mutate(nace2d = substr(nace5d, 1, 2)) %>% 
    filter(year == y, nace2d == "35") %>% 
    mutate(turnover_share = turnover / sum(turnover, na.rm = TRUE)) %>% 
    select(vat_ano, turnover_share)
  
  output_share <- as.matrix(df$turnover_share)

  # step 2: build price exposure vector
  
  psi_exposure <- psi_exposure_list[[i]]
  
  row_sums <- rowSums(psi_exposure)
  
  # step 3: identify energy firms in price exposure vector
  
  vats_in_io_matrix <- vats_as_ordered_in_io_matrix[[i]]
  index_of_energy_firms <- match(df$vat_ano, vats_in_io_matrix)
  
  # step 4: build denominator
  
  denominator <- t(output_share) %*% as.matrix(row_sums[index_of_energy_firms])

  # step 5: compute dlogpz
  
  dlogpz_list[[i]] = 1.6/denominator
}

# save it
#save(dlogpz_list, file = paste0(proc_data,"/dlogpz_shocks_that_imply_hicp_energy_increases_in_diego_jmp.RData"))

