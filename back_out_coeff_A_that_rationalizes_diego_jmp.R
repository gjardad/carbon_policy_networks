#### HEADER -------

## This code backs out coeff A that rationalizes
# an output fall of 0.6% (as in Kaenzig (2023)) in the spirit
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

load(paste0(proc_data,"/dlogpz_shocks_that_imply_hicp_energy_increases_in_diego_jmp.RData"))
load(paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))
load(paste0(proc_data,"/firms_total_costs_by_year.RData"))
load(paste0(proc_data,"/vat_ids_and_indices_of_euets_firms.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
load(paste0(proc_data, "/ordered_domar_weights_by_year.RData"))

# Compute d log A -----

A_list <- list()
i <- 0
for(y in 2005:2022){
  
  i <- i + 1
  
  firms_ids <- vats_as_ordered_in_io_matrix[[i]]
  
  ordered_domar_weights <- ordered_domar_weights_list[[i]]
  
  # step 3: compute emission intensiveness
  firm_emissions <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
    filter(year == y) %>% 
    rename(vat = vat_ano) %>% 
    select(vat, emissions)
  
  # emissions of euets firms ordered according to order in io matrix
  euets_indices_in_io_matrix <- as.vector(euets_vat_ids_list[[i]]$index_in_io_matrix)
  euets_indices_in_firm_year_balance_sheet <- which(firm_emissions$vat %in% euets_vat_ids_list[[i]]$vat)
    
  emissions_unordered <- firm_emissions$emissions
  
  ordered_emissions <- rep(0, length(firms_ids))
  ordered_emissions[euets_indices_in_io_matrix] <- emissions_unordered[euets_indices_in_firm_year_balance_sheet]
  
  ordered_total_costs <- firms_total_costs_list[[i]]
  
  emission_intensiveness <- ordered_emissions/ordered_total_costs
  
  # step 4: compute denominator
  
  dlogpz <- dlogpz_list[[i]]
  
  denominator <- dlogpz * t(ordered_domar_weights) %*% emission_intensiveness
  
  # step 5: compute dlogA
  
  A_list[[i]] = -0.6/denominator
}

# save it
# save(A_list, file = paste0(proc_data,"/A_coeff_that_implies_output_decrease_in_diego_jmp.RData"))

