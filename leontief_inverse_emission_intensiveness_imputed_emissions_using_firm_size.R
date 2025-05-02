#### HEADER -------

## This code creates the column of network-adjusted emission intensity
# by year for all firms in the final sample using imputed emissions
# for non-EUETS firms according to their firm size

# In the paper's notation, it creates the column psi_e for each year

# It also creates the vector of VAT ids of euets firms for which emission intensiveness > 0
# by year, organized in the order in which they show up in the I-O matrix

# This is also the order in which those firms show up in the exposure matrices
# in psi_exposure_list

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
library(dplyr)
library(Matrix)

# Import data ------

load(paste0(proc_data,"/io_matrix_by_year.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
load(paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))
load(paste0(proc_data,"/firms_total_costs_by_year.RData"))

# Create column of emission intensiveness in the Leontief inverse -------

# this is psi_(:,e) in my notation
# it captures the network-adjusted emission intensity of firms

psi_e_list_imputation_using_firm_size <- list()
euets_vat_ids_list <- list()
i <- 0

for(y in 2005:2022){
  
  i <- i + 1
  
  io_matrix <- io_matrix_list[[i]]
  ordered_total_costs <- firms_total_costs_list[[i]]
  firms <- vats_as_ordered_in_io_matrix[[i]]
  n_firms <- nrow(io_matrix)
  
  firm_emissions <- firm_year_belgian_euets %>% 
    filter(year == y, in_sample == 1) %>% 
    select(vat, emissions)
  
  index_of_pollutant_firms <- match(firm_emissions$vat, firms)
  valid_indices <- na.omit(index_of_pollutant_firms)
  
  emissions_unordered <- firm_emissions$emissions
  
  ordered_emissions <- rep(0, n_firms)
  ordered_emissions[valid_indices] <- emissions_unordered[!is.na(index_of_pollutant_firms)]
  
  indices_euets_firms_w_positive_emissions <- which(ordered_emissions > 0)
  euets_vat_ids <- data.frame(vat = firms[indices_euets_firms_w_positive_emissions],
                              index_in_io_matrix = indices_euets_firms_w_positive_emissions)
  
  emission_intensiveness <- ordered_emissions/ordered_total_costs
  
  psi_e <- emission_intensiveness
  current_power <- io_matrix %*% emission_intensiveness
  
  for(r in 1:50){
    psi_e <- psi_e + current_power
    current_power <- io_matrix %*% current_power
  }
  
  psi_e_list[[i]] <- psi_e
  euets_vat_ids_list[[i]] <- euets_vat_ids
}

# Save it -------------------
save(psi_e_list, file = paste0(proc_data,"/network_adjusted_emission_intensiveness_by_year.RData"))
save(euets_vat_ids_list, file = paste0(proc_data,"/vat_ids_and_indices_of_euets_firms.RData"))


