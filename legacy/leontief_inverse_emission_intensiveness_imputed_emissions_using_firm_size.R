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
pollutant_firms_vat_ids_list <- list()
i <- 0

for(y in 2008:2020){
  
  # OBS: 2008-2020 period because sector-level emissions built using PEFA are only
  # available 2008 onwards, and because for 2020 there's a big drop in EUETS coverage
  # for sectoes C20 and C24 which for which the reasons are still unclear
  
  i <- i + 1
  
  io_matrix <- io_matrix_list[[i+3]]
  ordered_total_costs <- firms_total_costs_list[[i+3]]
  firms <- vats_as_ordered_in_io_matrix[[i+3]]
  n_firms <- nrow(io_matrix)
  # i+3 because in io_matrix_list, firms_total_costs_list,
  # and vats_as_ordered_in_io_matrix the first element is 2005
  # and here first element is 2008
  
  firm_emissions <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
    filter(year == y) %>% 
    rename(vat = vat_ano) %>% 
    select(vat, emissions)
  
  # identify pollutant firms
  index_of_firms_emissions <- match(firm_emissions$vat, firms)
  valid_indices <- na.omit(index_of_firms_emissions)
  
  emissions_unordered <- firm_emissions$emissions
  
  ordered_emissions <- rep(0, n_firms)
  ordered_emissions[valid_indices] <- emissions_unordered[!is.na(index_of_firms_emissions)]
  
  indices_firms_w_positive_emissions <- which(ordered_emissions > 0)
  
  pollutant_firms_vat_ids <- data.frame(vat = firms[indices_firms_w_positive_emissions],
                                  index_in_io_matrix = indices_firms_w_positive_emissions)
  
  # build psi_e
  emission_intensiveness <- ordered_emissions/ordered_total_costs
  
  psi_e <- emission_intensiveness
  current_power <- io_matrix %*% emission_intensiveness
  
  for(r in 1:50){
    psi_e <- psi_e + current_power
    current_power <- io_matrix %*% current_power
  }
  
  psi_e_list_imputation_using_firm_size[[i]] <- psi_e
  pollutant_firms_vat_ids_list[[i]] <- pollutant_firms_vat_ids
}

# Save it -------------------
#save(psi_e_list_imputation_using_firm_size, file = paste0(proc_data,"/ntw_adj_emission_intensiveness_by_year_imputation_using_firm_size.RData"))
#save(pollutant_firms_vat_ids_list, file = paste0(proc_data,"/vat_ids_and_indices_of_pollutant_firms_using_firm_size_imputation.RData"))


