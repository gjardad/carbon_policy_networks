#### HEADER -------

## This code creates the column of network-adjusted emission intensity
# by year for all firms in the final sample

# In the paper's notation, it creates the column psi_e for each year

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
library(Matrix)

# Import data ------

  load(paste0(proc_data,"/io_matrix_by_year.RData"))
  load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))

# Create column of emission intensiveness in the Leontief inverse -------

  # this is psi_(:,e) in my notation
  # it captures the network-adjusted emission intensity of firms
    
  psi_e_list <- list()
  i <- 0

  for(y in 2005:2022){
    
    i <- i + 1
    
    io_matrix <- io_matrix_list[[i]]
    firms <- vats_as_ordered_in_io_matrix[[i]]
    n_firms <- nrow(io_matrix)
    
    firm_emissions <- firm_year_belgian_euets %>% 
      filter(year == y, in_sample == 1) %>% 
      select(vat, emissions, revenue) %>% 
      mutate(emission_intensiveness = emissions/revenue)
    
    index_of_pollutant_firms <- match(firm_emissions$vat, firms)
    valid_indices <- na.omit(index_of_pollutant_firms)
    
    emission_intensiveness_unordered <- firm_emissions$emission_intensiveness 
    
    emission_intensiveness <- rep(0, n_firms)
    emission_intensiveness[valid_indices] <- emission_intensiveness_unordered[!is.na(index_of_pollutant_firms)]
    
    psi_e <- emission_intensiveness
    current_power <- io_matrix %*% emission_intensiveness
    
    for(r in 1:50){
      psi_e <- psi_e + current_power
      current_power <- io_matrix %*% current_power
    }
    
    psi_e_list[[i]] <- psi_e
  }

# Save it -------------------
save(psi_e_list, file = paste0(proc_data,"/network_adjusted_emission_intensiveness_by_year.RData"))
  
  
