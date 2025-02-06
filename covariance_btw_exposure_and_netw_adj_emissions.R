#### HEADER -------

## This code creates the covariance between exposure to policy and netw. adj.
# emissions for all euets firms, by year

# see computational notes for notation used in the code

# obs: I'm creating the vector x as the vector of firms' total costs
# in the model, this should be the vector of firms' total *physical* output
# I'm assuming perfect competition and therefore zero profits and therefore
# revenue = costs and I'm normalizing prices to 1 in the SS so that
# physical output = revenue = costs

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

  load(paste0(proc_data,"/firms_total_costs_by_year.RData"))
  load(paste0(proc_data,"/io_matrix_by_year.RData"))
  load(paste0(proc_data,"/firms_exposure_to_euets_treated_firms_by_year.RData"))
  load(paste0(proc_data,"/network_adjusted_emission_intensiveness_by_year.RData"))
  load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))

# Calculate covariances ------

df_cov_list <- list()

for(y in 2005:2022){
  
  i <- i + 1
  
  io_matrix <- io_matrix_list[[i]]
  ordered_total_costs <- firms_total_costs_list[[i]]
  firms <- vats_as_ordered_in_io_matrix[[i]]
  psi_exposure <- psi_exposure_list[[i]]
  psi_e <- psi_e_list[[i]]
  x <- firms_total_costs_list[[i]]
  
  n_firms_euets <- ncol(psi_exposure)
  
  df_cov <- data.frame(vat = numeric(n_firms_euets),
                       cov = numeric(n_firms_euets))
  
  for(j in n_firms_euets){
    
    vat <- vat_of_firm
    cov <- transpose(x) %*% (io_matrix %*% (psi_exposure * psi_e) - (io_matrix %*% psi_exposure)(io_matrix %*% psi_e))
    
    row <- c(vat, cov)
    
    df_cov <- row_binds(df_cov,row)
  }
  
  df_cov_list[[i]] <- df_cov
  
}