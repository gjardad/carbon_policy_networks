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
  load(paste0(proc_data,"/vat_ids_and_indices_of_euets_firms.RData"))
  load(paste0(proc_data,"/euets_sector_year_emissions.RData"))
  
  owid <- read_csv(paste0(raw_data,"/owid_annual-co2-emissions-per-country.csv")) %>% 
    filter(Entity == "Belgium", Year >= 2005) %>% 
    select(-c(Entity,Code)) %>% 
    rename(year = 1, emissions = 2)

# Calculate covariances ------

  df_cov_list <- list()
  i <- 0
  
  for(y in 2005:2022){
    
    i <- i + 1
    
    io_matrix <- io_matrix_list[[i]]
    ordered_total_costs <- firms_total_costs_list[[i]]
    firms <- vats_as_ordered_in_io_matrix[[i]]
    psi_exposure <- psi_exposure_list[[i]]
    psi_e <- psi_e_list[[i]]
    x <- as.vector(firms_total_costs_list[[i]])
    vat_of_euets_firms <- as.vector(euets_vat_ids_list[[i]][,1])
    index_of_euets_firms <- as.vector(euets_vat_ids_list[[i]][,2])
    
    n_firms_euets <- ncol(psi_exposure)
    
    total_emissions <- owid[owid$year == y, "emissions"][[1]]
    
    if(y <= 2007){
      # because I don't have those values for years before 2008
      euets_sectors_emissions <- NA
      euets_emissions <- NA
    } else{
      euets_sectors_emissions <- euets_sector_year_emissions %>% 
        filter(nace2d == "euets_sectors", year == y) %>%
        select(emissions_eurostat) %>% 
        pull()
        
      euets_emissions <- euets_sector_year_emissions %>% 
        filter(nace2d == "euets_sectors", year == y) %>%
        select(emissions_euets) %>% 
        pull()
    }
    
    df_cov <- data.frame(vat = vat_of_euets_firms,
                         cov = numeric(n_firms_euets),
                         corr = numeric(n_firms_euets),
                         total_emissions = rep(total_emissions, n_firms_euets),
                         euets_sectors_emissions = rep(euets_sectors_emissions, n_firms_euets),
                         euets_emissions = rep(euets_emissions, n_firms_euets))
    
    for(j in 1:n_firms_euets){
      
      cov <- (io_matrix %*% (psi_exposure[,j] * psi_e) - (io_matrix %*% psi_exposure[,j])*(io_matrix %*% psi_e))
      std_exposure <- sqrt((io_matrix %*% (psi_exposure[,j] * psi_exposure[,j]) - (io_matrix %*% psi_exposure[,j])*(io_matrix %*% psi_exposure[,j])))
      std_emission_intensity <- sqrt((io_matrix %*% (psi_e * psi_e) - (io_matrix %*% psi_e)*(io_matrix %*% psi_e)))
      
      temp <- cov/std_exposure*std_emission_intensity
      temp[is.na(temp)] <- 0
      # for some firms, there's no variability among their suppliers in exposure or emission intensity
      # in which case std = 0 and for this reason temp is NA
      # for those firms, the cov term is zero (because no variability) so set its value to zero (instead of NA)
      
      cov_x <- x %*% cov
      df_cov[j,2] <- cov_x
      
      corr <- x %*% temp
      df_cov[j,3] <- corr
      
    }
    
    # create row for column-wise sum
    sum_cov <- sum(df_cov[[2]], na.rm = TRUE)
    sum_corr <- sum(df_cov[[3]], na.rm = TRUE)
    
    last_row <- df_cov[nrow(df_cov), ]
    
    new_row <- data.frame(
      vat = "total",            # First variable
      cov = sum_cov,          # Second variable
      corr = sum_corr,          # Third variable
      total_emissions = last_row[[4]],     # Fourth variable
      euets_sectors_emissions = last_row[[5]],     # Fifth variable
      euets_emissions = last_row[[6]]      # Sixth variable
    )
    
    df_cov <- rbind(df_cov, new_row)
    
    df_cov_list[[i]] <- df_cov
    
  }
  
# save it -----
save(df_cov_list, file = paste0(proc_data,"/reallocation_term_of_targeted_interventions_by_year.RData"))
  
  