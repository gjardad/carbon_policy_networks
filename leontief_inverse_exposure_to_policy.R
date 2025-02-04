#### HEADER -------

## This code creates the column of network-adjusted exposure to firms treated by EUETS
# by year for all firms in the final sample

# In the paper's notation, it creates the column psi_(:,j) for each j treated by EUETS
# for each year

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
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))
  load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
  
  # load emissions prices
  emissions_price <- read_csv(paste0(raw_data, "/icap_price.csv")) %>% 
    select(1,3) %>% 
    rename(date = 1, price = 2) %>% 
    slice(-1)
  
  emissions_price$date <- as.Date(emissions_price$date)
  
  emissions_price <- emissions_price %>%
    mutate(price = as.numeric(as.character(price)))
  
  # make it annual prices
  annual_emissions_price <- emissions_price %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(average_price = mean(price, na.rm = TRUE)) %>%
    ungroup()

# Create column of firms treated by EUETS in the Leontief inverse -------

  # this is psi_(:,j) in my notation
  # it captures the network-adjusted exposure of firms to firms treated by EUETS

  # the psi_exposure matrix stores psi_(:,j) for each one of the js treated by EUTES

  psi_exposure_list <- list()
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
    # valid_indices has length larger than n_firms_euets because there are firms treated by EUETS
    # but for which emissions in a particular year are 0
    
    n_firms_euets <- sum(emission_intensiveness > 0)
    indices_firms_with_positive_emissions <- which(emission_intensiveness > 0)
    
    psi_exposure_matrix <- Matrix(0, nrow = n_firms, ncol = n_firms_euets, sparse = TRUE)
    
    # create psi_(:,j) for each j treated by EUETS
    for(j in 1:n_firms_euets){
      
      index_of_firm <- indices_firms_with_positive_emissions[[j]]
      basis_vector <- numeric(n_firms)
      basis_vector[index_of_firm] <- 1
      
      psi_j <- Matrix(0, nrow = n_firms, ncol = 1, sparse = TRUE)
      psi_j[index_of_firm] <- emission_intensiveness[index_of_firm]
      
      current_power <- io_matrix %*% basis_vector
      
      for(r in 1:50){
        psi_j <- psi_j + current_power
        current_power <- io_matrix %*% current_power
      }
      
      psi_exposure_matrix[,j] <- psi_j
      
    }
    
  psi_exposure_list[[i]] <- psi_exposure_matrix
  
  }

# Save it -------------------
save(psi_exposure_list, file = paste0(proc_data,"/firms_exposure_to_euets_treated_firms_by_year.RData"))
