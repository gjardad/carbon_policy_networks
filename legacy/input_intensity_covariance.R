#### HEADER -------

## Calculate covariance matrix of input intensity at the
# leve of the sector-input pair in the year 2005.

# That is, for each sector calculate the covariance of the input
# intensity of any given pair of inputs across firms, and then average
# this number across sectors.

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

load(paste0(proc_data,"/firm_year_nominal_domestic_input_bundle_2005.RData"))

load(paste0(proc_data,"/firm_year_belgian_euets.RData"))

# Calculate average input intensity by sector -----

  test <- firm_year_nominal_domestic_input_bundle_2005 %>% 
    ungroup() %>% 
    select(-expenditure_share, -year, -output) %>% 
    group_by(vat_j_ano) %>% 
    mutate(total_expenditure = sum(expenditure, na.rm = TRUE),
           expenditure_share = expenditure/total_expenditure) %>% 
    ungroup()

  test <- test %>% 
    mutate(nace4d_buyer = substr(nace5d_buyer, 1, 4),
           nace3d_buyer = substr(nace5d_buyer, 1, 3),
           nace2d_buyer = substr(nace5d_buyer, 1, 2),
           nace4d_supplier = substr(nace5d_supplier, 1, 4),
           nace3d_supplier = substr(nace5d_supplier, 1, 3),
           nace2d_supplier = substr(nace5d_supplier, 1, 2))
  
  pollutant_inputs <- as.data.frame(c(unique(firm_year_belgian_euets$nace5d))) %>% 
    rename(nace5d = 1) %>% 
    mutate(nace4d = substr(nace5d, 1, 4),
           nace3d = substr(nace5d, 1, 3),
           nace2d = substr(nace5d, 1, 2)) %>% 
    filter(!is.na(nace5d))

  df_cov_matrix <- test %>% 
    filter(nace4d_supplier %in% pollutant_inputs$nace4d) %>% 
    select(-expenditure, -total_expenditure, -nace5d_buyer, -nace5d_supplier) %>% 
    ungroup()
  
  # calculate covariance matrix for two sectors
  df_cov_matrix_2d <- df_cov_matrix %>% 
    select(-c(vat_j_ano, nace4d_buyer, nace4d_supplier, nace3d_buyer, nace3d_supplier))
  
  start_time <- Sys.time()
  
  # Calculate the covariance for selected sectors
  correlation_results <- df_cov_matrix_2d %>%
    group_by(nace2d_buyer) %>%
    filter(n() > 1) %>%  # Keep only buyers with more than one observation
    summarise(correlation_matrix = list({
      # Create a wide format for expenditure shares
      exp_matrix <- pivot_wider(
        data = cur_data(),
        names_from = nace2d_supplier,
        values_from = expenditure_share,
        values_fill = list(expenditure_share = 0),  # Fill NAs with 0
        values_fn = list(expenditure_share = mean)  # Change to mean if duplicates
      ) %>%
        as.data.frame()  # Ensure it's a data frame
      
      # Check that we still have more than one observation post-pivot
      if (nrow(exp_matrix) > 1) {
        # Calculate correlation matrix
        cor_matrix <- cor(exp_matrix[, sapply(exp_matrix, is.numeric)], use = "pairwise.complete.obs")
        return(cor_matrix)
      } else {
        return(matrix(NA, ncol = ncol(exp_matrix), nrow = ncol(exp_matrix), dimnames = list(NULL, NULL)))  # Return NA matrix if not enough data
      }
    }), .groups = "drop")
  
  # End measuring time
  end_time <- Sys.time()
  execution_time <- end_time - start_time

  
      

