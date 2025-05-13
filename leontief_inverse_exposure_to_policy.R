#### HEADER -------

## This code creates a matrix that stores the net. adj. exposure of firms to policy

# This matrix is a non-squared subset of the Leontief inverse, times the emission intensity vector

# A typical entry [i,j] of the matrix stores d log p_i/d log p_z from a policy
# that ONLY targets firm j

# The matrix as dimension N x M, where N is the number of firms in the I-O matrix
# (i.e. in B2B) for any given year, and M is the number of firms for which emissions > 0
# after imputation

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
library(parallel)

# Import data ------

load(paste0(proc_data,"/io_matrix_by_year.RData"))
load(paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
load(paste0(proc_data,"/firms_total_costs_by_year.RData"))

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

# Create matrix of firm exposure ------

  psi_exposure_list <- list()
  i <- 0
  
  for(y in 2008:2022){
    
    i <- i + 1
    
    io_matrix <- io_matrix_list[[i+3]]
    ordered_total_costs <- firms_total_costs_list[[i+3]]
    firms_ids <- vats_as_ordered_in_io_matrix[[i+3]]
    annual_emissions_price_year <- annual_emissions_price %>% 
      filter(year == y)
  
    firm_emissions <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
      filter(year == y) %>% 
      rename(vat = vat_ano) %>% 
      select(vat, emissions)
    
    index_of_firms_emissions <- match(firm_emissions$vat, firms_ids)
    valid_indices <- na.omit(index_of_firms_emissions)
    
    emissions_unordered <- firm_emissions$emissions
    
    ordered_emissions <- rep(0, length(firms_ids))
    ordered_emissions[valid_indices] <- emissions_unordered[!is.na(index_of_firms_emissions)]
    
    emission_intensiveness <- ordered_emissions/ordered_total_costs
    
    indices_firms_with_positive_emissions <- which(emission_intensiveness > 0)
    
    psi_exposure_matrix <- Matrix(0, nrow = length(firms_ids), ncol = length(indices_firms_with_positive_emissions), sparse = TRUE)

    # Step 1: construct sparse matrix V
    # this is the matrix whose columns are \tau_j \hadamard \mathcal{E}
    # where \mathcal{E} is the vector of emission intensiveness
    # and \tau_j is the targeting vector, which takes on 1 for firm j and 0 otherwise
    
    n <- length(emission_intensiveness)
    m <- length(indices_firms_with_positive_emissions)
  
    rows <- indices_firms_with_positive_emissions
    cols <- seq_along(indices_firms_with_positive_emissions)
    vals <- emission_intensiveness[rows]
    
    V <- sparseMatrix(i = rows, j = cols, x = vals, dims = c(n, m))
    
    # --- Define batch computation function (fixed for S4 objects) ---
    compute_batch <- function(batch_cols, io_matrix, V, max_power = 50) {
      V_batch <- V[, batch_cols, drop = FALSE]
      PsiV_batch <- V_batch
      current_power <- V_batch
      
      for (r in 1:max_power) {
        current_power <- io_matrix %*% current_power
        PsiV_batch <- PsiV_batch + current_power
      }
      return(PsiV_batch)
    }
    
    # --- Set batch size and select test batches ---
    batch_size <- 500
    num_cols <- ncol(V)
    batches <- split(seq_len(num_cols), ceiling(seq_len(num_cols) / batch_size))
    test_batches <- batches[1:3]  # First 3 batches only
    
    # --- Create cluster for parallel computation ---
    n_cores <- detectCores() - 1
    cl <- makeCluster(n_cores)
    
    # Export required objects and methods to workers
    clusterExport(cl, varlist = c("V", "io_matrix", "compute_batch"), envir = environment())
    
    # Load required packages on workers
    clusterEvalQ(cl, library(Matrix))
    
    # --- Run parallel batch computation ---
    start_time <- Sys.time()
    
    PsiV_test <- parLapply(
      cl,
      test_batches,
      function(batch) compute_batch(batch, io_matrix, V)
    )
    
    stopCluster(cl)
    
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    
    # Combine result
    PsiV_test_result <- do.call(cbind, PsiV_test)
    
    cat("??? Parallel test complete - processed", ncol(PsiV_test_result), "columns in", round(elapsed_time, 2), "seconds.\n")
    
    
#######################################
    
    # Step 2: split into batches
    batch_size <- 500
    
    # Ensure V is a sparse dgCMatrix (e.g., from Matrix::Matrix())
    V_batch <- V[, 1:batch_size, drop = FALSE]
    V_batch <- as(V_batch, "dgCMatrix")
    
    # Initialize timing
    time_taken <- system.time({
      
      PsiV_batch <- V_batch  # I * V
      current_power <- V_batch
      
      for (r in 1:50) {
        current_power <- io_matrix %*% current_power
        current_power <- as(current_power, "dgCMatrix")
        PsiV_batch <- PsiV_batch + current_power
      }
      
    })
    
    # Show timing results
    print(time_taken)
    
    psi_exposure_list[[i]] <- psi_exposure_matrix
    
  }
  
  
  
  # create psi_(:,j) for each j with positive emissions
  for(j in 1:5){
    
    # length(indices_firms_with_positive_emissions)
    
    index_of_firm <- indices_firms_with_positive_emissions[[j]]
    basis_vector <- numeric(length(firms_ids))
    basis_vector[index_of_firm] <- 1
    
    psi_j <- Matrix(0, nrow = length(firms_ids), ncol = 1, sparse = TRUE)
    psi_j[index_of_firm] <- emission_intensiveness[index_of_firm]
    
    current_power <- psi_j
    
    for(r in 1:50){
      current_power <- io_matrix %*% current_power
      psi_j <- psi_j + current_power
    }
    
    psi_exposure_matrix[,j] <- psi_j
    
  }  
  
  # step 1: construct sparse matrix V
  # this is the matrix whose columns are \tau_j \hadamard \mathcal{E}
  # where \mathcal{E} is the vector of emission intensiveness
  # and \tau_j is the targeting vector, which takes on 1 for firm j and 0 otherwise
  
  n <- length(emission_intensiveness)
  m <- length(indices_firms_with_positive_emissions)
  
  # Construct sparse matrix V
  rows <- indices_firms_with_positive_emissions
  cols <- seq_along(indices_firms_with_positive_emissions)
  vals <- emission_intensiveness[rows]
  
  V <- sparseMatrix(i = rows, j = cols, x = vals, dims = c(n, m))
  
  # step 2: compute \Psi(\tau_j \hadamard \mathcal{E}) using the Neumann series
  
  PsiV <- as(V, "dgCMatrix")
  current_power <- PsiV
  
  for (r in 1:50) {
    current_power <- io_matrix %*% current_power
    current_power <- as(current_power, "dgCMatrix")  # avoid Tsparse intermediate
    PsiV <- PsiV + current_power
  }