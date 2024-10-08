#### HEADER -------

## This code creates the Leontief inverse matrix for year 2020
# using different sample sizes for the firms being included in the matrix
# and measures the time it takes to create the inverse for each sample size

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
library(purrr)

# Import data ------

#load(paste0(proc_data, "/b2b_sample.RData"))
load(paste0(proc_data, "/firm_year_input_cost.RData"))

library(haven)
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))

# Measure time for Leontief inverse of subset of B2B ------

measure_leontief_time <- function(df, firm_sample_size) {
  # Subset the dataset to include only a sample of firms (both as buyers and sellers)
  sampled_firms <- df %>%
    filter(year == 2020) %>% 
    select(vat_i_ano, vat_j_ano) %>%
    distinct() %>%
    slice_sample(n = firm_sample_size)

  # Step 1: expand the data set such that everyone is buyer and supplier
  firms <- unique(c(sampled_firms$vat_j_ano, sampled_firms$vat_i_ano))
  
  # Step 2: Create all combinations of unique firms for buyers and sellers
  df_complete <- expand.grid(
    vat_j_ano = firms,
    vat_i_ano = firms,
    year = 2020
  )
  
  df_complete <- df_complete %>% 
    left_join(df %>% select(vat_j_ano, vat_i_ano, year, corr_sales_ij),
              by = c("vat_j_ano", "vat_i_ano", "year")) %>%
    mutate(corr_sales_ij = replace_na(corr_sales_ij, 0))
  
  # Step 2: Construct the transaction matrix for a specific year (e.g., 2020)
  transaction_matrix <- df_complete %>%
    select(vat_i_ano, vat_j_ano, corr_sales_ij) %>%
    pivot_wider(names_from = vat_i_ano, values_from = corr_sales_ij) %>%
    mutate(input_cost = rowSums(across(where(is.numeric)) + 1),
           # + 1 guarantees inverse exists (see Networks: A Primer p. 638)
           across(where(is.numeric), ~ .x / input_cost),  
           across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
    select(-c(input_cost)) %>% 
    column_to_rownames(var = "vat_j_ano") %>% 
    as.matrix()
  
  # Step 3: Measure time to calculate Leontief inverse
  time_taken <- system.time({
    I_matrix <- diag(1, nrow(transaction_matrix)) # Create identity matrix of correct size
    leontief_inverse <- solve(I_matrix - transaction_matrix)
  })
  
  # Return the number of firms and the time taken
  return(list(firm_sample_size = firm_sample_size, time_taken = time_taken))
}

# Define the different sample sizes of firms you want to test
firm_sample_sizes <- c(10, 50, 100, 200, 500, 1000, 5000)

# Measure the time taken for each sample size
timing_results <- map(firm_sample_sizes, ~ measure_leontief_time(df_b2b, .x))

# View the results
timing_results <- do.call(rbind, lapply(timing_results, as.data.frame))
print(timing_results)
