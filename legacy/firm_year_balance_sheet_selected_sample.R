#### HEADER -------

## Code that creates data set with basic variables for firms in the selected sample
# Variables are
# 1. vat id
# 2. year
# 3. total revenue from annual accounts
# 4. total wage bill
# 5. sales to other firms in the selected sample
# 6. imports
# 7. sales to domestic final demand
# 8. purchases from firms in the selected sample
# 9. exports

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
library(readxl)

# Import data ----

  load(paste0(proc_data,"/annual_accounts_selected_sample.RData"))

  load(paste0(proc_data,"/b2b_selected_sample.RData"))

  load(paste0(proc_data,"/df_b2b.RData"))
  
  load(paste0(proc_data,"/df_trade.RData"))

# Clean data ----
  
  #is auxiliary data ready?
  aux_data <- T
  if(aux_data == F){
    
  # calculate firms' total purchases and total sales
  #sales_ij is sales of firm i to firm j
  total_transactions <- df_b2b %>%
    # Create a long format for vat codes and year
    pivot_longer(c(vat_i_ano, vat_j_ano), names_to = "vat_type", values_to = "vat_ano") %>%
    # Group by vat_ano and year
    group_by(vat_ano, year) %>%
    # Calculate total_sales and total_purchases 
    summarise(
      total_sales = sum(corr_sales_ij[vat_type == "vat_i_ano"], na.rm = TRUE),
      total_purchases = sum(corr_sales_ij[vat_type == "vat_j_ano"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Replace NA values with 0 (if needed)
    mutate(
      total_sales = replace_na(total_sales, 0),
      total_purchases = replace_na(total_purchases, 0)
    )
  
  save(total_transactions, file = paste0(int_data,"/total_transactions.RData"))
  
  # calculate firms' sales to and purchases from other firms in the sample
  transactions_with_sampled_firms <- df_b2b_selected_sample %>%
    # Create a long format for vat codes and year
    pivot_longer(c(vat_i_ano, vat_j_ano), names_to = "vat_type", values_to = "vat_ano") %>%
    # Group by vat_ano and year
    group_by(vat_ano, year) %>%
    # Calculate total_sales and total_purchases 
    summarise(
      total_sales_to_sample = sum(corr_sales_ij[vat_type == "vat_i_ano"], na.rm = TRUE),
      total_purchases_from_sample = sum(corr_sales_ij[vat_type == "vat_j_ano"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Replace NA values with 0 (if needed)
    mutate(
      total_sales_to_sample = replace_na(total_sales_to_sample, 0),
      total_purchases_from_sample = replace_na(total_purchases_from_sample, 0)
    )
  
  save(transactions_with_sampled_firms, file = paste0(int_data,"/transactions_with_sampled_firms.RData"))
  
  # calculate firms' imports and exports
  transactions_abroad <- df_trade %>% 
    group_by(vat_ano, year, flow) %>% 
    summarise(
      total = sum(cn_value, na.rm = T)
    ) %>% 
    pivot_wider(
      names_from = flow,
      values_from = total,
      names_prefix = "total_",
      values_fill = 0  # Fill with 0 for missing values
    )
  
  save(transactions_abroad, file = paste0(int_data,"/transactions_abroad.RData"))
}
  
# Build final data set ----
  
  # load aux data
  load(paste0(int_data,"/total_transactions.RData"))
  load(paste0(int_data,"/transactions_with_sampled_firms.RData"))
  load(paste0(int_data,"/transactions_abroad.RData"))
  
  firm_year_balance_sheet_selected_sample <- df_annual_accounts_selected_sample %>% 
    select(vat_ano, year, turnover_VAT, v_0009800, v_0001023, nace5d) %>% 
    rename(value_added = v_0009800, wage_bill = v_0001023, turnover = turnover_VAT) %>% 
    left_join(total_transactions, by = c("vat_ano", "year")) %>% 
    left_join(transactions_with_sampled_firms, by = c("vat_ano", "year")) %>% 
    left_join(transactions_abroad, by = c("vat_ano", "year")) %>% 
    rename(network_sales = total_sales_to_sample,
           network_purchases = total_purchases_from_sample,
           imports = total_I, exports = total_X) %>% 
    mutate(sales_final_demand = turnover - total_sales - exports,
           total_sales_within_sample = network_sales + sales_final_demand + exports)

# save it ----
save(firm_year_balance_sheet_selected_sample, file = paste0(proc_data,"/firm_year_balance_sheet_selected_sample.RData"))  
  