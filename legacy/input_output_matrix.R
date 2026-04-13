#### HEADER -------

## Calculate the input-output matrix for the Belgian economy in 2005

# I-O matrix Omega with dimesion N x N
# Each entry Omega_ij is pj*x_ij/(total expenditure)_i
# where (total expenditure)_i = sum of expenditure on domestic inputs, imports,
# labor, and emissions

# This implies that all entries of Omega are positive, smaller than 1,
# and that row-wise sum is smaller than 1.
# Thus by some theorems we get that Leontief exists and is element-wise nonnegative
# (Gershgorin circle theorem + section 2.1 in Carvalho and Tahbaz-Salehi, Production Networks: A Primer)

# This code generates four objects:
# list of matrices N x N with total expenditures of each row on each column, by year
# list of IO matrices N x N with expenditure as share of total expenditures of each row on each column, by year
# list of matrices N x 1 with VATs of firms in the order in which they show up in the IO matrix, by year
# list of matrices N x 1 with firms' total expenditures (inputs + labor + emissions), by year

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
library(dplyr)
library(Matrix)

# Import data ---

  load(paste0(proc_data,"/b2b_selected_sample.RData"))
  
  load(paste0(proc_data,"/firm_year_balance_sheet_selected_sample.RData"))
  
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))
  
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
  
# Build input-output matrix Omega ----
  
  # obs: in B2B data vat_i_ano is the id of the supplier, vat_j_ano is the id of the buyer
  # but in my notation i is the buyer and j is the seller.
  # Additionally, in sparseMatrix notation i indexes rows and j indexes columns.
  # So, really, (i,j) indexes three different things:
  # (suppliers, buyers) in B2B
  # (buyer, supplier) in my model
  # (rows, cols) in sparseMatrix
  # Don't get confused!
  
  colnames(df_b2b_selected_sample) <- c("vat_supplier", "vat_buyer",
                                        "year", "corr_sales") 
  
  exp_list <- list()
  io_matrix_list <- list()
  vats_as_ordered_in_io_matrix <- list()
  firms_total_costs_list <- list()
  
  i <- 0
  
  for(y in 2005:2022){
  
  # (sparse) matrix of expenditures exp_matrix
  
    # subset the relevant year
    df_b2b_year <- df_b2b_selected_sample %>%
      filter(year == y)
  
    # make sure buyers and suppliers cover the same set
    firms <- unique(c(df_b2b_year$vat_supplier, df_b2b_year$vat_buyer))
    n_firms <- length(firms)
    firm_index_buyer <- match(df_b2b_year$vat_buyer, firms)
    firm_index_supplier <- match(df_b2b_year$vat_supplier, firms)
  
    # create a square sparse matrix exp_matrix (firms x firms) of expenditures
    # where rows are buyers
    exp_matrix <- sparseMatrix(i = firm_index_buyer,
                        j = firm_index_supplier,
                        x = df_b2b_year$corr_sales,
                        dims = c(length(firms), length(firms)))
    
    # store it
    i <- i + 1
    exp_list[[i]] <- exp_matrix
    
    # create vector of total costs by firm
    # total costs = total exp inputs + labor + emissions
    
      # create map btw indices in df_balance_sheet and df_b2b
      df_balance_sheet_year <- firm_year_balance_sheet_selected_sample %>% 
        filter(year == y)
      
      firm_index_balance_sheet <- match(df_balance_sheet_year$vat_ano, firms)
      valid_indices <- na.omit(firm_index_balance_sheet)
      
      # total exp on inputs
      total_expenditure_inputs <- df_balance_sheet_year$network_purchases
      
      ordered_total_expenditure_inputs <- rep(NA, n_firms)
      ordered_total_expenditure_inputs[valid_indices] <- total_expenditure_inputs[!is.na(firm_index_balance_sheet)]
    
      # wage bill
      wage_bill <- df_balance_sheet_year$wage_bill

      ordered_wage_bill <- rep(NA, n_firms)
      ordered_wage_bill[valid_indices] <- wage_bill[!is.na(firm_index_balance_sheet)]
      
      # emissions
      df_emissions_year <- firm_year_belgian_euets %>% 
        filter(year == y, in_sample == 1) %>% 
        select(vat, emissions, allowance_shortage)
      
      annual_emissions_price_year <- annual_emissions_price %>% 
        filter(year == y)
      
      firm_index_emissions <- match(df_emissions_year$vat, firms)
      valid_indices <- na.omit(firm_index_emissions)
      
      emissions <- df_emissions_year$emissions
      
      ordered_emissions <- rep(NA, n_firms)
      ordered_emissions[valid_indices] <- emissions[!is.na(firm_index_emissions)]
      
      ordered_emissions_costs <- ordered_emissions*annual_emissions_price_year[[2]]
      ordered_emissions_costs[is.na(ordered_emissions_costs)] <- 0
      
      # allowance shortage
      allowance_shortage <- df_emissions_year$allowance_shortage
      
      ordered_allowance_shortage <- rep(NA, n_firms)
      ordered_allowance_shortage[valid_indices] <- allowance_shortage[!is.na(firm_index_emissions)]
      
      ordered_allowance_shortage_costs <- ordered_allowance_shortage*annual_emissions_price_year[[2]]
      ordered_allowance_shortage_costs[is.na(ordered_allowance_shortage_costs)] <- 0
      
      # total exp 
      # all quantities are in nominal euros
      ordered_total_costs = ordered_total_expenditure_inputs + 
        ordered_wage_bill + ordered_emissions_costs
      
    # expenditure share matrix
    io_matrix <- exp_matrix
    io_matrix@x <- exp_matrix@x / ordered_total_costs[exp_matrix@i + 1]
    io_matrix_list[[i]] <- io_matrix
    
    # vector of VATs in order of appearance in I-O matrix
    vats_as_ordered_in_io_matrix[[i]] <- firms
    
    # vector that stores firms' total expenditure (inputs + labor + emissions)
    firms_total_costs_list[[i]] <- ordered_total_costs
  }
  
# save it -----
save(exp_list, file = paste0(proc_data,"/io_matrix_by_year_in_absolute_terms.RData"))
save(io_matrix_list, file = paste0(proc_data,"/io_matrix_by_year.RData"))
save(vats_as_ordered_in_io_matrix, file = paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
save(firms_total_costs_list, file = paste0(proc_data,"/firms_total_costs_by_year.RData"))



