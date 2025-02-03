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
  
  # load annual emissions prices
  
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
  i <- 0
  
  y <- 2025
  #for(y in 2005:2022){
  
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
    
    # create vector of total expenditures by firm
    df_balance_sheet_year <- firm_year_balance_sheet_selected_sample %>% 
      filter(year == y)
    
    total_expenditure <- df_balance_sheet_year$network_purchases
    
    firm_index_balance_sheet <- match(df_balance_sheet_year$vat_ano, firms)
    
    ordered_x <- rep(NA, n_firms)
    ordered_total_expenditure[firm_index_balance_sheet] <- total_expenditure
    
    
  #}
  
  
  



