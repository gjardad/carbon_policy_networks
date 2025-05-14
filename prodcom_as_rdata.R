#### HEADER -------

## Save prodcom data as RData

# This code is meant to be ran within Stata

#####################

## Setup -----
rm(list = ls())

## Recover globals defined in Stata -------

  # Capture command-line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Assign the first argument to jarda_folder
  jarda_folder <- args[1]
  
  gert_folder <- args[2]
  
  prodcom_file <- args[3]

## Setup file paths -----
  
raw_data <- paste0(jarda_folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(jarda_folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(jarda_folder, "/carbon_policy_networks/data/processed")

output <- paste0(jarda_folder, "/carbon_policy_networks/output")

code <- paste0(jarda_folder, "/carbon_policy_networks/code")

# Libraries ----

.libPaths("E:/Documents/JARDANG/r_packages/win-library/4.2.1")

library(tidyverse)
library(dplyr)

# Import data -------

library(haven)
prodcom <- read_dta(paste0(gert_folder, "/", prodcom_file))
prodcom <- prodcom %>% 
  rename(vat_ano = 1) %>% 
  mutate(vat_ano = as.character(vat_ano))

# Save it ----
save(prodcom, file = paste0(gert_folder,"/prodcom.RData"))

# Version with mock prodcom ---------

mock <- F
if(mock == T){
library(haven)
  mock_prod <- read_dta(paste0(raw_data, "/NBB/prod.dta"))
  mock_prod <- mock_prod %>% 
    rename(vat_ano = 1) %>% 
    mutate(vat_ano = as.character(vat_ano))
  
  # Save it ----
  #save(mock_prod, file = paste0(proc_data, "mock_prod.RData"))
}
