#### HEADER -------

## Save prodcom data as RData

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

# gert_folder <- TO BE ADDED

# Libraries ----

library(tidyverse)
library(dplyr)

# Import data -------

library(haven)
#prodcom <- read_dta(paste0(gert_folder,"/ADD NAME OF PRODCOM FILE IN YOUR FOLDER.dta"))
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
