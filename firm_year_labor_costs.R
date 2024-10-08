#### HEADER -------

## This code creates firm-year measure of labor costs

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

# Import data --------

library(haven)
df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

# Clean data --------

firm_year_labor_costs <- df_national_accounts %>% 
  select(vat_ano, year, v_0001023, v_0001033) %>% 
 #rename(labor = 3)

# Save it -------
save(firm_year_labor_costs, file = paste0(proc_data,"/firm_year_labor_costs.RData"))  

