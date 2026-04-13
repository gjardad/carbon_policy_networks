#### HEADER -------

## This code creates df with firm-year measure of labor costs
# and creates version of the df with imputed labor costs for firms
# for which labor costs are missing using sector-year avg labor costs

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
  select(vat_ano, year, v_0001023) %>% 
  rename(labor = 3)

# Impute labor costs

  # calculate sector-year avg labor to revenue
  sector_year_labor_costs <- df_national_accounts %>%
    select(vat_ano, nace5d, year, v_0001023, v_0000070) %>%
    mutate(labor_to_revenue = ifelse(is.na(v_0000070) | v_0000070 == 0, NA, v_0001023/v_0000070)) %>% 
    group_by(year, nace5d) %>% 
    summarise(avg_labor_to_revenue = mean(labor_to_revenue, na.rm = TRUE)) %>% 
    select(nace5d, year, avg_labor_to_revenue)
  
  # impute labor
  firm_year_labor_costs_imputed <- df_national_accounts %>% 
    select(vat_ano, year, nace5d, v_0000070, v_0001023) %>%
    left_join(sector_year_labor_costs, by=c("year", "nace5d")) %>% 
    mutate(v_0001023 = ifelse(is.na(v_0001023), avg_labor_to_revenue*v_0000070, v_0001023)) %>% 
    select(vat_ano, year, v_0001023) %>% 
    rename(labor = 3)

# Save it -------
save(firm_year_labor_costs, file = paste0(proc_data,"/firm_year_labor_costs.RData"))  

