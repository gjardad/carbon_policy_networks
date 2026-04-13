#### HEADER -------

## This code creates data set with firm-year total input costs

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

# Import data ------
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))

# Clean data ------

firm_year_input_cost <- df_b2b %>%
  group_by(vat_j_ano, year) %>% # Group by j and year
  summarize(input_cost = sum(corr_sales_ij, na.rm = TRUE)) %>% 
  rename(vat = vat_j_ano)

save(firm_year_input_cost, file = paste0(proc_data,"/firm_year_input_cost.RData"))  

