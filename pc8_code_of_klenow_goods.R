#### HEADER -------

## This code creates data set that provides CPA v.2.1 codes for each
# good listed as a fossil fuel in Klenow et al (2025)

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
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

# Import data -------

load(paste0(proc_data,"/cpc_v2_codes_of_klenow_goods.RData"))
load(paste0(proc_data,"/cpc_v2_to_cpa_v21.RData"))

# Create data set ------

cpa_v21_codes_of_klenow_goods_with_description <- cpc_v2_codes_of_klenow_goods %>% 
  left_join(cpc_v2_to_cpa_v21, by = "cpc_v2_code")

cpa_v21_codes_of_klenow_goods <- cpa_v21_codes_of_klenow_goods_with_description %>% 
  select(cpa_v21_code) %>% 
  distinct()

# Save it -------
save(cpa_v21_codes_of_klenow_goods_with_description, file = paste0(proc_data,"/cpa_v21_codes_of_klenow_goods_with_description.RData"))  
save(cpa_v21_codes_of_klenow_goods, file = paste0(proc_data,"/cpa_v21_codes_of_klenow_goods.RData"))  

library(haven)  
write_dta(cpa_v21_codes_of_klenow_goods, paste0(proc_data,"/cpa_v21_codes_of_klenow_goods.dta"))
