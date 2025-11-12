#### HEADER -------

## This code creates data set at the firm-year level with info on
#  

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

load(paste0(proc_data,"/cpc_v2_to_cpc_v21.RData"))

library(readxl)  
cpa_v21_to_cpc_v21 <- read_excel(paste0(raw_data,"/Correspondences_and_dictionaries/CPA_2_1-CPC_2_1_final_26_April_2021.xlsx"), sheet = "CPA 2.1 - CPC 2.1 - 26-04-2021")

colnames(cpa_v21_to_cpc_v21) <- c("cpa_v21_code", "cpa_v21_count", "cpa_v21_description",
                                 "cpc_v21_code", "cpc_v21_count", "cpc_v21_description")

cpa_v21_to_cpc_v21$cpa_v21_code <- gsub("\\.", "", cpa_v21_to_cpc_v21$cpa_v21_code)

# Create CPC v2 to CPA 21 correspondence ------

cpc_v2_to_cpa_v21 <- cpc_v2_to_cpc_v21 %>% 
  left_join(cpa_v21_to_cpc_v21 %>% select(cpc_v21_code, cpa_v21_code), by = "cpc_v21_code") %>% 
  select(cpc_v2_code, cpa_v21_code)

# Save it
save(cpc_v2_to_cpa_v21, file = paste0(proc_data,"/cpc_v2_to_cpa_v21.RData"))  

