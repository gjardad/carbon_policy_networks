#### HEADER -------

## This code creates correspondence from  CPC v.2 to CPC v.2.1

# It is based on the following raw correspondence: https://unstats.un.org/unsd/classifications/Econ/tables/CPC/CPCv2_CPCv21/cpc2-cpc21.txt

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

library(readxl)  
cpc_v2_to_cpc_v21 <- read_excel(paste0(raw_data,"/Correspondences_and_dictionaries/cpc_v2_to_cpc_v21.xlsx"), sheet = "Sheet1") %>%
  mutate(
    across(c(1, 3), ~ str_pad(as.character(.x), width = 5, side = "left", pad = "0"))
  )

colnames(cpc_v2_to_cpc_v21) <- c("cpc_v2_code", "is_cpc_v2_partial", "cpc_v21_code", "is_cpc_v21_partial")

# Save it -------
save(cpc_v2_to_cpc_v21, file = paste0(proc_data,"/cpc_v2_to_cpc_v21.RData"))  

