#### HEADER -------

## This code creates data set that is random sample of B2B

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
library(haven)
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))

# Create and save sample -----
df_b2b_sample <- df_b2b %>% sample_n(10000)
save(df_b2b_sample, file = paste0(proc_data,"/b2b_sample.RData"))  
