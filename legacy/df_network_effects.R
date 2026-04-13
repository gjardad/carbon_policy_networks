#### HEADER -------

## This code creates data set that is used in regs for network effects of EUETS
# It contains firm-year level info on
#1. NACE sector
#2. EUETS treatment status
#3. EUETS activity
#4. emission intensity
#5. output
#6. exp share EUETS inputs
#7. allowance shortage
#8. downstream exposure
#9. upstream exposure
#10. exp share emission intensive inputs

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
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------

load(paste0(proc_data,"/firm_year_real_output_2005.RData"))

library(haven)
df_belgium_euets <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))



