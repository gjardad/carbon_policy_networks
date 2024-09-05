#### HEADER -------

## This code creates data set at the firm-activity-year level with info on
# 1. pct of firm's emissions that come installations of the corresponding activity

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
library(summarytools)

# Import data ----

df_installation <- load(paste0(proc_data,"/installation_year_emissions.RData"))

load(paste0(proc_data,"/firm_year_emissions.RData"))
