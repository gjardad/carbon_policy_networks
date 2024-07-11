#### HEADER -------

## This code creates data set at the installation-year level with info on
# 1. emissions
# 2. BvD id
# 3. activity and NACE ids

#####################

## Setup ------

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----
packages <- paste0(code, "/R_packages")

# NBB server doesnt have access to internet so need to download source file form CRAN in different computer
# then copy it here and then install package using local file

# for all packages, need to also install dependencies from CRAN
install.packages(paste0(packages,"/reprex_2.1.1.tar.gz"), repos = NULL, type = "source")

library(tidyverse)

## Import data ------

df_installation <- read.csv(paste0(raw_data,"/EUTL/installation.csv"))

df_account <- read.csv(paste0(raw_data,"/EUTL/account.csv"))

df_compliance <- read.csv(paste0(raw_data,"/EUTL/compliance.csv"))

## Clean data -------

df_installation <- df_installation %>% 
  rename(installation_id = id)

