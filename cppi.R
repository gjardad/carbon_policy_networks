#### HEADER -------

## This code cleans the construction producer prices in Belgian data from Eurostat

# It produces a data set at the level of the nace code-year with the CPP index
# using 2010 as base year (2010 = 100)

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
library(readxl)

# Import data ------

  file_path <- paste0(raw_data, "/Eurostat/eurostat_cppi_annual_belgium.xlsx")

  # Read the necessary cells from the sheet
  years <- seq(2000, 2022)  # Years from 2000 to 2023
  sector <- rep(c("41","42","43"), each = length(years))
  price_indices <- read_excel(file_path, sheet = "Sheet 1", range = "B13:X13", col_names = FALSE)

 cppi <- data.frame(
    code = sector,
    year = years,
    price_index = as.numeric(price_indices)
  )
 
 # make 2010 = 100
 cppi <- cppi %>% 
   mutate(price_index = price_index/price_index[year == 2010] * 100)

# save it -------
save(cppi, file = paste0(proc_data,"/cppi_final.RData"))
 