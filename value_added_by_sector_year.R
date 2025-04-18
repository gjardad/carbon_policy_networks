#### HEADER -------

## Creates time series for value added by NACE sector in Belgium

# Obs: my series is based on FIGARO and does not match the numbers for VA directly
# from Eurostat. I believe the reason for this is that gross fixed capital is missing
# from FIGARO.

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
library(dplyr)
library(readxl)

# Import, clean and save data -----

va <- read_csv(paste0(raw_data, "/value_added_by_sector_year.csv"))

va <- va[6:71,-2]
va <- data.frame(lapply(va, as.character), stringsAsFactors = FALSE)
va[1,seq(3, 43, by = 2)] <- as.character(seq(2003,2023,by = 1))
va <- va[-c(1:3),seq(1, 41, by = 2)]
colnames(va) <- c("nace", as.character(seq(2003,2022,by = 1)))

# get nace sector codes
nace_codes <- as.vector(unlist(read_excel(paste0(raw_data,"/FIGARO/Description_FIGARO_Tables(24ed).xlsx"),
                         sheet = "Prod, Ind & Accounting items")[5:67,4])) # exclude sector "U"

va[,1] <- nace_codes

va <- va %>%
  pivot_longer(
    cols = starts_with("20"),  # Select all year columns
    names_to = "year",          # Name of the new column for years
    values_to = "value"         # Name of the new column for values
  )

save(va, file = paste0(proc_data, "/value_added_by_sector_year.RData"))





