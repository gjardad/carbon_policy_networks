#### HEADER -------

## Clean data on energy prices by country over time
# and emissions from energy sector by country over time

#####################

## Setup ------
rm(list = ls())

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "y_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------
  
  energy_prices <- data.frame()
  
  library(readxl)
  file_path <- paste0(raw_data,"/Eurostat/energy_prices_by_country_month.xlsx")
  
  # Get the names of all sheets
  sheet_names <- excel_sheets(file_path)
  sheet_names <- sheet_names[3:length(sheet_names)] # no data in first two
  
  # Loop through each sheet within data set
  for (sheet in sheet_names) {
    
    # Read the necessary cells from the sheet
    sector <- read_excel(file_path, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
    index <- read_excel(file_path, sheet = sheet, range = "C9", col_names = FALSE)[[1]]
    countries <- as.vector(unlist(read_excel(file_path, sheet = sheet, range = "A13:A51", col_names = FALSE)))
    months <- seq(from = as.Date("2000-01-01"), 
                  to = as.Date("2024-10-01"), 
                  by = "month")
    prices <- read_excel(file_path, sheet = sheet, range = "B13:VY51", col_names = FALSE)
    prices <- prices[ , seq(1, ncol(prices), by = 2)]
    
    # create vector of prices by stacking columns
    stacked_prices <- as.vector(t(apply(prices, 1, t)))
    stacked_prices <- as.numeric(ifelse(stacked_prices == ":", NA, stacked_prices))
    
    # Reshape the data and append to the main data frame
    temp_df <- data.frame(
      sector = rep(sector, length(stacked_prices)),
      index = rep(index, length(stacked_prices)),
      country = rep(countries, ncol(prices)),
      month = rep(months, each = nrow(prices)),
      prices = stacked_prices
    )
    
    # Append to the main dataset
    energy_prices <- rbind(energy_prices, temp_df)
  }