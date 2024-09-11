#### HEADER -------

## This code cleans the PPI Belgian data from Eurostat

# It produces a data set at the level of the nace code-year with the PPI index
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

# Create ppi by sector-year -----

  # Initialize an empty data frame to store results
  ppi_data <- data.frame()

  # Loop through each data set
  for (i in 1:4){
    
    file_path <- paste0(raw_data,"/eurostat_ppi_nace_annual_belgium_", i, ".xlsx")
    
    # Get the names of all sheets
    sheet_names <- excel_sheets(file_path)
    sheet_names <- sheet_names[3:length(sheet_names)] # no data in first two
    
    # Loop through each sheet within data set
    for (sheet in sheet_names) {
      
      # Read the necessary cells from the sheet
      sector <- read_excel(file_path, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
      index_year <- substr(read_excel(file_path, sheet = sheet, range = "C9", col_names = FALSE)[[1]], 8, 11)
      years <- seq(2000, 2023)  # Years from 2000 to 2023
      price_indices <- read_excel(file_path, sheet = sheet, range = "B13:AV13", col_names = FALSE)
      price_indices <- price_indices[, seq(1, ncol(price_indices), by = 2)]
      
      # Reshape the data and append to the main data frame
      temp_df <- data.frame(
        sector = sector,
        year = years,
        index_year = index_year,
        price_index = as.numeric(price_indices)
      )
      
      # Append to the main dataset
      ppi_data <- rbind(ppi_data, temp_df)
    }
  }

  # Reshape the data to wide format with columns for each index year
  library(tidyr)
  ppi_wide <- pivot_wider(ppi_data, 
                          names_from = index_year, 
                          values_from = price_index,
                          names_prefix = "index_")

  # Transform all three indices into 2010 = 100 index and create "collapsed" index
  ppi_wide <- ppi_wide %>% 
    group_by(sector) %>% 
    mutate(
      # Get the index value for the year 2010 for each index
      index_2015_value_in_10 = index_2015[year == 2010],
      index_2021_value_in_10 = index_2021[year == 2010],
    
      # Transform index_2015 and index_2021 relative to 2010
      index_2015 = 100 / index_2015_value_in_10 * index_2015,
      index_2021 = 100 / index_2021_value_in_10 * index_2021,
      
      # Collapse into one column
      ppi_2010 = coalesce(index_2010, index_2015, index_2021)
    ) %>%
    ungroup()  # Always good practice to ungroup after transformations
  
# Create correspondence between sector name and sector code -----

  correspondence <- read_excel(paste0(raw_data,"/correspondence_nace_codes.xlsx")) %>% 
    select(c(2,4)) %>% 
    rename(code = 1, sector = 2) %>% 
    mutate(
      code_level1 = ifelse(grepl("[A-Za-z]", code), code, NA),
      code_level2 = substr(code, 1, 2),
      code_level3 = substr(gsub("\\.", "", code), 1, 3),
      code_level4 = substr(gsub("\\.", "", code), 1, 4)
    ) %>%
    fill(code_level1) %>% 
    select(-c("code")) %>% 
    distinct() %>% 
    group_by(sector) %>%
    slice_max(nchar(code_level4)) %>%  # Select row with max number of digits in 'code_level4'
    ungroup()
  
# Merge data sets ----
  
  ppi_merge <- ppi_wide %>% 
    left_join(correspondence, by = "sector") %>% 
    select(-c(3:7))
  
# Create most disaggregated ppi by sector-year
  
  
  