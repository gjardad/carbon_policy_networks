#### HEADER -------

## This code cleans the SPPI in Belgian data from Eurostat and Statbel

# It produces a data set at the level of the nace code-year with the SPP index
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

  sppi_statbel <- read_excel(paste0(raw_data, "/sppi_statbel.xlsx")) %>% 
    select(c(1,6)) %>% 
    slice(-1) %>% 
  rename(services= 1, index = 2)

  sppi_statbel <- sppi_statbel %>%
    # Extract the code inside parentheses and remove any "." characters
    mutate(code = ifelse(is.na(index), 
                         str_extract(services, "\\(([^)]+)\\)"), 
                         NA)) %>% 
    mutate(code = str_replace_all(code, "[.()]", "")) %>% # Remove periods and parentheses
    mutate(code = str_trim(code)) %>%                    # Trim any extra spaces
    fill(code) %>%                                       # Fill down the sector code
    filter(!is.na(index)) %>%                            # Remove rows with sector codes
    rename(year = services, price_index = index)
  
  sppi_statbel <- sppi_statbel %>% 
    mutate(code = ifelse(is.na(code), "H-N_X_K", code))
  # code that matches overall services in eurostat

  services_pp <- data.frame()

  file_path <- paste0(raw_data, "/Eurostat/eurostat_services_pp_annual_belgium.xlsx")

  sheet_names <- excel_sheets(file_path)
  sheet_names <- sheet_names[3:length(sheet_names)] # no data in first two
  
  # Loop through each sheet within data set
  for (sheet in sheet_names) {

    # Read the necessary cells from the sheet
    sector <- read_excel(file_path, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
    index_year <- read_excel(file_path, sheet = sheet, range = "C9", col_names = FALSE)[[1]]
    years <- seq(2000, 2023)  # Years from 2000 to 2023
    price_indices <- read_excel(file_path, sheet = sheet, range = "B13:AV13", col_names = FALSE)
    price_indices <- price_indices[, seq(1, ncol(price_indices), by = 2)]
    
    # Reshape the data and append to the main data frame
    temp_df <- data.frame(
      code = sector,
      year = years,
      index_year = index_year,
      price_index = as.numeric(price_indices)
    )
    
    # Append to the main dataset
    services_pp <- rbind(services_pp, temp_df)
  }
  
  # Reshape the data to wide format with columns for each index year
  library(tidyr)
  sppi_wide <- pivot_wider(services_pp, 
                          names_from = index_year, 
                          values_from = price_index,
                          names_prefix = "index_",
                          values_fill = list(price_index = NA))
  
  # Transform all indices into 2010 = 100 index and create "collapsed" index
  sppi_wide <- sppi_wide %>% 
    group_by(code) %>% 
    mutate(
      # make sure its numeric
      index_I15 = as.numeric(index_I15),
      
      # Get the index value for the year 2010
      index_I15_value_in_10 = index_I15[year == 2010],
      
      # Transform index_2015 relative to 2010
      index_I15 = ifelse(!is.na(index_I15_value_in_10), 100 / index_I15_value_in_10 * index_I15, NA),
      
      # Collapse into one column
      sppi_2010 = coalesce(index_I10, index_I15)
    ) %>%
    ungroup() %>% 
    select(c(code, year, sppi_2010)) %>% 
    filter(year > 2005 & year < 2023)
  
  # Add data for code H-N_X_K from statbel
  sppi_statbel[, 2] <- sppi_statbel[, 2] %>%
    mutate(across(everything(), as.numeric))  # Convert all columns to numeric

  sppi_wide[18:34, "sppi_2010"] <- sppi_statbel[1:17,2] 

  # Create most disaggregated sppi by sector-year
  
  # step 1: identify parent codes
  parent_codes <- sppi_wide %>%  distinct(code)
  parent_codes$parent_code1 <- c(NA, NA, "H-N_X_K", "H", "H-N_STS",
                                "H49", "H", "H50", "H", "H",
                                "H52", "H52", "H", "H53", "H53",
                                "H-N_X_K", "I", "I", "H-N_X_K", "J",
                                "J", "J", "J", "J", "J62_J63",
                                "J62_J63", "J63", "J63", "H-N_X_K", "L",
                                "H-N_X_K", "M_STS", "M69_M702", "M69", "M69",
                                "M69_M702", "M_STS", "M_STS", "M_STS", "H-N_X_K",
                                "N", "N", "N", "N", "N",
                                "N81", "N")
  
  # step 3.3: parent_code2
  parent_codes <- parent_codes %>%
    left_join(parent_codes %>% select(code, parent_code1), 
              by = c("parent_code1" = "code"), 
              suffix = c("", "_parent")) %>%
    mutate(parent_code2 = parent_code1_parent) %>%
    select(-parent_code1_parent) # Clean up by removing the extra column
  
  # step 3.4: parent_code3
  parent_codes <- parent_codes %>%
    left_join(parent_codes %>% select(code, parent_code1), 
              by = c("parent_code2" = "code"), 
              suffix = c("", "_parent")) %>%
    mutate(parent_code3 = parent_code1_parent) %>%
    select(-parent_code1_parent) 
  
  # step 3.5: parent_code4
  parent_codes <- parent_codes %>%
    left_join(parent_codes %>% select(code, parent_code1), 
              by = c("parent_code3" = "code"), 
              suffix = c("", "_parent")) %>%
    mutate(parent_code4 = parent_code1_parent) %>%
    select(-parent_code1_parent) # Clean up by removing the extra column
  
  # step 4: create sppi_final
  
  # step 4.1: create sppi_final in sppi_wide
  sppi_wide$sppi_final <- sppi_wide$sppi_2010
  
  # step 4.2: loop over rows with missing sppi_2010
  missing_rows <- which(is.na(sppi_wide$sppi_2010))
  
  for (i in missing_rows) {
    code <- sppi_wide$code[i]
    year <- sppi_wide$year[i]
    
    # Find all parent codes for this code
    parent_candidates <- parent_codes[parent_codes$code == code, ]
    
    # Iterate over the parent_code variables (parent_code1, parent_code2, ..., parent_code7)
    for (j in 1:5) {
      parent_code <- parent_candidates[[paste0("parent_code", j)]]
      
      # Check if the parent_code has a non-missing sppi_2010 value in the same year
      sppi_value <- sppi_wide$sppi_2010[sppi_wide$code == parent_code & sppi_wide$year == year]
      
      if (length(sppi_value) > 0 && !is.na(sppi_value)) {
        # Assign the first non-missing ppi_2010 from the parent code to ppi_final
        sppi_wide$sppi_final[i] <- sppi_value
        break
      }
    }
  }
  
  # step 4.3: clean and save Sppi_wide
  sppi_final <- sppi_wide %>% 
    select(-c(sppi_2010)) %>% 
    slice(-c(1:17))
  
  save(sppi_final, file = paste0(proc_data,"/sppi_final.RData"))
