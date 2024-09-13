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

  # Find data sets within raw_data folder
  file_names <- list.files(path = raw_data, pattern = "^eurostat_ppi", full.names = FALSE)
  
  # Loop through each data set
  for (file in file_names){
    
    file_path <- paste0(raw_data,"/", file)
    
    # Get the names of all sheets
    sheet_names <- excel_sheets(file_path)
    sheet_names <- sheet_names[3:length(sheet_names)] # no data in first two
    
    # Loop through each sheet within data set
    for (sheet in sheet_names) {
      
      # Read the necessary cells from the sheet
      sector <- read_excel(file_path, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
      sector_code <- sub(".*\\[(.*)\\].*", "\\1", sector)
      index_year <- substr(read_excel(file_path, sheet = sheet, range = "C9", col_names = FALSE)[[1]], 8, 11)
      years <- seq(2000, 2023)  # Years from 2000 to 2023
      price_indices <- read_excel(file_path, sheet = sheet, range = "C13:AX13", col_names = FALSE)
      price_indices <- price_indices[, seq(1, ncol(price_indices), by = 2)]
      
      # Reshape the data and append to the main data frame
      temp_df <- data.frame(
        code = sector_code,
        year = years,
        index_year = index_year,
        price_index = as.numeric(price_indices)
      )
      
      # Append to the main dataset
      ppi_data <- rbind(ppi_data, temp_df)
    }
  }
  
  #save(ppi_data, file = paste0(int_data,"/ppi_data.RData"))

  # Reshape the data to wide format with columns for each index year
  library(tidyr)
  ppi_wide <- pivot_wider(ppi_data, 
                          names_from = index_year, 
                          values_from = price_index,
                          names_prefix = "index_",
                          values_fill = list(price_index = NA))

  # Transform all three indices into 2010 = 100 index and create "collapsed" index
  ppi_wide <- ppi_wide %>% 
    group_by(code) %>% 
    mutate(
      # make sure its numeric
      index_2015 = as.numeric(index_2015),
      index_2021 = as.numeric(index_2021),
      
      # Get the index value for the year 2010 for each index
      index_2015_value_in_10 = index_2015[year == 2010],
      index_2021_value_in_10 = index_2021[year == 2010],
    
      # Transform index_2015 and index_2021 relative to 2010
      index_2015 = ifelse(!is.na(index_2015_value_in_10), 100 / index_2015_value_in_10 * index_2015, NA),
      index_2021 = ifelse(!is.na(index_2021_value_in_10), 100 / index_2021_value_in_10 * index_2021, NA),
      
      # Collapse into one column
      ppi_2010 = coalesce(index_2010, index_2015, index_2021)
    ) %>%
    ungroup() %>% 
    select(c(code, year, ppi_2010))
  
  #save(ppi_wide, file = paste0(int_data,"/ppi_wide.RData"))
  
# Create most disaggregated ppi by sector-year
  
  # step 1: identify codes
  codes <- ppi_wide %>%  distinct(code)
  
  # step 2: identify "unorthodox" parent codes
  library(stringr)
  unorthodox_codes <- ppi_wide %>% 
    group_by(code) %>% 
    slice(1) %>% 
    select(code) %>% 
    filter(str_detect(code, "-|_")) %>% 
    rename(parent_code = code)
  
  # step 3: build parent codes
  
    # step 3.1: parent_code1
    parent_codes <- codes %>%
    mutate(parent_code1 = NA_character_) %>%
    rowwise() %>%
    mutate(
      parent_code1 = case_when(
        # For codes that are letter + 4 digits (e.g., B1234), take letter + 3 digits
        str_detect(code, "^[BCD]\\d{4}$") ~ str_sub(code, 1, 4),
        
        # For codes that are letter + 3 digits (e.g., B123), take letter + 2 digits
        str_detect(code, "^[BCD]\\d{3}$") ~ str_sub(code, 1, 3),
        
        # For B + 2 digits (e.g., B12), parent_code1 is just B
        str_detect(code, "^B\\d{2}$") ~ "B",
        
        # For D + 2 digits (e.g., D12), parent_code1 is "B-D"
        str_detect(code, "^D\\d{2}$") ~ "B-D",
        
        # For C + specific 2 digits, parent_code1 is from parent_codes and contains "_" and "CXX"
        str_detect(code, "^C(10|11|13|14|17|18|20|21|22|23|24|25|26|27|29|30|31|32)$") ~ {
          match <- unorthodox_codes %>% 
            filter(str_detect(parent_code, "_") & str_detect(parent_code, str_c("C", str_sub(code, 2, 3)))) %>%
            pull(parent_code)
          ifelse(length(match) > 0, match[1], NA_character_)
        },
        
        # For C + other specific 2 digits, parent_code1 is from parent_codes and contains "-" and "CXX"
        str_detect(code, "^C(12|15|16|33)$") ~ {
          match <- unorthodox_codes %>% 
            filter(str_detect(parent_code, "-") & str_detect(parent_code, str_c("C", str_sub(code, 2, 3)))) %>%
            pull(parent_code)
          ifelse(length(match) > 0, match[1], NA_character_)
        },
        
        # For codes starting with "C" and containing "_", parent_code1 is from parent_codes and contains "-" and first three elements of code
        str_detect(code, "^C(10|13|31)_.+") ~ {
          match <- unorthodox_codes %>% 
            filter(str_detect(parent_code, "-") & str_detect(parent_code, str_sub(code, 1, 3))) %>%
            pull(parent_code)
          ifelse(length(match) > 0, match[1], NA_character_)
        },
        
        # For codes starting with "C" + 17, 20, 22, 24, 26, or 29 and containing "_", parent_code1 is "C"
        # (those are included in thw category above, but parent_codes is empty for those)
        str_detect(code, "^C(17|20|22|24|26|29)_.+") ~ "C",
        
        # For codes starting with "C" and containing "-", parent_code1 is "B_C"
        str_detect(code, "^C.*-") ~ "B_C",
        
        # Default case
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
    
    # step 3.2: add parent_code1 by hand
    parent_codes[2,2] <- "B-E36"
    parent_codes[3,2] <- "B-E36"
    parent_codes[12,2] <- "B-D"
    parent_codes[13,2] <- "B_C"
    parent_codes[14,2] <- "B_C_X_MIG_NRG"
    parent_codes[15,2] <- "B_C"
    parent_codes[33,2] <- "B_C"
    parent_codes[136,2] <- "C"
    parent_codes[275,2] <- "C"
    parent_codes[349,2] <- "B-D"
    parent_codes[354,2] <- "B-E36"
    parent_codes[c(71:77),2] <- "C11"
  
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
    
    # step 3.6: parent_code5
    parent_codes <- parent_codes %>%
      left_join(parent_codes %>% select(code, parent_code1), 
                by = c("parent_code4" = "code"), 
                suffix = c("", "_parent")) %>%
      mutate(parent_code5 = parent_code1_parent) %>%
      select(-parent_code1_parent) # Clean up by removing the extra column
    
    # step 3.7: parent_code6
    parent_codes <- parent_codes %>%
      left_join(parent_codes %>% select(code, parent_code1), 
                by = c("parent_code5" = "code"), 
                suffix = c("", "_parent")) %>%
      mutate(parent_code6 = parent_code1_parent) %>%
      select(-parent_code1_parent) # Clean up by removing the extra column
    
    # step 3.8: parent_code7
    parent_codes <- parent_codes %>%
      left_join(parent_codes %>% select(code, parent_code1), 
                by = c("parent_code6" = "code"), 
                suffix = c("", "_parent")) %>%
      mutate(parent_code7 = parent_code1_parent) %>%
      select(-parent_code1_parent) # Clean up by removing the extra column
    
  parent_codes <- parent_codes[-c(4:11),]
  
  #save(parent_codes, file = paste0(int_data,"/parent_codes.RData"))
    
  # step 4: create ppi_final
    
    # step 4.1: create ppi_final in ppi_wide
    ppi_wide$ppi_final <- ppi_wide$ppi_2010
    
    # step 4.2: loop over rows with missing ppi_2010
    missing_rows <- which(is.na(ppi_wide$ppi_2010))
    
    for (i in missing_rows) {
      code <- ppi_wide$code[i]
      year <- ppi_wide$year[i]
      
      # Find all parent codes for this code
      parent_candidates <- parent_codes[parent_codes$code == code, ]
      
      # Iterate over the parent_code variables (parent_code1, parent_code2, ..., parent_code7)
      for (j in 1:7) {
        parent_code <- parent_candidates[[paste0("parent_code", j)]]
        
        # Check if the parent_code has a non-missing ppi_2010 value in the same year
        ppi_value <- ppi_wide$ppi_2010[ppi_wide$code == parent_code & ppi_wide$year == year]
        
        if (length(ppi_value) > 0 && !is.na(ppi_value)) {
          # Assign the first non-missing ppi_2010 from the parent code to ppi_final
          ppi_wide$ppi_final[i] <- ppi_value
          break
        }
      }
    }
  
    # step 4.3: clean and save ppi_wide
    ppi_final <- ppi_wide %>% 
      select(-c(ppi_2010))
  
    save(ppi_final, file = paste0(proc_data,"/ppi_final.RData"))
  