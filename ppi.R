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

  # there are two "Manufacture of furniture" sectors: C31 and C310; drop duplicates
  df_furniture <- ppi_data %>%
    filter(sector == "Manufacture of furniture") %>%
    group_by(year, index_year) %>%
    mutate(n_duplicates = n()) %>%
    filter(
      # Keep both if both have a non-NA price_index
      n_duplicates == 1 | (!is.na(price_index) & n_duplicates == 2) |
        # If both have NA price_index, keep one randomly
        (is.na(price_index) & all(is.na(price_index)))
    ) %>%
    slice_sample(n = 1) %>%  # Randomly select one if both price_index are NA
    ungroup() %>% 
    select(-c(n_duplicates))
  
  # Combine the filtered furniture data with all other sectors
  ppi_data <- ppi_data %>%
    filter(sector != "Manufacture of furniture") %>%
    bind_rows(df_furniture) # this places "Manufacture of furniture" obs in diff position
  
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
    ungroup() 
  
  #save(ppi_wide, file = paste0(int_data,"/ppi_wide.RData"))
  
# Create most disaggregated ppi by sector-year
  
  # build variable parent_code1, ..., parent_coden which represent the higher levels of each nace code
  # build array that contains all the codes (including parent_codes) for which ppi_2010 is non-missing for any given year
  
  # for each code c and year y:
    # if ppi_2010 is non-missing, then set ppi_collapsed to the value of ppi_2010 in c-y
    # if ppi_2010 is missing, check if parent_code1 is in the list of codes for which ppi_2010 is non-missing in y
      # if parent_code1 is there, then set the value of ppi_collapsed in c-y to the value of ppi_2010 in parent_code1 year y
    # else, check if parent_code2 is in the list of codes for which ppi_2010 is non-missing in y
    # do this iterative procedure until you fill all the ppi_collapsed.
  
  
  get_parent_code <- function(code) {
    if (nchar(code) > 1) {
      return(sub(".$", "", code))
    }
    return(NA)  # Return NA for top-level codes (e.g., "B")
  }
  
  # Sort by code and year
  ppi_wide <- ppi_wide %>% arrange(code, year)
  
  # Create a new column ppi_collapsed
  ppi_wide <- ppi_wide %>%
    group_by(year) %>%
    mutate(ppi_collapsed = ppi_2010)  # Start by setting ppi_collapsed equal to ppi_2010
  
  # Fill missing ppi_collapsed based on the hierarchy
  for (i in 1:nrow(ppi_wide)) {
    if (is.na(ppi_wide$ppi_collapsed[i])) {
      parent_code <- get_parent_code(ppi_wide$code[i])
      while (!is.na(parent_code)) {
        parent_value <- ppi_wide %>%
          filter(code == parent_code, year == ppi_wide$year[i]) %>%
          pull(ppi_2010)
        
        if (length(parent_value) > 0 && !is.na(parent_value)) {
          ppi_wide$ppi_collapsed[i] <- parent_value
          break
        }
        parent_code <- get_parent_code(parent_code)  # Move up the hierarchy
      }
    }
  }
  
  # View the result
  print(ppi_wide)
  
  
  
  
  
  
  
  
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
  
  # Populate obs for which code_levels are non-existent
  #did this by hand looking at the labels and the name of the sectors according to
  # https://ec.europa.eu/eurostat/databrowser/view/sts_inppd_a__custom_12844308/default/table?lang=en
  # which is Eurostat page for "Producer prices in industry, domestic market - annual data"
  
  ppi_merge <- ppi_megre %>% 
    mutate(
      code_level1 = case_when(
        sector == "Industry (except construction, sewerage, waste management and remediation activities)" ~ "B-E36",
        sector == "Industry (except construction, sewerage, waste management and remediation activities), except food, beverages and tobacco)" ~ "B-E36_X_FOOD",
        sector == "Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply" ~ "B-D",
        sector == "Mining and quarrying; manufacturing" ~ "B_C",
        sector == "Mining and quarrying; manufacturing (except MIG energy)" ~ "B_C_X_MIG_NRG",
        sector == "Mining and quarrying; manufacturing (except MIG energy), except food, beverages and tobacco" ~ "B_C_X_FD_MIG_NRG",
        sector == "Mining and quarrying" ~ "B",
        sector == "Manufacturing" ~ "C",
        sector == "Manufacture of food products; beverages and tobacco products" ~ "C10-C12",
        sector == "Manufacture of food products and beverages" ~ "C10_C11",
        sector == "Manufacture of textiles, wearing apparel, leather and related products" ~ "C13-C15",
        sector == "Manufacture of textiles and wearing apparel" ~ "C13_C14",
        sector == "Manufacture of wood, paper, printing and reproduction" ~ "C16-C18",
        sector == "Manufacture of paper and paper products; printing and reproduction of recorded media" ~ "C17_C18",
        sector == "Manufacture of chemicals and chemical products; basic pharmaceutical products and pharmaceutical preparations" ~ "C20_C21",
        sector == 
          TRUE ~ code_level1  # Keep existing value if none of the conditions are met
      )
      
    )
  
  
  