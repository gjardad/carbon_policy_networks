#### HEADER -------

## This code creates two data sets on emissions at the sector-year level for Belgium

  # 1. sector_year_emissions_all_gases
    
    # auxiliary data set that includes emissions (tCO2eq) for each sector (NACE2d), year,
    # gas from Eurostat

  # 2. euets_sector_year_emissions

    # main data set that includes total GHG (tCO2eq) for each sector (NACE2d) and year,
    # broken down by EUETS and non-EUETS volumes

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
library(stringr)

# Import data ------

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

  # is sector_year_emissions data set ready? T for yes, F for no
  sector_year_emissions_ready <- T
  
  if(!sector_year_emissions_ready){
    
    # sector-year data from Eurostat
    sector_year_emissions <- data.frame()
    
    # Find data sets within raw_data/Eurostat folder that include ghg_annual_belgium
    file_folder <- paste0(raw_data, "/Eurostat")
    file_names <- list.files(path = file_folder, pattern = "^ghg_annual_belgium", full.names = FALSE)
    
    # Loop through each data set
    for (file in file_names){
      
      file_path <- paste0(file_folder,"/", file)
      
      # Get the names of all sheets
      sheet_names <- excel_sheets(file_path)
      sheet_names <- sheet_names[3:length(sheet_names)] # no data in first two
      
      # Loop through each sheet within data set
      for (sheet in sheet_names) {
        
        # Read the necessary cells from the sheet
        gas <- read_excel(file_path, sheet = sheet, range = "C6", col_names = FALSE)[[1]]
        gas_code <- regmatches(gas, regexpr("(?<=\\[).*?(?=\\])", gas, perl = TRUE))
        sector <- read_excel(file_path, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
        sector_code <- regmatches(sector, regexpr("(?<=\\[).*?(?=\\])", sector, perl = TRUE))
        years <- seq(2008, 2022)  # Years from 2008 to 2022
        emissions <- read_excel(file_path, sheet = sheet, range = "AC12:BE12", col_names = FALSE)
        emissions <- emissions[, seq(1, ncol(emissions), by = 2)]
        
        # Reshape the data and append to the main data frame
        temp_df <- data.frame(
          sector = sector_code,
          gas = gas_code,
          year = years,
          emissions = as.numeric(emissions)
        )
        
        # Append to the main dataset
        sector_year_emissions <- rbind(sector_year_emissions, temp_df)
      }
    }
    
    save(sector_year_emissions, file = paste0(proc_data,"/sector_year_emissions_all_gases.RData"))
    
  } else {
    load(paste0(proc_data,"/sector_year_emissions_all_gases.RData"))
  }
  
# Clean data -----
  
  # what are the sectors included in BE EUETS among firms included in the sample?
  firm_year_belgian_euets$nace2d <- substr(firm_year_belgian_euets$nace5d, 1, 2)
  firm_year_belgian_euets_in_sample <- firm_year_belgian_euets %>% 
    filter(in_sample == 1)
    
  euets_sectors <- unique(firm_year_belgian_euets_in_sample$nace2d)
  
  sector_year_emissions_euets <- firm_year_belgian_euets_in_sample %>% 
    group_by(nace2d, year) %>% 
    summarize(emissions_euets = sum(emissions, na.rm = TRUE), .groups = 'drop') %>% 
    filter(!is.na(nace2d))
  
  # Eurostat emissions for the EUETS sectors
  eurostat_emissions_euets <- sector_year_emissions %>%
    mutate(nace2d = case_when(
      # Replace specific strings
      sector == "D" ~ "35",
      sector == "F" ~ "41-43",
      # Keep codes without numbers as they are
      !str_detect(sector, "\\d") ~ sector,
      # Pattern to match the desired format and transform it
      TRUE ~ str_replace_all(str_remove_all(sector, "[A-Z]+"), "_", "-") %>%
        str_replace_all("(\\d+)-(\\d+)", "\\1-\\2")
    )) %>%
    # Filter out observations where sector_number does not contain any digits
    filter(str_detect(nace2d, "\\d"),
           gas == "GHG") %>% 
    select(-c(sector,gas)) %>% 
    rename(emissions_eurostat = emissions)

  # make sector categorization in EUETS consistent with Eurostat
  euets_sector_year_emissions <- sector_year_emissions_euets %>%
    mutate(nace2d = case_when(
      nace2d == "31" ~ "31-32",
      nace2d == "38" ~ "37-39",
      nace2d %in% c("41", "42", "43") ~ "41-43",
      nace2d == "63" ~ "62-63",
      nace2d == "70" ~ "69-70",
      nace2d %in% c("81", "82") ~ "80-82",
      nace2d %in% c("10", "11") ~ "10-12",
      nace2d %in% c("13", "15") ~ "13-15",
      TRUE ~ nace2d  # Keep other values unchanged
    )) %>% 
    group_by(nace2d, year) %>% 
    summarize(emissions_euets = sum(emissions_euets, na.rm = TRUE), .groups = 'drop') %>%
    left_join(eurostat_emissions_euets, by = c("nace2d", "year")) %>% 
    filter(year >= 2008)
  
  euets_sector_year_emissions$emissions_noneuets <- euets_sector_year_emissions$emissions_eurostat - euets_sector_year_emissions$emissions_euets
  
  euets_sector_year_emissions <- euets_sector_year_emissions %>%
    group_by(year) %>%
    summarise(
      nace2d = "euets_sectors",
      emissions_euets = sum(emissions_euets, na.rm = TRUE),  # Sum across the group
      emissions_eurostat = sum(emissions_eurostat, na.rm = TRUE),  # Sum across the group
      emissions_noneuets = emissions_eurostat - emissions_euets
    ) %>% 
    bind_rows(euets_sector_year_emissions)
  
# save it ----
save(euets_sector_year_emissions, file = paste0(proc_data,"/euets_sector_year_emissions.RData"))  
  