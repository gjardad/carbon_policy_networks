#### HEADER -------

## Creates yearly time series of emissions from energy by CRF category-year

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

# Import data -----
emissions <- c()

for(year in 2005:2022){
  
  temp_sheet1 <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                            sheet = "Table1.A(a)s1")[9:85,c(1,7)]
  
  temp_sheet2 <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                            sheet = "Table1.A(a)s2")[9:87,c(1,7)]
  
  temp_sheet4 <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                            sheet = "Table1.A(a)s4")[9:100,c(1,7)]
  
  colnames(temp_sheet1) <- c("fuel", "emission_factor")
  colnames(temp_sheet2) <- c("fuel", "emission_factor")
  colnames(temp_sheet4) <- c("fuel", "emission_factor")
  
  stacked_temp <- bind_rows(temp_sheet1, temp_sheet2, temp_sheet4)
  
  df <- stacked_temp %>%
    rename(fuel = 1, emissions = 2) %>% 
    mutate(crf_code = ifelse(str_detect(fuel, "^\\d+"),
                             str_extract(fuel, "^\\d+(\\.\\w+(\\.\\w+)*)*"),
                             NA)) %>% 
    fill(crf_code) %>% 
    filter(!str_detect(fuel, "^\\d")) %>% 
    mutate(fuel = case_when(
      fuel == "Liquid fuels" ~ "liquid",
      fuel == "Solid fuels" ~ "solid",
      fuel == "Gaseous fuels (6)" ~ "gas",
      fuel == "Other fossil fuels (7)" ~ "other",
      fuel == "Peat (8)" ~ "peat",
      fuel == "Biomass (3)" ~ "biomass",
      TRUE ~ fuel
    )) %>% 
    mutate(year = year)
  
  emissions <- bind_rows(emissions, df)
  
}

emissions_from_energy_consumption <- emissions %>% 
  mutate(crf_code = paste0(crf_code, ".")) %>% 
  mutate(emissions = ifelse(!is.na(parse_number(emissions)), 
                                  parse_number(emissions), 
                                  NA))

#save(emissions_from_energy_consumption, file = paste0(proc_data, "/emissions_from_energy_consumption_by_crf_year.RData"))




