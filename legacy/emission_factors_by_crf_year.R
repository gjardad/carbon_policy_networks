#### HEADER -------

## Creates yearly time series of emission factors by CRF category-year

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
emission_factors <- c()

for(year in 2005:2022){

  temp_sheet1 <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                     sheet = "Table1.A(a)s1")[9:85,c(1,4)]
  
  temp_sheet2 <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                            sheet = "Table1.A(a)s2")[9:87,c(1,4)]
  
  temp_sheet4 <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                            sheet = "Table1.A(a)s4")[9:100,c(1,4)]
  
  colnames(temp_sheet1) <- c("fuel", "emission_factor")
  colnames(temp_sheet2) <- c("fuel", "emission_factor")
  colnames(temp_sheet4) <- c("fuel", "emission_factor")
    
  stacked_temp <- bind_rows(temp_sheet1, temp_sheet2, temp_sheet4)
  
  df <- stacked_temp %>%
    rename(fuel = 1, emission_factor = 2) %>% 
    mutate(crf_code = ifelse(str_detect(fuel, "^\\d+"),
                             str_extract(fuel, "^\\d+(\\.\\w+(\\.\\w+)*)*"),
                             NA)) %>% 
    fill(crf_code) %>% 
    filter(!str_detect(fuel, "^\\d")) %>% 
    mutate(fuel = case_when(
      fuel == "Liquid fuels" ~ "liquid",
      fuel == "Diesel oil" ~ "liquid",
      fuel == "Gas/diesel oil" ~ "liquid",
      fuel == "Gasoline" ~ "liquid",
      fuel == "Liquefied petroleum gases (LPG)" ~ "liquid",
      fuel == "Other liquid fuels (please specify)" ~ "liquid",
      fuel == "Residual fuel oil" ~ "liquid",
      fuel == "Solid fuels" ~ "solid",
      fuel == "Gaseous fuels (6)" ~ "gas",
      fuel == "Other fossil fuels (7)" ~ "other",
      fuel == "Other fossil fuels(7)" ~ "other",
      fuel == "Other fossil fuels (please specify)(7)"  ~ "other",
      fuel == "Other non-specified"  ~ "other",
      fuel == "Peat (8)" ~ "peat",
      fuel == "Biomass (3)" ~ "biomass",
      fuel == "Biomass(3)" ~ "biomass",
      TRUE ~ fuel
    ),
    emission_factor = case_when(
      emission_factor == "IE" ~ NA,
      emission_factor == "NO" ~ NA,
      TRUE ~ emission_factor
    ),
    year = year) %>%
    group_by(fuel, crf_code, year) %>% 
    summarise(emission_factor = mean(as.numeric(emission_factor), na.rm = T)) %>% 
    filter(fuel != "Military use") # EF is missing and it only adds a category that I don't want to deal with
  
  emission_factors <- bind_rows(emission_factors, df)
  
}

emission_factors_from_energy_consumption <- emission_factors

# add "." to the end
emission_factors_from_energy_consumption <- emission_factors_from_energy_consumption %>% 
  mutate(crf_code = paste0(crf_code, "."))

# save it
# save(emission_factors_from_energy_consumption, file = paste0(proc_data, "/emission_factors_from_energy_consumption.RData"))




