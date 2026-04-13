#### HEADER -------

## Creates yearly time series of emissions from industrial process by CRF category-year

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
  
  temp_sheet <- read_excel(paste0(raw_data,"/NIR/BEL-CRT-2024-V1.0-", year, "-20241217-192810_awaiting submission.xlsx"),
                            sheet = "Table2(I).A-H")[9:88, c(1,7)]
  
  df <- temp_sheet %>%
    rename(crf = 1, emissions = 2) %>% 
    mutate(crf_code = ifelse(str_detect(crf, "^\\d+"),
                             str_extract(crf, "^\\d+(\\.\\w+(\\.\\w+)*)*"),
                             NA)) %>% 
    fill(crf_code) %>% 
    mutate(year = year)
  
  emissions <- bind_rows(emissions, df)
  
}

emissions_from_ind_process <- emissions %>% 
  mutate(crf_code = paste0(crf_code, ".")) %>% 
  mutate(emissions = ifelse(!is.na(parse_number(emissions)), 
                            parse_number(emissions), 
                            NA))
# save it
save(emissions_from_ind_process, file = paste0(proc_data, "/emissions_from_ind_process_crf.RData"))




