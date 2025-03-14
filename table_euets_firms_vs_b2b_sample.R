#### HEADER -------

## Creates table that compares EUETS firms with firms in B2B sample, by sector

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

# Import data -----

load(paste0(proc_data,"/firm_year_balance_sheet_selected_sample.RData"))
load(paste0(proc_data,"/firm_year_belgian_euets.RData"))
load(paste0(output,"/sector_year_emissions_euets_noneuets.RData"))

# Get agg. numbers from firms in sample ----

total_selected_sample <- firm_year_balance_sheet_selected_sample %>%
  mutate(nace2d = substr(nace5d, 1, 2)) %>% 
  mutate(nace2d = case_when(
    nace2d %in% as.character(10:12) ~ "C10-C12",
    nace2d %in% as.character(13:15) ~ "C13-C15",
    nace2d %in% as.character(16:33) ~ paste0("C", nace2d),
    nace2d %in% as.character(31:32) ~ "C31_C32",
    nace2d == "35" ~ "D",
    nace2d == "36" ~ "E36",
    nace2d %in% as.character(37:39) ~ "E37-E39",
    nace2d %in% as.character(41:43) ~ "F",
    nace2d %in% as.character(45:47) ~ paste0("G", nace2d),
    nace2d %in% as.character(49:53) ~ paste0("H", nace2d),
    nace2d %in% as.character(62:63) ~ "J62_J63",
    nace2d == "68" ~ "L68A",
    nace2d %in% as.character(69:70) ~ "M69_M70",
    nace2d == "71" ~ "M71",
    nace2d %in% as.character(80:82) ~ "N80-N82",
    TRUE ~ nace2d
    )
  ) %>% 
  group_by(year, nace2d) %>% 
  summarise(
    total_turnover = sum(turnover, na.rm = TRUE),
    total_value_added = sum(value_added, na.rm = TRUE),
    unique_vat_ano_count = n_distinct(vat_ano)
  ) %>% 
  filter(year >= 2002)

# Build table -----

data_for_table <- df_sectoral_emissions %>% 
  left_join(total_selected_sample, by = c("year", "nace2d")) %>% 
  rename(in_sample_output = total_turnover,
         in_sample_value_added = total_value_added,
         n_in_sample_firms = unique_vat_ano_count)

data_for_table <- data_for_table %>% 
  mutate(
    across(10:16, ~ round(as.numeric(.x) / 10^9))
  )



