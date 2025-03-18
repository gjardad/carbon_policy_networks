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
library(kableExtra)

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

desired_order <- c("nace2d", "emissions", "euets_output", "output", 
                   "value_added_eurostat", "figaro_value_added", 
                   "figaro_output", "in_sample_output", "in_sample_value_added")

data_for_table_2012 <- data_for_table %>%
  filter(year == 2012, !is.na(euets_output)) %>% 
  mutate(
    across(c(4,5,7,10:13), ~ round(as.numeric(.x) / 10^9, 2)), # output and value added in bi of EUR
    across(c("emissions", "euets_emissions"), ~ round(as.numeric(.x) / 10^3, 2)), # emissions in mi of ton
    agg_count = "$-$",
    sample_emissions = "$-$",
    nace_names = case_when(
      nace2d == "C10-C12" ~ "Food, Beverages, Tobacco",
      nace2d == "C13-C15" ~ "Textiles, Apparel, Fur, Leather",
      nace2d == "C16" ~ "Wood products",
      nace2d == "C17" ~ "Paper",
      nace2d == "C19" ~ "Coke and refined petroleum",
      nace2d == "C20" ~ "Chemicals",
      nace2d == "C21" ~ "Pharmaceuticals",
      nace2d == "C22" ~ "Rubber and plastic",
      nace2d == "C23" ~ "Non-metallic minerals",
      nace2d == "C24" ~ "Basic metals",
      nace2d == "C25" ~ "Fabricated metals",
      nace2d == "C27" ~ "Electrical equipment",
      nace2d == "C28" ~ "Machinery and equipment",
      nace2d == "C29" ~ "Motor vehicles",
      nace2d == "C30" ~ "Other transport equipment",
      nace2d == "C33" ~ "Repair and installation of machinery",
      nace2d == "D" ~ "Electricity, gas, steam, and air conditioning",
      nace2d == "E37-E39" ~ "Sewerage and waste management",
      nace2d == "F" ~ "Construction",
      nace2d == "G45" ~ "Wholesale and retail trade of motor vehicles",
      nace2d == "G46" ~ "Wholesale trade except motor vehicles",
      nace2d == "H49" ~ "Land transport",
      nace2d == "H52" ~ "Warehousing",
      nace2d == "J62_J63" ~ "Computer programming",
      nace2d == "M71" ~ "Architectural and engineering",
      nace2d == "N80-N82" ~ "Security and investigation",
      TRUE ~ NA
    ) 
  ) %>% 
  ungroup()

desired_order <- c("nace_names", "agg_count", "n_in_sample_firms", "n_euets_firms", 
                   "figaro_value_added", "in_sample_value_added", "euets_value_added", 
                   "figaro_output", "in_sample_output", "euets_output",
                   "emissions", "sample_emissions", "euets_emissions")

test <- data_for_table_2012 %>%
  select(all_of(desired_order))

latex_table <- kable(test, format = "latex", booktabs = TRUE, 
                     caption = "Table Caption Here") %>%
  kable_styling(latex_options = "striped", full_width = F)




