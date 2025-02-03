#### HEADER -------

## Creates data set with Belgian national aggregates that is comparable to
# selected sample from annual accounts

# The data set contains for the yeras 2002, 2012, and 2022
  # 1. total output
  # 2. GDP
  # 3. Wage mass
  # 4. Emissions
  # 5. Emissions from the EUETS sectors
  # 6. Imports
  # 7. Exports

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

# Belgian GDP, output, and compensation ----
  file <- paste0(raw_data, "/Eurostat/eurostat_belgian_national_aggregates_level1agg_2002-22.xlsx")
  
  sheets <- excel_sheets(file)
  sheets <- sheets[3:length(sheets)]
  
  df_belgian_national_agg <- c()
  
  for(sheet in sheets){
    
    sector <- read_excel(file, sheet = sheet, range = "C7", col_names = FALSE)[[1]]
    outcome <- read_excel(file, sheet = sheet, range = "C8", col_names = FALSE)[[1]]
    #years <- seq(2002,2023,1)
    years <- c(2002,2012,2022)
    #values <- read_excel(file, sheet = sheet, range = "B12:AS12", col_names = FALSE)
    values <- read_excel(file, sheet = sheet, range = "B12:G12", col_names = FALSE)
    values <- values[, seq(1, ncol(values), by = 2)]
    values[values == "p"] <- NA
    values <- as.vector(unlist(as.numeric(values)))
    
    dataset <- data.frame(
      year = years, 
      sector = rep(sector, times = length(years)),  # Repeat sector for all years
      outcome = rep(outcome, times = length(years)), # Repeat outcome for all years
      value = values
    )
    
    df_belgian_national_agg <- bind_rows(df_belgian_national_agg, dataset)
    
  }
  
  df_belgian_national_agg <- df_belgian_national_agg %>%
    mutate(outcome = case_when(
      outcome == "B1G" ~ "value_added",
      outcome == "P1" ~ "output",
      outcome == "D11" ~ "compensation",
      TRUE ~ outcome
    )) %>%
    group_by(year, outcome) %>%
    summarise(value = sum(value[sector == "TOTAL"]) - sum(value[sector == "K"]) -
                sum(value[sector == "O"]),
              sector = "total_minus_fin_gov") %>%
    ungroup()

# Belgian imports and exports 2012 and 2022 -----
  file <- paste0(raw_data, "/Eurostat/eurostat_belgian_imp_exp_by_nace_agglevel1_12-22.xlsx")
  
  sheets <- excel_sheets(file)
  sheets <- sheets[3:length(sheets)]
  
  df_belgian_imp_exp <- c()
  
  for(sheet in sheets){
    
    sector <- read_excel(file, sheet = sheet, range = "C9", col_names = FALSE)[[1]]
    outcome <- read_excel(file, sheet = sheet, range = "C8", col_names = FALSE)[[1]]
    #years <- seq(2002,2023,1)
    years <- c(2012,2022)
    values <- read_excel(file, sheet = sheet, range = "B14:E14", col_names = FALSE)
    values <- values[, seq(1, ncol(values), by = 2)]
    values[values == ":"] <- NA
    values <- as.vector(unlist(as.numeric(values)))
    
    dataset <- data.frame(
      year = years, 
      sector = rep(sector, times = length(years)),  # Repeat sector for all years
      outcome = rep(outcome, times = length(years)), # Repeat outcome for all years
      value = values
    )
    
    df_belgian_imp_exp <- bind_rows(df_belgian_imp_exp, dataset)
    
  }
  
  df_belgian_imp_exp <- df_belgian_imp_exp %>%
    mutate(outcome = case_when(
      outcome == "IMP" ~ "imports",
      outcome == "EXP" ~ "exports",
      TRUE ~ outcome
    )) %>%
    group_by(year, outcome) %>%
    summarise(value = sum(value[sector == "TOTAL"]) - sum(value[sector == "K"], na.rm=T),
              sector = "total_minus_fin_gov") %>%
    mutate(value = value/10^3) %>% 
    ungroup()
  
  df_belgian_national_agg <- bind_rows(df_belgian_national_agg, df_belgian_imp_exp)
  
  # make it wide
  df_belgian_national_agg <- df_belgian_national_agg %>% 
    filter(sector == "total_minus_fin_gov") %>%
    select(-sector) %>% 
    pivot_wider(names_from = outcome,
                values_from = value,
                values_fill = 0)

# Belgian emissions ----

  # EUETS sectors 2012 and 2022
  load(paste0(proc_data,"/euets_sector_year_emissions.RData"))
  
  df_belgian_agg_emissions <- euets_sector_year_emissions %>% 
    filter(year == 2012 & nace2d == "euets_sectors"| year == 2022 & nace2d == "euets_sectors") %>% 
    select(year, emissions_euets, emissions_eurostat) %>% 
    rename(euets_firms = emissions_euets, euets_sectors = emissions_eurostat)
  # euets_firms covers all emissions from EUETS firms
  # euets_sectors covers total emissions from sectors covered by EUETS

  # total emissions
  #load(paste0(proc_data,"/sector_year_emissions_all_gases.RData")) #Eurostat
  owid <- read_csv(paste0(raw_data,"/owid_annual-co2-emissions-per-country.csv")) %>% 
    filter(Entity == "Belgium") %>% 
    filter(Year == 2002 | Year == 2012 | Year == 2022) %>% 
    select(3,4) %>% 
    rename(year = 1, total_emissions = 2)
  
  df_belgian_agg_emissions <- owid %>% 
    left_join(df_belgian_agg_emissions, by = "year")
  
# Put it all together
  
  df_belgian_national_agg <- bind_cols(df_belgian_national_agg, df_belgian_agg_emissions %>% select(-year))
  
  df_belgian_national_agg$exports <- ifelse(df_belgian_national_agg$year == 2002,
                                            NA, df_belgian_national_agg$exports)
  df_belgian_national_agg$imports <- ifelse(df_belgian_national_agg$year == 2002,
                                            NA, df_belgian_national_agg$imports)
  
  # adjust the units
  # for columns 2-6: bi of current euros
  # for columns 7-9: mi tonnes of C02eq
  df_belgian_national_agg <- df_belgian_national_agg %>% 
    mutate(
      across(2:6, ~ round(as.numeric(.x) / 10^3)), 
      across(7:9, ~ round(as.numeric(.x) / 10^6)) 
    )

# save it ----
save(df_belgian_national_agg, file = paste0(proc_data,"/df_belgian_national_agg.RData"))