#### HEADER -------

## Creates data set for sector-year emissions, output, value added, emission intensity
# broken down by EUETS, non-EUETS

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

  sector_emissions <- read_csv(paste0(raw_data,"/Eurostat/eurostat_emissions_by_sector_belgium.csv"))
  sector_emission_intensity <- read_csv(paste0(raw_data,"/Eurostat/eurostat_emission_intensity_by_sector_belgium.csv"))
  sector_output <- read_csv(paste0(raw_data,"/Eurostat/eurostat_output_basic_prices_by_sector_belgium.csv"))
  sector_value_added <- read_csv(paste0(raw_data,"/Eurostat/eurostat_value_added_basic_prices_by_sector_belgium.csv"))
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))
  
  # output and value added by sector from FIGARO
  figaro <- data.frame()
  for(y in 2010:2022){
    
    temp <- read_csv(paste0(raw_data,"/FIGARO/figaro_use_table_", y, ".csv")) %>% 
      select(1, starts_with("BE"))
    
    sum_output <- t(colSums(temp[ , 2:(ncol(temp)-5)]))
    value_added <- t(colSums(temp[2948:2950, 2:(ncol(temp)-5)]))
    
    sector_names <- sub("^[^_]*_", "", colnames(temp)[2:(length(colnames(temp)) - 5)])
    
    test <- data.frame(
      nace2d = sector_names,
      year = rep(y, length(sector_names)),
      figaro_value_added = value_added[1,]*10^6,
      figaro_output = sum_output[1,]*10^6
    )
    
    rownames(test) <- NULL
    
    figaro <- rbind(figaro, test)
  }

# Build data frame -----
  
  firm_year_belgian_euets <- firm_year_belgian_euets %>% filter(in_sample == 1)
  firm_year_belgian_euets$nace2d <- substr(firm_year_belgian_euets$nace5d, 1, 2)
  
  euets_firms_by_sector <- firm_year_belgian_euets %>% 
    # make nace codes compatible with eurostat's
    mutate(nace2d = case_when(
      nace2d == "10" | nace2d == "11" ~ "C10-C12", # no firms in sector 12
      nace2d == "13" | nace2d == "15" ~ "C13-C15", # no firms in sector 14
      nace2d %in% as.character(16:33) ~ paste0("C", nace2d),
      nace2d == "31" ~ "C31_C32", # no firms in sector 32
      nace2d == "35" ~ "D",
      nace2d == "38" ~ "E37-E39", # no firms in sector 37, 39
      nace2d == "42" | nace2d == "43" ~ "F",
      nace2d %in% as.character(45:47) ~ paste0("G", nace2d),
      nace2d %in% as.character(49:53) ~ paste0("H", nace2d),
      nace2d == "63" ~ "J62_J63", # no firms in sector 62
      nace2d == "68" ~ "L68A",
      nace2d == "70" ~ "M69_M70",
      nace2d == "71" ~ "M71",
      nace2d %in% as.character(80:82) ~ "N80-N82"
      )
    ) %>% 
    group_by(year,nace2d) %>% 
    summarize(
      euets_output = sum(revenue, na.rm = TRUE),     
      euets_emissions = sum(emissions, na.rm = TRUE),
      euets_value_added = sum(value_added, na.rm = TRUE),
      n_euets_firms = n(), 
      .groups = 'drop'
    )
  
  sector_emissions <- sector_emissions %>%
    select(c(4,5,8,9)) %>% 
    rename(gas = 1, nace2d = 2, year = 3, emissions = 4) %>% 
    mutate(gas = sub(":.*", "", gas),
           nace2d = sub(":.*", "", nace2d)) %>% 
    group_by(nace2d) %>%
    filter(nchar(nace2d) > 1 | nace2d %in% c("B", "D", "F", "I", "L", "O", "P"))
  
  sector_emission_intensity <- sector_emission_intensity %>% 
    select(c(4,5,9,10)) %>% 
    rename(gas = 1, nace2d = 2, year = 3, emission_intensity = 4) %>% 
    mutate(gas = sub(":.*", "", gas),
           nace2d = sub(":.*", "", nace2d)) %>% 
    group_by(nace2d) %>%
    filter(nchar(nace2d) > 1 | nace2d %in% c("B", "D", "F", "I", "L", "O", "P"))
  
  sector_output <- sector_output %>% 
    select(c(5,9,10)) %>% 
    rename(nace2d = 1, year = 2, output_eurostat = 3) %>% 
    mutate(nace2d = sub(":.*", "", nace2d))
  
  sector_value_added <- sector_value_added %>%
    filter(stk_flow == "TOTAL:Total") %>% 
    select(c(5,9,10)) %>%
    rename(nace2d = 1, year = 2, value_added_eurostat = 3) %>% 
    mutate(nace2d = sub(":.*", "", nace2d),
           value_added_eurostat = value_added_eurostat*10^6)
  
  figaro <- figaro %>% 
    mutate(nace2d = case_when(
      nace2d == "C10T12" ~ "C10-C12",
      nace2d == "C13T15" ~ "C13-C15",
      nace2d == "C31_32" ~ "C31_C32",
      nace2d == "D35" ~ "D",
      nace2d == "E37T39" ~ "E37-E39",
      nace2d == "J59_60" ~ "J59_J60",
      nace2d == "J62_63" ~ "J62_J63",
      nace2d == "M69_70" ~ "M69_M70",
      nace2d == "M74_75" ~ "M74_M75",
      nace2d == "N80T82" ~ "N80-N82",
      nace2d == "Q87_88" ~ "Q87_Q88",
      nace2d == "R90T92" ~ "R90-R92",
      TRUE ~ nace2d
      )
    )
  
  df_sectoral_emissions <- sector_emissions %>% 
    left_join(sector_emission_intensity, by = c("gas", "year", "nace2d")) %>%
    filter(gas == "CO2") %>%
    select(-gas) %>% 
    mutate(emission_intensity = emission_intensity/10^3, # express emission intensity in ton/EUR
           output = ifelse(emission_intensity == 0, NA,
                                   emissions/emission_intensity)) %>% 
    left_join(euets_firms_by_sector, by = c("nace2d", "year")) %>%
    left_join(sector_value_added, by = c("nace2d", "year")) %>% 
    left_join(figaro, by = c("nace2d", "year")) %>% 
    filter(year < 2023)
  
  # Obs: remember units!
  # emissions are in ton of CO2-eq
  # output is in EUR in current prices
  # emission intensity is in ton/EUR
  
  # save it
  save(df_sectoral_emissions, file = paste0(output,"/sector_year_emissions_euets_noneuets.RData"))
  
  
 
   
  
  
  
  





