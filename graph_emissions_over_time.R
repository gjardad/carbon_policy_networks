#### HEADER -------

## This 

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

# Import data ------

load(paste0(proc_data,"/firm_year_input_bundle_euets.RData"))

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

df_owindata <- read_csv(paste0(raw_data,"/annual-co2-emissions-per-country.csv"))

# Clean data set -----

firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
  mutate(nace2d = str_sub(nace5d, 1, 2)) %>% 
  filter(year >= 2005) %>%

firm_year_input_bundle_euets <- firm_year_input_bundle_euets %>% 
  left_join(firm_year_belgian_euets %>% select(vat, year, emissions),
            by = c("vat", "year")) %>%  
  ungroup()

df_owindata <- df_owindata %>% 
  filter((Entity == "European Union (28)" | Entity == "Belgium") & Year >= 1990) %>% 
  rename(country = Entity, year = Year, emissions = 4) %>% 
  select(-c(Code)) %>%
  group_by(country) %>% 
  mutate(emissions = emissions/emissions[year == 2005]*100)
  

# Graphs -------

  library(ggplot2)

  # evolution of EUETS emissions
  df_total_emissions <- firm_year_input_bundle_euets %>%
    group_by(year) %>%
    summarize(total_emissions = sum(emissions, na.rm = TRUE))
  
  ggplot(df_total_emissions, aes(x = year, y = total_emissions)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_point(size = 2, color = "red") +  # Add points for each year
    labs(title = "Total Emissions Over Time", 
         x = "Year", 
         y = "Total Emissions") +
    theme_minimal()

  # by sector

  # evolution of number of firms
  n_firms <- firm_year_input_bundle_euets %>% 
    group_by(year) %>% 
    summarize(count = n_distinct(vat))
  
  ggplot(n_firms, aes(x = year, y = count)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_point(size = 2, color = "red") +  # Add points for each year
    labs(title = "", 
         x = "Year", 
         y = "Number of firms") +
    theme_minimal()
