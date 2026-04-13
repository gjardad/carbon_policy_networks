#### HEADER -------

## This code creates data set that is used in regs for direct effects of EUETS
# It contains firm-year level info on
#1. NACE sector
#3. EUETS activity
#4. emission intensity
#5. output
#6. exp share dirty inputs
#7. allowance shortage
# for firms directly affected by the EUETS

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

load(paste0(proc_data,"/firm_year_real_output_2005.RData"))
load(paste0(proc_data,"/firm_year_labor_costs.RData"))
load(paste0(proc_data,"/firm_year_input_cost.RData"))
load(paste0(proc_data,"/firm_year_total_imports.RData"))
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

carbon_prices <- read_csv(paste0(raw_data, "/icap_price.csv")) %>% 
  select(1,3) %>% 
  rename(date = 1, carbon_price = 2) %>% 
  slice(-1)

# Create df -------------

  # average price per year
  carbon_prices <- carbon_prices %>%
    # Convert 'date' to Date class and 'carbon_price' to numeric
    mutate(date = as.Date(date, format = "%Y-%m-%d"),
           carbon_price = as.numeric(carbon_price)) %>%
    # Extract year and calculate average price per year
    group_by(year = year(date)) %>%
    summarise(carbon_price = mean(carbon_price, na.rm = TRUE)) %>%
    ungroup()

  df_direct_euets_effects <- firm_year_belgian_euets %>%
    rename(vat_ano = vat) %>% 
    left_join(firm_year_real_output_2005, by = c("vat_ano", "year")) %>% 
    rename(vat = vat_ano) %>% 
    select(-c(revenue, nace2d, nace3d, capital, wage_bill, fte, hours, nace5d.x,
              emissions_prod, capital_prod, labor_prod, price_index, nace4d,
              shortage_prod, bvd_id)) %>% 
    rename(nace5d = nace5d.y) %>% 
    left_join(carbon_prices, by = "year")
  
  df_direct_euets_effects <- df_direct_euets_effects %>%
    left_join(firm_year_input_cost, by=c("vat", "year")) %>%
    rename(vat_ano = vat) %>% 
    left_join(firm_year_labor_costs, by=c("vat_ano", "year")) %>% 
    left_join(firm_year_total_imports, by = c("vat_ano", "year")) %>% 
    rename(vat = vat_ano) %>% 
    group_by(vat, year) %>% 
    mutate(total_input_cost = sum(input_cost, labor, total_imports, na.rm = TRUE)) %>%
    ungroup() %>% 
    mutate(allowance_shortage = allowance_shortage*carbon_price / total_input_cost)
  
# Save it ------
save(df_direct_euets_effects, file = paste0(proc_data,"/df_direct_euets_effects.RData"))

