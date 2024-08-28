#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. emissions
# 2. BvD id
# 3. NACE code
# 4. Belgian VAT
# 5. labor input
# 6. capital input
# 7. revenue

# for all firms in Belgium treated by the EUETS

#####################

# Setup ------
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

# Import data -------

library(haven)
df_belgium_vat <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

df_firm_emissions <- load(paste0(proc_data, "/firm_year_emissions.RData"))

df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

# Clean Data ------

firm_year_belgian_euets <- firm_year_emissions %>% 
  left_join(df_belgium_vat %>% select(firm_id, vat_ano), by = "firm_id") %>% 
  filter(!is.na(vat_ano)) %>% 
  distinct() %>% 
  left_join(df_national_accounts %>%  select(vat_ano, year, 
                                             v_0022_27,v_0000070, v_0001033,
                                             v_0001003, v_0001013, nace5d),
            by = c("vat_ano", "year")) %>% 
  rename(vat = vat_ano, capital = v_0022_27, revenue = v_0000070,
         fte = v_0001003, wage_bill = v_0001033, hours = v_0001013) %>% 
  mutate(emission_prod = ifelse(revenue == 0, 0, log(revenue/emissions)),
         labor_prod = ifelse(revenue == 0, 0, log(revenue/fte)),
         capital_prod = ifelse(revenue == 0, 0, log(revenue/capital)))

# Save it -------
save(firm_year_belgian_euets, file = paste0(proc_data,"/firm_year_belgian_euets.RData"))  


