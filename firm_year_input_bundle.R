#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. expenditure (in Euros) on each NACE code

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

load(paste0(proc_data,"/b2b_sample.RData"))

library(haven)
df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

df_ppi <- 1

# Clean data -------

long <- df_b2b_sample %>%
  rename(vat_ano = vat_i_ano) %>% 
  left_join(df_national_accounts %>% select(vat_ano, year, nace5d),
            by = c("vat_ano", "year")) %>%
  rename(vat_i_ano = vat_ano) %>% 
  group_by(vat_j_ano, year, nace5d) %>%
  summarize(expenditure = sum(corr_sales_ij, na.rm = TRUE)) %>% 
  rename(vat = vat_j_ano) %>% 
  mutate(nace4d = substr(nace5d,1,4)) %>% 
  left_join(ppi, by = c("year", "nace4d")) %>% 
  mutate(real_expenditure = expenditure/ppi*100)

firm_year_input_bundle <- long %>% 
  pivot_wider(names_from = nace5d, 
              values_from = c(expenditure, real_expenditure),
              values_fill = list(expenditure = 0, real_expenditure = 0))

firm_year_input_bundle <- firm_year_input_bundle %>%
  rename_with(~gsub("expenditure", "exp_nace5d_", .), starts_with("expenditure")) %>%
  rename_with(~gsub("real_expenditure", "real_exp_", .), starts_with("real_expenditure"))

# Save it ------
save(firm_year_input_bundle, file = paste0(proc_data,"/firm_year_input_bundle.RData"))
