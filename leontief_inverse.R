#### HEADER -------

## This code creates the Leontief inverse matrix for each year

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
library(purrr)

# Import data ------

load(paste0(proc_data, "/b2b_sample.RData"))
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/firm_year_labor_costs.RData"))
load(paste0(proc_data, "/euets_prices_year.RData"))

library(haven)
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))

# Create emission costs ----

df_price_year$year <- as.numeric(df_price_year$year)

emissions_costs <- firm_year_belgian_euets %>% 
  select(vat, year, emissions, allowance_shortage) %>% 
  left_join(df_price_year %>% select(year, nominal_price),
            by = "year") %>% 
  mutate(emissions = emissions*nominal_price,
         allowance_shortage = allowance_shortage*nominal_price) %>% 
  select(-nominal_price) %>% 
  filter(year <= 2022)

  # make allowance_shortage = 0 if negative
  # this is an assumption! I don't know if firm collects revenue from selling its allowances
  # when allowances > emissions

  emissions_costs <- emissions_costs %>% 
    mutate(allowance_shortage = ifelse(allowance_shortage < 0 , 0, allowance_shortage))

# Create Leontief inverse -------
  
leontief_list <- list()
i <- 0

for(y in 2005:2022){
  
  # Step 1: widen B2B
  df <- df_b2b %>%
    filter(year == y)
  
  firms <- unique(c(df$vat_j_ano, df$vat_i_ano))

  df_complete <- expand.grid(
    vat_j_ano = firms,
    vat_i_ano = firms,
    year = y
  )
  
  df_complete <- df_complete %>% 
    left_join(df %>% select(vat_j_ano, vat_i_ano, year, corr_sales_ij),
              by = c("vat_j_ano", "vat_i_ano", "year")) %>%
    mutate(corr_sales_ij = replace_na(corr_sales_ij, 0))
  
  transaction_matrix <- df_complete %>%
    select(vat_i_ano, vat_j_ano, corr_sales_ij) %>%
    pivot_wider(names_from = vat_i_ano, values_from = corr_sales_ij) %>%
    rename(vat = vat_j_ano) %>%
    mutate(year = y) %>% 
    # left_join with data on emissions costs instead
    left_join(emissions_costs,
              by = c("vat", "year")) %>%
    rename(vat_ano = vat) %>% 
    left_join(firm_year_labor_costs, by = c("vat_ano", "year")) %>%
    rename(buyer = vat_ano) %>% 
    column_to_rownames(var = "buyer") %>% 
    select(-year)
  
  # Step 2: add emissions and shortage "sectors"
  emissions <- rep(0, ncol(transaction_matrix))
  shortage <- rep(0, ncol(transaction_matrix))
  
  transaction_matrix <- transaction_matrix %>% 
    rbind(emissions, shortage)
  
  # Step 3: make costs relative to total input costs
  transaction_matrix <- transaction_matrix %>% 
    mutate(input_cost = rowSums(across(where(is.numeric)), na.rm = TRUE),
           across(where(is.numeric), ~ .x / input_cost),
           across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
    select(-c(input_cost, labor)) 
    
  # Step 4: inverse
  I_matrix <- diag(1, nrow(transaction_matrix)) # Create identity matrix of correct size
  leontief_inverse <- solve(I_matrix - transaction_matrix)
  
  i = i + 1
  
  leontief_list[[i]] <- leontief_inverse

}

# Save it -------------------
save(leontief_list, file = "leontief_inverse.RData")
