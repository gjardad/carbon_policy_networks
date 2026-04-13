#### HEADER -------

## This code creates data set at the firm-year level with info on
# imports at disaggregated level

# TO-DO: how to get real value of imports? price data seems very noisy. pc8_to_cn8 updated only up until 2014,
# and even before 2014 there are some cncodes for which no corresponding NACE code. 

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

library(haven)
df_trade <- read_dta(paste0(raw_data,"/NBB/import_export_ANO.dta"))

pc8_to_cn8 <- read_dta(paste0(raw_data, "/cn8_pc8_concord.dta")) %>% 
  select(year, cn8, pc8) %>% 
  mutate(nace4d = str_sub(pc8, 1, 4))

load(paste0(proc_data,"/ppi_final.RData"))

# Firm-level imports ----
  firm_year_imports <- df_trade %>% 
    filter(flow == "I") %>%
    group_by(vat_ano, year, cncode) %>%
    summarise(
      total_cn_value = sum(cn_value, na.rm = TRUE),
      total_cn_weight = sum(cn_weight, na.rm = TRUE),
      total_cn_units = sum(cn_units, na.rm = TRUE)
    ) %>% 
    ungroup()

  # total imports
  firm_year_total_imports <- df_trade %>% 
    filter(flow == "I") %>% 
    group_by(vat_ano, year) %>% 
    summarise(total_imports = sum(cn_value, na.rm = T))
  
  save(firm_year_total_imports, file = paste0(proc_data,"/firm_year_total_imports.RData"))

# EUETS imports ----
  df_belgium_euets <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
    rename(bvd_id = bvdid, firm_id = companyregistrationnumber)
  
  df_belgium_euets <- df_belgium_euets %>% 
    distinct(vat_ano) %>% 
    mutate(is_euets = 1)
  
  imports_euets <- firm_year_imports %>% 
    left_join(df_belgium_euets,
              by = "vat_ano") %>% 
    filter(is_euets == 1) %>%
    select(-c(is_euets)) %>%
    filter(year >= 2005) %>% 
    ungroup()
  
# Deflate EUETS imports ----
  
  nace4d_rev2 <- pc8_to_cn8 %>%
    filter(year >= 2008) %>%
    select(cn8, nace4d) %>%
    rename(nace4d_rev2 = nace4d) %>% 
    distinct(cn8, .keep_all = TRUE) %>%
    rename(cncode = cn8) 
  
  cncode_to_nace <- pc8_to_cn8 %>%
    rename(cncode = cn8) %>% 
    select(cncode) %>% 
    distinct() %>% 
    left_join(nace4d_rev2, by = "cncode")
  
  # fill the NA values
  cncode_to_nace <- cncode_to_nace %>% 
    mutate(filled_val = ifelse(is.na(nace4d_rev2), NA, nace4d_rev2)) %>%
    fill(filled_val) %>%  # Fill NAs with the previous value
    # Now, replace consecutive NAs with the first valid above value
    mutate(
      nace4d_rev2 = ifelse(is.na(nace4d_rev2), filled_val, nace4d_rev2)  # Replace NA with the filled value
    ) %>%
    select(-filled_val) %>% 
    mutate(cncode = as.character(cncode))
  
  imports_euets <- imports_euets %>% 
    select(-c(total_cn_units, total_cn_weight)) %>% 
    left_join(cncode_to_nace, by = "cncode") %>% 
    rename(code = nace4d_rev2) %>% 
    mutate(code3d = str_sub(code, 1, 3))
    
  imports_euets <- imports_euets %>%
    left_join(ppi, by = c("code", "year")) %>% 
    left_join(ppi, by=c("code3d" = "code", "year"),
              suffix = c("", "_3d")) %>% 
    mutate(price_index = ifelse(!is.na(price_index), price_index, price_index_3d),
           real_imports = total_cn_value/price_index * 100) %>% 
    select(vat_ano, year, cncode, real_imports)
  
  # relevant categories
  # CN2: 25, 26, 27, 28, 29, 36, 38, 39, 40, 44, 45, 47, 48, 69, 70, 72, 73, 74, 75, 76, 78, 79, 83, 84, 85, 86, 87
  
  # some cncodes have missing value because they don't have a corresponding NACE code
  # or because their NACE code didn't have a price index
  
  # no NACE code is due to the fact that pc8_to_cn8 goes up to 2014,
  # so if new code introduced after 2014
  # this code wont have a corresponding NACE
  # indeed missing imports more frequent in more recent years
  
# Wide matrices -----

  library(Matrix)
  
  # Create unique identifiers for rows (vat_ano, year)
  firm_year_imports <- firm_year_imports %>%
    mutate(row_id = as.integer(factor(paste(vat_ano, year, sep = "_"))),
           col_id = as.integer(factor(cncode)))  # Create column ids for cncode
  
  # Create sparse matrix for cn_value
  firm_year_imports_value <- sparseMatrix(
    i = firm_year_imports$row_id,
    j = firm_year_imports$col_id,
    x = firm_year_imports$total_cn_value
  )
  
  # Create sparse matrix for cn_weight
  firm_year_imports_weight <- sparseMatrix(
    i = firm_year_imports$row_id,
    j = firm_year_imports$col_id,
    x = firm_year_imports$total_cn_weight
  )
  
  # Create sparse matrix for cn_units
  firm_year_imports_units <- sparseMatrix(
    i = firm_year_imports$row_id,
    j = firm_year_imports$col_id,
    x = firm_year_imports$total_cn_units
  )
  
  # Extract unique vat_ano-year and cncode values for labels
  unique_vat_ano_year <- unique(firm_year_imports %>% select(vat_ano, year))
  unique_cncode <- unique(firm_year_imports$cncode)
  
  # Set row and column names
  rownames(firm_year_imports_value) <- paste(unique_vat_ano_year$vat_ano, unique_vat_ano_year$year, sep = "_")
  colnames(firm_year_imports_value) <- unique_cncode
  
  rownames(firm_year_imports_weight) <- paste(unique_vat_ano_year$vat_ano, unique_vat_ano_year$year, sep = "_")
  colnames(firm_year_imports_weight) <- unique_cncode
  
  rownames(firm_year_imports_units) <- paste(unique_vat_ano_year$vat_ano, unique_vat_ano_year$year, sep = "_")
  colnames(firm_year_imports_units) <- unique_cncode
  
# save it -----
  save(firm_year_imports_value, file = paste0(proc_data,"/firm_year_imports_value.RData"))
  save(firm_year_imports_weight, file = paste0(proc_data,"/firm_year_imports_weightRData"))
  save(firm_year_imports_units, file = paste0(proc_data,"/firm_year_imports_units.RData"))
  
  
# Wide matrices for EUETS firms -----
  
  firm_year_imports_value_euets <- imports_euets %>%
    filter(!is.na(real_imports)) %>% # excluding cncodes for which we dont have price_index
    pivot_wider(
      names_from = cncode,        
      values_from = real_imports, 
      names_prefix = "real_exp_",
      values_fill = list(real_imports = 0) 
    )
  
  # Create unique identifiers for rows (vat_ano, year)
  imports_euets <- imports_euets %>%
    mutate(row_id = as.integer(factor(paste(vat_ano, year, sep = "_"))),
           col_id = as.integer(factor(cncode)))  # Create column ids for cncode
  
  # Create sparse matrix for cn_value
  firm_year_imports_value_euets <- sparseMatrix(
    i = firm_year_imports$row_id,
    j = firm_year_imports$col_id,
    x = firm_year_imports$total_cn_value
  )
  
# save it -----
  save(firm_year_imports_value_euets, file = paste0(proc_data,"/firm_year_imports_value_euets.RData"))

  
  