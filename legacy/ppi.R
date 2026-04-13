#### HEADER -------

## This code creates a producer prices index at the sector-year level for services,
# industry and construction

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

# Import data ----

load(paste0(proc_data,"/ippi_final.RData"))

load(paste0(proc_data,"/sppi_final.RData"))

load(paste0(proc_data,"/cppi_final.RData"))

load(paste0(proc_data,"/nace_hicp_data.RData"))

library(readxl)
nace_codes <- read_excel(paste0(raw_data, "/list_nace_codes.xlsx")) %>% 
  slice(-c(1:5)) %>%
  select(2) %>% 
  rename(code = 1) %>% 
  mutate(code = as.numeric(code)) %>% 
  mutate(across(where(is.numeric), round, digits = 2)) %>% 
  mutate(code = as.character(code)) %>% 
  mutate(
    code = case_when(
      str_detect(code, "^\\d\\.") ~ str_pad(code, width = nchar(code) + 1, pad = "0", side = "left"),  # Add zero for 1 digit before "."
      !str_detect(code, "\\.") & nchar(code) == 1 ~ paste0("0", code),  # Add zero if no "." and only one character
      TRUE ~ code  # Keep as is if it doesn't meet the other conditions
    ),
    code = str_replace_all(code, "\\.", "")
  )

nace_codes <- nace_codes %>% 
  filter(!is.na(code))

# Merge ppis to nace codes

  ippi_nace <- ippi_final %>%
    filter(str_detect(code, "^[A-Za-z]\\d+$")) %>%  # ^[A-Za-z] ensures the first character is a letter, \\d+ checks for following numbers
    mutate(code = str_replace(code, "^[A-Za-z]", "")) %>% 
    rename(price_index = ippi_final)

  sppi_nace <- sppi_final %>% 
    filter(str_detect(code, "^[A-Za-z]\\d+$")) %>%  # ^[A-Za-z] ensures the first character is a letter, \\d+ checks for following numbers
    mutate(code = str_replace(code, "^[A-Za-z]", "")) %>% 
    rename(price_index = sppi_final)
  
  # simply stack them because the codes in each one of them are mutually excludent
  price_indices <- rbind(ippi_nace, sppi_nace, cppi)
  
  ppi <- nace_codes %>%
    expand_grid(year = 2005:2022)
  
  # populate price_index of nace codes using 
  ppi <- ppi %>% 
    left_join(price_indices, by=c("code", "year"))
  
  # if disaggregated NACE has no correspondence with price_indices, but parent code does,
  # then populate using parent code
  
  # 3-digit parent codes for 4-digit NACE codes, and 2-digit parent codes for 3-digit NACE codes
  ppi <- ppi %>% 
    mutate(parent1 = str_sub(code, 1, -2)) %>%
    left_join(price_indices, by = c("parent1" = "code", "year"), suffix = c("", ".parent1")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index.parent1, price_index)) %>%  # Fill NA with the new price_index
    select(-price_index.parent1)
  
  # 2-digit parent codes for 4-digit NACE codes
  ppi <- ppi %>% 
    mutate(parent2 = ifelse(nchar(code) == 4, str_sub(code, 1, 2), NA)) %>%
    left_join(price_indices, by = c("parent2" = "code", "year"), suffix = c("", ".parent2")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index.parent2, price_index)) %>%  # Fill NA with the new price_index
    select(-price_index.parent2)
  
  ppi <- ppi %>% 
    select(-c(parent1, parent2))
  
  ppi <- ppi %>% 
    distinct()
  
  # impute price_index in 2005 for codes for which it is missing
  ppi <- ppi %>%
    group_by(code) %>%
    mutate(
      # Step 2: Calculate percentual variation between 2006 and 2007
      perc_variation = ifelse(year == 2006 & !is.na(price_index), 
                              (lead(price_index) - price_index) / price_index, 
                              NA)
    ) %>%
    # Step 3: For the rows where price_index is missing in 2005, calculate the imputed value
    mutate(
      price_index = ifelse(year == 2005 & is.na(price_index),
                           price_index[year == 2006] / (1 + perc_variation[year == 2006]),
                           price_index)
    ) %>%
    ungroup() %>% 
    select(-perc_variation)
  
  # add data from hicp for sectors for which ppi not available
  
  # nace_hicp only includes 4-digit NACE codes, so this connects 4-digit NACE in ppi
  # with 4-digit NACE in nace_hicp
  ppi <- ppi %>% 
    left_join(nace_hicp_data %>% select(nace, year, price_index),
              by = c("code" = "nace", "year"), suffix = c("", ".hicp")) %>% 
    mutate(price_index = ifelse(is.na(price_index), price_index.hicp, price_index)) %>% 
    select(-price_index.hicp)
  
  # some 4-digit NACE in ppi still have no price_index
  # for those, calculate average among sectors
    # 1. that share same first 3/2 digits; and
    # 2. for which we do have price_index
  
  nace_hicp_3d <- nace_hicp_data %>% 
    mutate(parent1 = str_sub(nace, 1, 3)) %>% 
    group_by(parent1, year) %>% 
    summarize(price_index = mean(price_index, na.rm = TRUE)) %>% 
    ungroup()
  
  ppi <- ppi %>% 
    mutate(parent1 = ifelse(nchar(code) == 4, str_sub(code, 1, 3), NA)) %>% 
    left_join(nace_hicp_3d, by = c("parent1", "year"), suffix = c("", ".parent1")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index.parent1, price_index)) %>%  # Fill NA with the new price_index
    select(-c(price_index.parent1, parent1))
  
  nace_hicp_2d <- nace_hicp_data %>% 
    mutate(parent2 = str_sub(nace, 1, 2)) %>% 
    group_by(parent2, year) %>% 
    summarize(price_index = mean(price_index, na.rm = TRUE)) %>% 
    ungroup()
  
  ppi <- ppi %>% 
    mutate(parent2 = ifelse(nchar(code) == 4, str_sub(code, 1, 2), NA)) %>% 
    left_join(nace_hicp_2d, by = c("parent2", "year"), suffix = c("", ".parent2")) %>%
    mutate(price_index = ifelse(is.na(price_index), price_index.parent2, price_index)) %>%  # Fill NA with the new price_index
    select(-c(price_index.parent2, parent2))
  
# save it ------
  save(ppi, file = paste0(proc_data,"/ppi_final.RData"))
  