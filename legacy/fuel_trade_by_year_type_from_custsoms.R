#### HEADER -------

## This code creates total I and X by year and fuel type
# for Belgium from customs data

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
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

load(paste0(proc_data,"/df_trade.RData"))
#load(paste0(proc_data,"/trade_random_sample.RData"))
load(paste0(proc_data,"/hs_codes_for_fossil_fuels.RData"))
load(paste0(proc_data, "/ncv_by_fuel_hs_year.RData"))

# Calculate total imports and exports by fuel category ------------

fuel_trade <- df_trade %>% 
  mutate(cncode_6digit = substr(cncode, 1, 6)) %>% 
  left_join(hs_codes_for_fossil_fuels, by = c("cncode_6digit" = "hs_code")) %>%
  filter(!is.na(hs_description)) %>% 
  group_by(year, cncode_6digit, flow) %>%
  summarise(
    total_cn_value  = sum(cn_value,  na.rm = TRUE),
    total_cn_weight = sum(cn_weight, na.rm = TRUE),
    total_cn_units  = sum(cn_units,  na.rm = TRUE),
    # description/state are constant within a 6-digit code per your note.
    # Take the first non-missing value in each group:
    hs_description = dplyr::first(na.omit(hs_description)),
    state       = dplyr::first(na.omit(state)),
    .groups = "drop"
  )
  
# Calculate I and X in TJ values -------
  
    # merge NCV values for imp and exp specifically
    # unless they are not available, in which case use avg or ires

  ncv_exact <- ncv_by_fuel_hs_year %>%
    filter(ncv_type %in% c("I", "X"))
  
  ncv_fallback <- ncv_by_fuel_hs_year %>%
    filter(ncv_type %in% c("avg", "ires")) %>%
    mutate(priority = if_else(ncv_type == "avg", 1L, 2L)) %>%
    arrange(year, hs_code, priority) %>%
    group_by(year, hs_code) %>%
    slice(1L) %>%  # keep avg if present, otherwise ires
    ungroup() %>%
    select(
      year,
      hs_code,
      ncv_type_fallback = ncv_type,
      value_fallback    = value,
      unit_fallback     = unit
    )

  fuel_with_ncv <- fuel_trade %>%
    # exact match on year + HS + flow (I/X)
    left_join(
      ncv_exact,
      by = c("year", "cncode_6digit" = "hs_code", "flow" = "ncv_type")
    ) %>%
    # attach fallback candidates by year + HS
    left_join(
      ncv_fallback,
      by = c("year", "cncode_6digit" = "hs_code")
    ) %>%
    # choose exact NCV if available; otherwise fallback (avg → ires)
    mutate(
      ncv_value = if_else(!is.na(value), value, value_fallback),
      ncv_unit  = dplyr::coalesce(unit, unit_fallback),
      ncv_type_final = case_when(
        !is.na(value)              ~ flow,                # matched I/X
        is.na(value) & !is.na(value_fallback) ~ ncv_type_fallback,  # avg/ires
        TRUE                       ~ NA_character_
      )
    ) %>%
    # keep what you care about (adjust as needed)
    select(
      year,
      cncode_6digit,
      flow,
      ncv_type = ncv_type_final,
      ncv_value,
      ncv_unit,
      everything()
    )
  