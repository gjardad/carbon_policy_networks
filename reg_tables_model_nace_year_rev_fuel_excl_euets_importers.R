#### HEADER -------

## This code generates regression tables for the basic fuel consumption proxy

# as part of the emission inference exercise

#####################

## Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ------

library(dplyr)
library(stargazer)

# Import and clean data --------

  load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))
  
  load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
  
  df <- amount_spent_on_fuel_by_firm_year %>%
    left_join(firm_year_belgian_euets %>% select(vat, year, revenue),
              by = c("vat_j_ano" = "vat", "year")
    ) %>%
    filter(emissions > 0,
           !is.na(amount_spent_on_fuel)) %>%
    mutate(
      year   = as.factor(year),
      nace5d = as.factor(nace5d)
    )
  
# Drop singleton firms -------
  
  #obs: there are some sectors with only one firm among the firms with emissions > 0
  # this means it's not possible to estimate the NACE fixed-effect in the LOFO regs
  # so we build training data excluding singleton firms
  
  sector_firm_counts <- df %>%
    group_by(nace5d) %>%
    summarise(n_firms = n_distinct(vat_j_ano), .groups = "drop")
  
  valid_sectors <- sector_firm_counts %>%
    filter(n_firms >= 2) %>%
    pull(nace5d)
  
  df_no_singletons <- df %>% filter(nace5d %in% valid_sectors)
  
  df_no_singletons <- df_no_singletons %>%
    mutate(year = factor(year),
           nace5d = factor(nace5d))
  
# Regression models ------------
  
  # FEs + fuel in levels
  model_level_fuel <- lm(
    emissions ~ year + nace5d + amount_spent_on_fuel,
    data = df_no_singletons
  )
  
  ## Fit log-log model
  
  # FEs + fuel
  model_log_fuel <- lm(
    log(emissions) ~ year + nace5d + log(amount_spent_on_fuel),
    data = df_no_singletons
  )
  
  # FEs + fuel excl EUETS importers
  model_log_fuel_excl_euets_importers <- lm(
    log(emissions) ~ year + nace5d + log(amount_spent_on_fuel_excl_euets_importers),
    data = df_no_singletons
  )
  
  # FEs + revenue + fuel
  model_log_rev_fuel <- lm(
    log(emissions) ~ year + nace5d + log(revenue) + log(amount_spent_on_fuel),
    data = df_no_singletons
  )

  # FEs + revenue + fuel excl EUETS importers
  model_log_rev_fuel_excl_euets_importers <- lm(
    log(emissions) ~ year + nace5d + log(revenue) + log(amount_spent_on_fuel_excl_euets_importers),
    data = df_no_singletons
  )

  
  