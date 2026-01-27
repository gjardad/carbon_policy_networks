#### HEADER -------

## This code creates data set with firm-year fuel consumption
# input cost share

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

library(dplyr)

# =========================
# Load data ---------------
# =========================

load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

load(paste0(proc_data, "/firm_year_domestic_input_cost.RData"))

load(paste0(proc_data, "/firm_year_total_imports.RData"))

load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))

# =========================
# Build data -------------
# =========================

fuel <- amount_spent_on_fuel_by_firm_year %>%
  rename(
    vat = vat_j_ano,
    fuel_spend = amount_spent_on_fuel_excl_euets_importers
  )

euets_flag <- firm_year_belgian_euets %>%
  mutate(euets = 1) %>% 
  select(vat, year, euets)

domestic_costs <- firm_year_domestic_input_cost %>%
  select(vat, year, input_cost)

imports <- firm_year_total_imports %>%
  rename(vat = vat_ano) %>% 
  select(vat, year, total_imports)

fuel_input_cost_share <- fuel %>%
  left_join(domestic_costs, by = c("vat", "year")) %>%
  left_join(imports,       by = c("vat", "year")) %>%
  left_join(euets_flag,    by = c("vat", "year")) %>%
  mutate(
    # non-matched firms are non-EUETS
    euets = if_else(is.na(euets), 0, euets),
    # total costs = domestic input costs + imports
    total_costs = input_cost + coalesce(total_imports, 0),
    # firm-year fuel share of costs (set to NA if total_costs is 0 or missing)
    fuel_share_costs = if_else(
      !is.na(total_costs) & total_costs > 0,
      fuel_spend / total_costs,
      NA_real_
    ),
    # indicator for positive fuel consumption
    fuel_positive = fuel_spend > 0
  ) %>% 
  select(vat, year, euets, amount_spent_on_fuel, fuel_share_costs,
         total_costs, input_cost, total_imports, emissions)

  # merge with nace5d info from annual accounts
fuel_input_cost_share <- fuel_input_cost_share %>% 
  left_join(df_annual_accounts_selected_sample_key_variables %>%
              select(vat, year, nace5d), by = c("vat", "year"))

# save it
save(fuel_input_cost_share, file = paste0(proc_data, "/fuel_input_cost_share.RData"))