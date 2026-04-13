#### HEADER -------

## This code creates measures of dispersion on

# 1. emissions productivity (inverse of emission intensity);
# 2. labor productivity
# 3. capital productivity

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

# Import data ------

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

# Fix NACE codes -----
  
  ## get rid of missing NACEs
  firm_year_belgian_euets <- firm_year_belgian_euets %>%
    group_by(bvd_id) %>%
    fill(nace5d, .direction = "downup") %>%
    ungroup() %>% 
    filter(!is.na(nace5d)) # those firms are missing on everything

  ## create less granular NACE codes
  firm_year_belgian_euets <- firm_year_belgian_euets %>% 
    mutate(nace2d = substr(nace5d, 1, 2),
           nace3d = substr(nace5d, 1, 3),
           nace4d = substr(nace5d, 1, 4))

  ## some firms have different nace codes across time
  inconsistent_nace <- firm_year_belgian_euets %>%
    group_by(firm_id) %>%
    summarise(
      nace5d_count = n_distinct(nace5d),
      nace4d_count = n_distinct(nace4d),
      nace3d_count = n_distinct(nace3d),
      nace2d_count = n_distinct(nace2d)
    )
  
# Create dispersion measures ------

dispersion_nace <- firm_year_belgian_euets %>%
  group_by(nace2d, year) %>%
  # create summary stats of the distribution
  summarize(
    avg_emissions_prod = mean(emissions_prod, na.rm = TRUE),
    p90_emissions_prod = quantile(emissions_prod, 0.9, na.rm = TRUE),
    p10_emissions_prod = quantile(emissions_prod, 0.1, na.rm = TRUE),
    avg_labor_prod = mean(labor_prod, na.rm = TRUE),
    p90_labor_prod = quantile(labor_prod, 0.9, na.rm = TRUE),
    p10_labor_prod = quantile(labor_prod, 0.1, na.rm = TRUE),
    avg_capital_prod = mean(capital_prod, na.rm = TRUE),
    p90_capital_prod = quantile(capital_prod, 0.9, na.rm = TRUE),
    p10_capital_prod = quantile(capital_prod, 0.1, na.rm = TRUE),
    avg_shortage_prod = mean(shortage_prod, na.rm = TRUE),
    p90_shortage_prod = quantile(shortage_prod, 0.9, na.rm = TRUE),
    p10_shortage_prod = quantile(shortage_prod, 0.1, na.rm= TRUE),
    num_obs = n()
  ) %>%
  ungroup() %>% 
  # create dispersion measures
  mutate(dispersion_emissions = p90_emissions_prod - p10_emissions_prod,
         dispersion_labor = p90_labor_prod - p10_labor_prod,
         dispersion_capital = p90_capital_prod - p10_capital_prod,
         dispersion_shortage = p90_shortage_prod - p10_shortage_prod)
  
  dispersion_activity <- firm_year_belgian_euets %>%
    group_by(activity_id, year) %>%
    # create summary stats of the distribution
    summarize(
      avg_emissions_prod = mean(emissions_prod, na.rm = TRUE),
      p90_emissions_prod = quantile(emissions_prod, 0.9, na.rm = TRUE),
      p10_emissions_prod = quantile(emissions_prod, 0.1, na.rm = TRUE),
      avg_labor_prod = mean(labor_prod, na.rm = TRUE),
      p90_labor_prod = quantile(labor_prod, 0.9, na.rm = TRUE),
      p10_labor_prod = quantile(labor_prod, 0.1, na.rm = TRUE),
      avg_capital_prod = mean(capital_prod, na.rm = TRUE),
      p90_capital_prod = quantile(capital_prod, 0.9, na.rm = TRUE),
      p10_capital_prod = quantile(capital_prod, 0.1, na.rm = TRUE),
      avg_shortage_prod = mean(shortage_prod, na.rm = TRUE),
      p90_shortage_prod = quantile(shortage_prod, 0.9, na.rm = TRUE),
      p10_shortage_prod = quantile(shortage_prod, 0.1, na.rm= TRUE),
      num_obs = n()
    ) %>%
    ungroup() %>% 
    # create dispersion measures
    mutate(dispersion_emissions = p90_emissions_prod - p10_emissions_prod,
           dispersion_labor = p90_labor_prod - p10_labor_prod,
           dispersion_capital = p90_capital_prod - p10_capital_prod,
           dispersion_shortage = p90_shortage_prod - p10_shortage_prod)

# Save it -------
save(dispersion_nace, file = paste0(proc_data,"/dispersion_nace_belgium.RData"))
save(dispersion_activity, file = paste0(proc_data,"/dispersion_activity_belgium.RData"))
  
  