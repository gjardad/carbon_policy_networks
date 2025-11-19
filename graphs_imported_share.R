#### HEADER -------

## This code creates df with share of imports out of total
# supply by fuel-year in Belgium

# and generates three graphs:
# 1. scatter plot of imports vs total supply (domestic production + imports)
# 2. 

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

# Import and clean data --------

  load(paste0(proc_data,"/energy_balance_by_fuel_year.RData"))

  energy_balance_wide <- energy_balance %>%
    mutate(
      # rename flow categories
      flow = case_when(
        str_starts(flow, "EXP:")       ~ "X",
        str_starts(flow, "IMP:")       ~ "I",
        str_starts(flow, "GIC:")       ~ "GIC",
        str_starts(flow, "FC_IND_E:")  ~ "fc_ind_e",
        str_starts(flow, "TI_E:")      ~ "ti_e",
        str_starts(flow, "STK_CHG:")   ~ "stk_chg",
        str_starts(flow, "PPRD:")      ~ "P",
        TRUE                           ~ flow
      )
    ) %>%
    select(siec_code, year, flow, energy_balance_value) %>%
    
    # make it wide by siec_code × year
    pivot_wider(
      names_from  = flow,
      values_from = energy_balance_value,
      values_fn = sum,                 
      values_fill = 0 
    ) %>%
    
    # compute import dependence = (imports - exports) / GIC
    mutate(
      net_imports = I - X,
      net_imports_dependence = if_else(
        !is.na(GIC) & GIC != 0,
        net_imports / GIC,
        NA_real_
      ),
      # make production 0 if NA
      P = if_else(is.na(P), 0, P),
      supply = P + I,
      import_origin_share = if_else(
        supply > 0,
        I / supply,
        NA_real_
      )
    ) %>%
  filter(year >= 2005,
         # exclude SIEC codes that represent umbrella categories or
         # fuels that are not used to generate GHG in stationary industrial installations
         !siec_code %in% c("7000", "8000", "900H", "E", "0000X0350-0370",
                           "0350-0370", "1000", "4000XBIO", "5110-5150",
                           "5210B", "5210P", "5220B", "5220P", "5230P", "5230B",
                           "5290", "5300"))

# Generate graphs --------
  
  
  ### Scatterplot of I vs (P + I)
  # If points lie close to the 45° line, I are the predominant source
   p_import_share_of_supply <- energy_balance_wide %>%
     ggplot(aes(x = I, y = supply)) +
     geom_point(alpha = 0.6, color = "blue") +
     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
     labs(
       x = "Imports",
       y = "Domestic production + Imports"
     ) +
    theme_classic() +
    theme(
      axis.text  = element_blank(),
    )
  
  print(p_import_share_of_supply)
  
  ### Scatterplot of X vs I for each fuel-year
  p_reexport <- energy_balance_wide %>%
    ggplot(aes(x = I, y = X)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #facet_wrap(~ siec_code, scales = "free") +
    labs(
      x = "Imports",
      y = "Exports"
      #title = "For some fuels, imports ≈ exports → re-export hub"
    ) +
    theme_classic() +
    theme(
      axis.text  = element_blank(),
    )
  
  print(p_reexport) 
  
  ### Change in stocks over time
  energy_balance_wide <- energy_balance_wide %>%
    mutate(
      stk_change_share = abs(stk_chg) / supply
    )
  
  p_stocks <- energy_balance_wide %>%
    filter(!is.na(stk_change_share)) %>%
    ggplot(aes(x = year, y = stk_change_share)) +
    geom_line(color = "blue", alpha = 0.7) +
    geom_hline(yintercept = 0.1, linetype = "dashed") +
    facet_wrap(~ siec_code, scales = "free_y") +
    labs(
      x = "", 
      y = "|Stock change| / Supply",
      title = "Stock changes are very small for almost all fuels"
    ) +
    theme_classic()

  print(p_stocks)  
  
