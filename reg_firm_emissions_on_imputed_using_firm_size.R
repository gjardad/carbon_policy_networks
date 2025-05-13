#### HEADER -------

## Regression of firm emissions on imputed emissions for imputed emissions
# using firms' output share as weight on total sector-specific emissions

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
library(broom)

# Import data -----

load(paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))

# Clean data -----

df <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
  select(vat_ano, year, nace2d, turnover, obs_emissions, imputation_group, agg_emissions) %>% 
  group_by(year, nace2d) %>%
  mutate(turnover_share = case_when(
    imputation_group == 3 ~ 0,
    imputation_group %in% c(1,2,4) ~ turnover / sum(turnover[imputation_group %in% c(1,2,4)], na.rm = TRUE))) %>%
  ungroup() %>% 
  mutate(imputed_emissions = turnover_share * agg_emissions) %>% 
  select(obs_emissions, imputed_emissions)

# Regression ------

model <- lm(obs_emissions ~ imputed_emissions, data = df)

# Get R-squared
summary(model)$r.squared

# Graph -------

ggplot(df, aes(x = imputed_emissions, y = obs_emissions)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "",
    x = "Imputed Emissions",
    y = "Observed Emissions"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # remove grid lines
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
  
ggsave(paste0(output, "/obs_vs_imputed_emissions.png"))



