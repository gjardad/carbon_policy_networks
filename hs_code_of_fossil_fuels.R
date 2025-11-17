#### HEADER -------

## This code identifies the HS codes for fossil fuel goods

# It is taken directly from HS chapter 27

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

# Create data set --------

# directly pulled from HS dictionaries
library(tibble)
hs_codes_for_fossil_fuels <- tribble(
  ~hs_code, ~hs_description, ~state,
  # --- Solid fossil fuels ---
  "270111", "Anthracite", "solid",
  "270112", "Bituminous coal", "solid",
  "270119", "Other coal", "solid",
  "270120", "Briquettes and similar solid fuels from coal", "solid",
  "270210", "Lignite, not agglomerated", "solid",
  "270220", "Agglomerated lignite", "solid",
  "270300", "Peat (including peat litter), whether or not agglomerated", "solid",
  "270400", "Coke and semi-coke of coal, lignite or peat; retort carbon", "solid",
  "271311", "Petroleum coke, not calcined", "solid",
  
  # --- Liquid fossil fuels ---
  "270900", "Crude petroleum oils and oils from bituminous minerals", "liquid",
  "271012", "Light oils and preparations (e.g. gasoline, naphtha)", "liquid",
  "271019", "Other petroleum oils (diesel, kerosene, heavy fuel oils, etc.)", "liquid",
  "271020", "Petroleum oils containing ≤70% biodiesel (mainly fossil-based)", "liquid",
  
  # --- Gaseous fossil fuels ---
  "271111", "Natural gas, liquefied", "gas",
  "271112", "Propane, liquefied", "gas",
  "271113", "Butanes, liquefied", "gas",
  "271114", "Other gaseous hydrocarbons, liquefied (e.g. propylene, butylene)", "gas",
  "271121", "Natural gas, gaseous", "gas",
  "271129", "Other petroleum gases, gaseous", "gas"
)

# Save it ------
save(hs_codes_for_fossil_fuels, file = paste0(proc_data,"/hs_codes_for_fossil_fuels.RData"))

