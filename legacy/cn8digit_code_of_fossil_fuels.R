#### HEADER -------

## This code identifies the HS codes for fossil fuel goods

# It is taken from HS chapter 27, with additional input from ChatGPT to
# only include fuels that are used to generate emissions in stationary industrial installations
# that is, exclude fuels used as feedstock or to generate emissions in non-stationary installations such as vehicles

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

# set of 8-digit codes listed by ChatGPT
library(tibble)
cn8digit_codes_for_fossil_fuels <- tribble(
  ~cn_code,   ~cn_description,                                ~state,
  
  # --------------------------
  # SOLID FUELS
  # --------------------------
  "27011100", "Anthracite",                                    "solid",
  "27011210", "Coking coal",                                   "solid",
  "27011290", "Other bituminous coal",                         "solid",
  "27011900", "Other coal",                                    "solid",
  "27012000", "Briquettes, ovoids and solid fuels manufactured from coal", "solid",
  "27021000", "Lignite, not agglomerated",                     "solid",
  "27022000", "Agglomerated lignite (brown-coal briquettes)",  "solid",
  "27030000", "Peat",                                          "solid",
  "27040010", "Coke and semi-coke of coal",                    "solid",
  "27040090", "Coke and semi-coke (other)",                    "solid",
  "27060000", "Coal tar",                                      "solid",
  "27071000", "Oils and other products of the distillation of high temperature coal tar", "solid",
  "27072000", "Cresols and xylenols",                          "solid",
  "27073000", "Carbonic acids",                                "solid",
  "27074000", "Naphthalene",                                   "solid",
  "27075000", "Other aromatic hydrocarbons from coal tar",     "solid",
  "27076000", "Phenols",                                       "solid",
  "27079100", "Other pitch and pitch coke",                    "solid",
  "27079900", "Other coal-tar derived products",               "solid",
  "27131100", "Petroleum coke, not calcined",                  "solid",
  "27131200", "Petroleum coke, calcined",                      "solid",
  
  # --------------------------
  # LIQUID FUELS
  # --------------------------
  # Gas oils / diesel oils
  "27101943", "Gas oils (diesel-type) for heating or industrial use", "liquid",
  "27101946", "Gas oils (other)",                               "liquid",
  "27101947", "Gas oils (other)",                               "liquid",
  "27101948", "Gas oils (other)",                               "liquid",
  "27101949", "Gas oils (other)",                               "liquid",
  
  # Heavy gas oils
  "27101951", "Heavy gas oils",                                 "liquid",
  "27101955", "Heavy gas oils",                                 "liquid",
  "27101959", "Heavy gas oils",                                 "liquid",
  
  # Fuel oils (residual fuel)
  "27101961", "Fuel oils",                                      "liquid",
  "27101968", "Fuel oils",                                      "liquid",
  "27101969", "Fuel oils",                                      "liquid",
  "27101971", "Fuel oils",                                      "liquid",
  "27101975", "Fuel oils",                                      "liquid",
  "27101979", "Fuel oils",                                      "liquid",
  
  # HS 2022 biodiesel–fossil blends (still used in stationary boilers)
  "27102011", "Petroleum oils containing ≤70% biodiesel",       "liquid",
  "27102019", "Petroleum oils containing ≤70% biodiesel",       "liquid",
  "27102090", "Petroleum oils containing ≤70% biodiesel",       "liquid",
  
  # --------------------------
  # GAS FUELS
  # --------------------------
  "27111100", "Liquefied natural gas (LNG)",                    "gas",
  "27112100", "Natural gas in gaseous state",                   "gas",
  "27111211", "Propane, liquefied",                             "gas",
  "27111219", "Propane, liquefied",                             "gas",
  "27111311", "Butane, liquefied",                              "gas",
  "27111319", "Butane, liquefied",                              "gas",
  "27112900", "Other petroleum gases (refinery gas / cracker gas)", "gas"
)

# Old list of fuels based on my own classification from reading HS chapter 27 ------


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
#save(hs_codes_for_fossil_fuels, file = paste0(proc_data,"/hs_codes_for_fossil_fuels.RData"))
save(cn8digit_codes_for_fossil_fuels, file = paste0(proc_data,"/cn8digit_codes_for_fossil_fuels.RData"))

