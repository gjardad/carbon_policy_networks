#### HEADER -------

## This code creates a data set with establishment-level NACE activity codes for firms in Belgium

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

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

## Helpers -------------------------

# Keep leading zeros, remove spaces, dots, etc.
clean_id <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9A-Za-z]", "")  # remove punctuation/spaces
  x
}

# Safe lowercase
lc <- function(x) tolower(x)

## Load files ------------------

enterprise    <- fread(file.path(raw_data, "/CBE/enterprise.csv"),    encoding = "UTF-8")
establishment <- fread(file.path(raw_data, "/CBE/establishment.csv"), encoding = "UTF-8")
address       <- fread(file.path(raw_data, "/CBE/address.csv"),       encoding = "UTF-8")
activity      <- fread(file.path(raw_data, "/CBE/activity.csv"),      encoding = "UTF-8")

# Standardize names to lower-case for robustness
setnames(enterprise,    names(enterprise),    lc(names(enterprise)))
setnames(establishment, names(establishment), lc(names(establishment)))
setnames(activity,      names(activity),      lc(names(activity)))

## Clean key IDs ----------------------

# Enterprise
enterprise <- enterprise %>%
  mutate(
    enterprisenumber = clean_id(enterprisenumber),
    # optional VAT-like field
    vat = paste0("BE", enterprisenumber)
  )

# Establishment
establishment <- establishment %>%
  mutate(
    establishmentnumber = clean_id(establishmentnumber),
    enterprisenumber    = clean_id(enterprisenumber)
  )

# Activity: entitynumber can be enterprise OR establishment (depends on record)
activity <- activity %>%
  mutate(
    entitynumber = clean_id(entitynumber),
    nacecode     = as.character(nacecode)
  )

## Attach main activity (NACE) at establishment level --------------

activity_keep <- activity %>%
  mutate(
    entitynumber   = as.character(entitynumber),
    naceversion    = as.character(naceversion),
    classification = as.character(classification),
    nacecode       = as.character(nacecode)
  ) %>%
  filter(naceversion %in% c("2008", "2025")) %>%
  filter(classification %in% c("MAIN", "SECO")) %>%
  filter(!is.na(nacecode), nacecode != "")

## 1) For each establishment, decide which version to keep
chosen_version <- activity_keep %>%
  group_by(entitynumber) %>%
  summarise(
    has_main_2008 = any(classification == "MAIN" & naceversion == "2008"),
    .groups = "drop"
  ) %>%
  mutate(version = ifelse(has_main_2008, "2008", "2025")) %>%
  select(entitynumber, version)

## 2) Keep only rows from the chosen version
activity_chosen <- activity_keep %>%
  left_join(chosen_version, by = "entitynumber") %>%
  filter(naceversion == version)

## 3) Create wide MAIN and SECO columns
to_wide <- function(df, class_value, prefix) {
  df %>%
    filter(classification == class_value) %>%
    arrange(entitynumber, nacecode) %>%
    group_by(entitynumber) %>%
    mutate(k = row_number()) %>%
    ungroup() %>%
    transmute(entitynumber,
              name  = paste0(prefix, "_", k),
              value = nacecode) %>%
    pivot_wider(names_from = name, values_from = value)
}

main_wide <- to_wide(activity_chosen, "MAIN", "main_nace")
seco_wide <- to_wide(activity_chosen, "SECO", "seco_nace")

## 4) Combine + attach version
activity_of_establishments <- chosen_version %>%
  left_join(main_wide, by = "entitynumber") %>%
  left_join(seco_wide, by = "entitynumber") %>%
  rename(establishmentnumber = entitynumber)

## Join VAT id ---------

activity_of_establishments <- establishment %>% 
  left_join(activity_chosen %>% select(-c("activitygroup", "version")),
            by = c("establishmentnumber" = "entitynumber")) %>% 
  left_join(enterprise %>% select(enterprisenumber, vat), by = "enterprisenumber")

# Save it -------
save(activity_of_establishments, file = paste0(proc_data, "/nace5d_of_establishments.RData"))
