#### HEADER -------

## This code creates a data set with establishment-level address for firms in Belgium

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

## ------------------------------------------------------------
## 1) Helpers
## ------------------------------------------------------------

# Keep leading zeros, remove spaces, dots, etc.
clean_id <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9A-Za-z]", "")  # remove punctuation/spaces
  x
}

# Safe lowercase
lc <- function(x) tolower(x)

## ------------------------------------------------------------
## 2) Load files
## ------------------------------------------------------------
enterprise    <- fread(file.path(raw_data, "/CBE/enterprise.csv"),    encoding = "UTF-8")
establishment <- fread(file.path(raw_data, "/CBE/establishment.csv"), encoding = "UTF-8")
address       <- fread(file.path(raw_data, "/CBE/address.csv"),       encoding = "UTF-8")
activity      <- fread(file.path(raw_data, "/CBE/activity.csv"),      encoding = "UTF-8")

# Standardize names to lower-case for robustness
setnames(enterprise,    names(enterprise),    lc(names(enterprise)))
setnames(establishment, names(establishment), lc(names(establishment)))
setnames(address,       names(address),       lc(names(address)))

## ------------------------------------------------------------
## 3) Clean key IDs
## ------------------------------------------------------------

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

# Address: EntityNumber can be enterprise OR establishment
address <- address %>%
  mutate(
    entitynumber = clean_id(entitynumber)
  )

## ------------------------------------------------------------
## 4) Join: establishment -> address (physical location)
##     Use: address$entitynumber == establishment$establishmentnumber
## ------------------------------------------------------------

address_of_establishments <- establishment %>%
  left_join(address %>% select("entitynumber", "zipcode", "countrynl", "countryfr", "municipalitynl", "municipalityfr",
                               "streetnl", "streetfr", "housenumber"),
            by = c("establishmentnumber" = "entitynumber")) %>% 
  left_join(enterprise %>% select(enterprisenumber, vat), by = "enterprisenumber")

# Save it -------
save(address_of_establishments, file = paste0(proc_data, "/address_of_establishments.RData"))

