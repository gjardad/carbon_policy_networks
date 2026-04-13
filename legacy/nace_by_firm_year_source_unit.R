#### HEADER -------

## This code creates a data set with NACE codes for firms in Belgium

# For each firm, it combines info on NACE
  # 1. for all its establishments from CBE;
  # 2. for all its installations treated by the EUETS from EUTL;
  # 3. from Annual Accounts.

# It creates a data set in long format

# OBS: as of now, this hasn't been done because VAT codes in CBE cannot be matched with
# anonymized VAT codes in annual accounts, so I can't use info from CBE.

#####################

# ------------
# Set up ------
# ------------

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
library(stringr)
library(tidyr)

# -------------
# Load data ---
# -------------

# CBE
load(paste0(proc_data, "/nace5d_of_establishments_from_cbe.RData"))

# EUTL
load(paste0(proc_data,"/installation_year_emissions.RData"))

library(haven)
df_belgium_euets <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

# Annual accounts
load(paste0(proc_data,"/annual_accounts_selected_sample.RData"))

#---------------------------
# Helpers: cleaning NACE
#---------------------------

# Clean 5-digit style codes (CBE, Annual Accounts): "XXXXX" character, but some not 5-digit
clean_nace_5d <- function(x) {
  x <- str_trim(as.character(x))
  x <- na_if(x, "")
  # keep only digits (drops stray punctuation)
  digits <- str_replace_all(x, "[^0-9]", "")
  digits <- na_if(digits, "")
  # best-effort: if shorter than 5, left-pad? (I recommend NOT padding here; keep as-is)
  # We'll just return the digits-only string; you can filter to nchar==5 downstream.
  digits
}

# Clean dotted EUETS/EUTL codes like "XX.XX", sometimes "64.1", sometimes "35"
# Output a standardized dotted 4-digit format "XX.XX" when possible.
clean_nace_dotted_4d <- function(x) {
  x <- str_trim(as.character(x))
  x <- na_if(x, "")
  x <- str_replace_all(x, "\\s+", "")
  x <- str_replace_all(x, ",", ".")   # just in case commas appear
  
  # Cases:
  # 1) "35" -> "35.00"
  x <- if_else(str_detect(x, "^\\d{2}$"), str_c(x, ".00"), x)
  
  # 2) "64.1" -> "64.10"
  x <- if_else(str_detect(x, "^\\d{2}\\.\\d$"), str_c(x, "0"), x)
  
  # 3) "64.10" stays
  # 4) If "XX.XXX" (rare), we keep raw-ish but try to truncate to 2 decimals (best effort)
  x <- if_else(str_detect(x, "^\\d{2}\\.\\d{3,}$"),
               str_replace(x, "^(\\d{2}\\.\\d{2}).*$", "\\1"),
               x)
  
  # Validate final pattern; if it doesn't match "NN.NN", keep NA for std (but keep raw elsewhere)
  std <- if_else(str_detect(x, "^\\d{2}\\.\\d{2}$"), x, NA_character_)
  std
}

digits_only <- function(x) {
  x <- str_trim(as.character(x))
  x <- na_if(x, "")
  d <- str_replace_all(x, "[^0-9]", "")
  na_if(d, "")
}

guess_level <- function(digits) {
  # digits is digits-only string
  n <- nchar(digits)
  if_else(is.na(digits), NA_integer_, n)
}

#---------------------------
# 1) CBE: establishment-level NACE (no year in your described table)
#---------------------------

cbe_evidence <- nace5d_of_establishments_from_cbe %>%
  transmute(
    vat           = str_trim(vat),
    year          = NA_integer_,                 # no year in this source as described
    source        = "CBE",
    unit_type     = "establishment",
    unit_id       = as.character(establishmentnumber),
    nace_raw      = as.character(nacecode),
    nace_version  = as.character(naceversion),
    classification = as.character(classification),
    nace_std      = clean_nace_5d(nacecode),     # digits-only; not forcing to 5
    nace_digits   = clean_nace_5d(nacecode),
    nace_level_guess = guess_level(clean_nace_5d(nacecode))
  ) %>%
  filter(!is.na(vat))  # drop rows that can't be assigned to a firm

#---------------------------
# 2) EUTL/EUETS: installation-year NACE via bvd_id -> vat mapping
#---------------------------

# Map bvd_id -> vat
bvd_to_vat <- df_belgium_euets %>%
  transmute(
    bvd_id = as.character(bvd_id),
    vat    = str_trim(vat_ano)
  ) %>%
  filter(!is.na(bvd_id), !is.na(vat)) %>%
  distinct()

eutl_evidence <- installation_year_emissions %>%
  mutate(
    bvd_id = as.character(bvd_id),
    installation_id = as.character(installation_id),
    year = as.integer(year),
    nace_raw = as.character(nace_id)
  ) %>%
  filter(str_detect(installation_id, "^BE_")) %>%
  left_join(bvd_to_vat, by = "bvd_id") %>%
  transmute(
    vat           = vat,
    year          = year,
    source        = "EUTL",
    unit_type     = "installation",
    unit_id       = installation_id,
    nace_raw      = nace_raw,
    nace_version  = NA_character_,               # not provided here
    classification = NA_character_,              # not provided here
    nace_std      = clean_nace_dotted_4d(nace_raw),     # "NN.NN" if possible
    nace_digits   = digits_only(clean_nace_dotted_4d(nace_raw)),  # "NNNN" if possible
    nace_level_guess = guess_level(digits_only(clean_nace_dotted_4d(nace_raw)))
  )

# Note: Some rows with missing vat after join are "unmapped" bvd_id -> vat

#---------------------------
# 3) Annual Accounts: firm-year NACE (one per firm-year)
#---------------------------

annual_accounts_evidence <- df_annual_accounts_selected_sample %>%
  transmute(
    vat           = str_trim(vat_ano),
    year          = as.integer(year),
    source        = "AA",
    unit_type     = "firm",
    unit_id       = NA_character_,
    nace_raw      = as.character(nace5d),
    nace_version  = NA_character_,
    classification = NA_character_,
    nace_std      = clean_nace_5d(nace5d),       # digits-only
    nace_digits   = clean_nace_5d(nace5d),
    nace_level_guess = guess_level(clean_nace_5d(nace5d))
  ) %>%
  filter(!is.na(vat), !is.na(year))

#---------------------------
# 4) Combine into the canonical long table
#---------------------------

firm_nace_evidence <- bind_rows(
  cbe_evidence,
  eutl_evidence,
  aa_evidence
) %>%
  mutate(
    vat = str_trim(vat),
    # Useful QC flags
    is_std_missing = is.na(nace_std),
    is_digits_missing = is.na(nace_digits)
  ) %>%
  arrange(vat, year, source, unit_type, unit_id)

#---------------------------
# 5) Quick diagnostics (optional)
#---------------------------

# How many non-standardizable codes by source?
qc_nonstd <- firm_nace_evidence %>%
  group_by(source) %>%
  summarise(
    n = n(),
    n_std_missing = sum(is_std_missing, na.rm = TRUE),
    share_std_missing = mean(is_std_missing, na.rm = TRUE)
  )

# Look at weird-length codes (CBE/AA should be mostly 5 digits; EUTL mostly 4 digits)
qc_levels <- firm_nace_evidence %>%
  count(source, nace_level_guess, sort = TRUE)

# Print diagnostics
qc_nonstd
qc_levels
