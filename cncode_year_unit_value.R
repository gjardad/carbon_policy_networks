#### HEADER -------

# Creates CN-8digit-code-year unit value for fossil fuels imported into Belgium

#####################

# -------------
## Setup ------
# -------------

rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

library(tidyverse)
library(tidyr)
library(dplyr)

# --------------------
#  Load data ---------
# --------------------

load(paste0(proc_data,"/fuel_imported_by_firm_year.RData"))

# ------------------------------
# Create CN-year unit value ----
# ------------------------------

# clean it
df <- fuel_imported_by_firm_year %>%
  mutate(
    vat_ano = as.character(vat_ano),
    cncode  = str_replace_all(as.character(cncode), "\\s+", ""),
    year    = as.integer(year),
    imports_value  = as.numeric(imports_value),
    imports_weight = as.numeric(imports_weight),
    imports_value  = ifelse(imports_value  <= 0, NA_real_, imports_value),
    imports_weight = ifelse(imports_weight <= 0, NA_real_, imports_weight)
  ) %>%
  filter(!is.na(cncode), !is.na(year))

# calculate firm-level unit values
df_price_firm <- df %>%
  filter(!is.na(imports_value), !is.na(imports_weight)) %>%
  mutate(
    price_eur_per_kg = imports_value / imports_weight,
    log_price = log(price_eur_per_kg)
  ) %>%
  filter(is.finite(log_price))

# firm-level outliers within cncode-year
df_price_firm_flagged <- df_price_firm %>%
  group_by(cncode, year) %>%
  mutate(
    med = median(log_price, na.rm = TRUE),
    mad_ = mad(log_price, constant = 1, na.rm = TRUE),
    z_mad = ifelse(mad_ > 0, (log_price - med) / mad_, NA_real_),
    outlier = !is.na(z_mad) & abs(z_mad) > 4
  ) %>%
  ungroup() %>%
  select(vat_ano, cncode, year, z_mad, outlier)

# 4) CN-year diagnostics (use DISTINCT FIRMS, not rows)
diag_cn_year <- df_price_firm %>%
  group_by(cncode, year) %>%
  summarise(
    n_firms = n_distinct(vat_ano),
    p10 = quantile(price_eur_per_kg, 0.10, na.rm = TRUE),
    p90 = quantile(price_eur_per_kg, 0.90, na.rm = TRUE),
    median_price = median(price_eur_per_kg, na.rm = TRUE),
    max_price = max(price_eur_per_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ratio_p90_p10 = p90 / p10,
    ratio_max_med = max_price / median_price,
    quality_flag = case_when(
      n_firms < 3 ~ "too_few_obs",
      ratio_p90_p10 <= 10  ~ "very_good",
      ratio_p90_p10 <= 30  ~ "ok",
      ratio_p90_p10 <= 100 ~ "noisy",
      TRUE ~ "bad"
    )
  )

# 5) CN-year price for usable cells (median firm price)
usable <- c("very_good", "ok")

cn_year_price_final <- df_price_firm %>%
  left_join(diag_cn_year %>% select(cncode, year, quality_flag),
            by = c("cncode", "year")) %>%
  filter(quality_flag %in% usable) %>%
  group_by(cncode, year) %>%
  summarise(
    price_eur_per_kg = median(price_eur_per_kg, na.rm = TRUE),
    n_firms = n_distinct(vat_ano),
    quality_flag = first(quality_flag),
    .groups = "drop"
  )

# 6) Build final quantity variable with two-layer rule
df_final <- df %>%
  left_join(diag_cn_year %>% select(cncode, year, quality_flag),
            by = c("cncode", "year")) %>%
  left_join(df_price_firm_flagged,
            by = c("vat_ano", "cncode", "year")) %>%
  left_join(cn_year_price_final %>% select(cncode, year, price_eur_per_kg),
            by = c("cncode", "year")) %>%
  mutate(
    quality_flag = ifelse(is.na(quality_flag), "no_unit_value", quality_flag),
    outlier = ifelse(is.na(outlier), FALSE, outlier),
    
    use_weight = quality_flag %in% c("very_good", "ok") &
      !outlier &
      !is.na(imports_weight),
    
    fuel_qty_kg = case_when(
      use_weight ~ imports_weight,
      !is.na(price_eur_per_kg) & !is.na(imports_value) ~ imports_value / price_eur_per_kg,
      TRUE ~ NA_real_
    ),
    
    qty_source = case_when(
      use_weight ~ "weight_direct",
      !is.na(price_eur_per_kg) ~ "value_deflated",
      TRUE ~ "missing"
    )
  )

missing_cn <- df_final %>%
  filter(is.na(fuel_qty_kg) & !is.na(imports_value)) %>%
  distinct(cncode) %>% 
  mutate(cn4 = substr(cncode, 1, 4))

unique(missing_cn$cn4)

