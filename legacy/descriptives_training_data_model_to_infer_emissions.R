#### HEADER -------

## This code generates descriptive statistics on the set of EUETS firms
# used to train the model to infer emissions

#####################

## Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ---------

library(dplyr)
library(knitr)
library(kableExtra)

# Import and clean data --------

  load(paste0(proc_data,"/df_used_to_train_model_to_infer_emissions.RData"))

  data_2018 <- df_training_data %>% 
    mutate(
      nace2d = substr(nace5d, 1, 2),
      fuel_consumption_over_revenue = amount_spent_on_fuel_from_non_euets_importers / revenue
    ) %>% 
    filter(year == 2018) %>%
    mutate(
      emissions = emissions / 1e3,
      fuel_share = 100 * amount_spent_on_fuel_from_non_euets_importers / revenue,
      revenue_mi = revenue / 1e6,
      nace2d_num = as.integer(nace2d),
      
      # NACE sector groups + "Others"
      nace_group = case_when(
        between(nace2d_num, 10, 12) ~ "Food, beverages and tobacco products",
        between(nace2d_num, 13, 15) ~ "Textiles, wearing apparel, and leather",
        nace2d_num == 16            ~ "Wood products",
        between(nace2d_num, 17, 18) ~ "Paper and printing",
        nace2d_num == 19            ~ "Coke and refined petroleum",
        between(nace2d_num, 20, 21) ~ "Chemicals and basic pharmaceuticals",
        nace2d_num == 22            ~ "Rubber and plastic products",
        nace2d_num == 23            ~ "Non-metallic mineral products",
        between(nace2d_num, 24, 25) ~ "Basic metals and fabricated metal products",
        nace2d_num == 35            ~ "Electricity, gas, and steam supply",
        TRUE                        ~ "Others"   # all remaining sectors
      )
    ) %>%
    mutate(
      nace_group = factor(
        nace_group,
        levels = c(
          "Food, beverages and tobacco products",
          "Textiles, wearing apparel, and leather",
          "Wood products",
          "Paper and printing",
          "Coke and refined petroleum",
          "Chemicals and basic pharmaceuticals",
          "Rubber and plastic products",
          "Non-metallic mineral products",
          "Basic metals and fabricated metal products",
          "Electricity, gas, and steam supply",
          "Others"
        )
      )
    )
  
# Helper function: stats for one group ------
  
  group_stats <- function(data, label) {
    data %>%
      summarise(
        row_label = label,
        N        = n_distinct(vat_j_ano),   # if you want obs instead: N = n()
        fc_p10   = quantile(fuel_share, 0.10, na.rm = TRUE),
        fc_p50   = quantile(fuel_share, 0.50, na.rm = TRUE),
        fc_p90   = quantile(fuel_share, 0.90, na.rm = TRUE),
        rev_p10  = quantile(revenue_mi, 0.10, na.rm = TRUE),
        rev_p50  = quantile(revenue_mi, 0.50, na.rm = TRUE),
        rev_p90  = quantile(revenue_mi, 0.90, na.rm = TRUE),
        emi_p10  = quantile(emissions, 0.10, na.rm = TRUE),
        emi_p50  = quantile(emissions, 0.50, na.rm = TRUE),
        emi_p90  = quantile(emissions, 0.90, na.rm = TRUE)
      )
  }
  
# Panel A: by importer status ------
  
  panelA <- bind_rows(
    group_stats(
      filter(data_2018, is_importer_in_euets == 1),
      "Fuel importers"
    ),
    group_stats(
      filter(data_2018, is_importer_in_euets == 0),
      "Not fuel importers"
    )
  )
  
# Panel B: by NACE 2-digit sector --------
  
  panelB_stats <- data_2018 %>%
    group_by(nace_group) %>%
    summarise(
      N       = n_distinct(vat_j_ano),
      fc_p10  = quantile(fuel_share, 0.10, na.rm = TRUE),
      fc_p50  = quantile(fuel_share, 0.50, na.rm = TRUE),
      fc_p90  = quantile(fuel_share, 0.90, na.rm = TRUE),
      rev_p10 = quantile(revenue_mi, 0.10, na.rm = TRUE),
      rev_p50 = quantile(revenue_mi, 0.50, na.rm = TRUE),
      rev_p90 = quantile(revenue_mi, 0.90, na.rm = TRUE),
      emi_p10 = quantile(emissions, 0.10, na.rm = TRUE),
      emi_p50 = quantile(emissions, 0.50, na.rm = TRUE),
      emi_p90 = quantile(emissions, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(nace_group) %>%
    mutate(row_label = nace_group) %>%
    select(row_label, N, fc_p10, fc_p50, fc_p90,
           rev_p10, rev_p50, rev_p90,
           emi_p10, emi_p50, emi_p90)
  
# Stack it all ------
  
  empty_row <- tibble(
    row_label = NA_character_,
    N        = NA_integer_,
    fc_p10   = NA_real_,
    fc_p50   = NA_real_,
    fc_p90   = NA_real_,
    rev_p10  = NA_real_,
    rev_p50  = NA_real_,
    rev_p90  = NA_real_,
    emi_p10  = NA_real_,
    emi_p50  = NA_real_,
    emi_p90  = NA_real_
  )
  
  table_desc <- bind_rows(
    # Panel A title row
    tibble(
      row_label = "Panel A: By fuel importer status",
      N        = NA_integer_,
      fc_p10   = NA_real_,
      fc_p50   = NA_real_,
      fc_p90   = NA_real_,
      rev_p10  = NA_real_,
      rev_p50  = NA_real_,
      rev_p90  = NA_real_,
      emi_p10  = NA_real_,
      emi_p50  = NA_real_,
      emi_p90  = NA_real_
    ),
    panelA,
    # Panel B title row
    tibble(
      row_label = "Panel B: By NACE 2-digit sectors",
      N        = NA_integer_,
      fc_p10   = NA_real_,
      fc_p50   = NA_real_,
      fc_p90   = NA_real_,
      rev_p10  = NA_real_,
      rev_p50  = NA_real_,
      rev_p90  = NA_real_,
      emi_p10  = NA_real_,
      emi_p50  = NA_real_,
      emi_p90  = NA_real_
    ),
    panelB_stats
  )
  
# Create LaTeX table --------------------------------------------------
  
  # Indent rows
  table_desc <- table_desc %>%
    mutate(
      row_label = case_when(
        row_label == "Panel A: By fuel importer status" ~ row_label,
        row_label == "Panel B: by NACE 2-digit sectors" ~ row_label,
        TRUE ~ paste0("\\hspace{3mm}", row_label)
      )
    )
  
  # Format NAs
  table_print <- table_desc %>%
    mutate(
      # # Firms column: integer, blank when NA
      N = ifelse(is.na(N), "", as.character(N)),
      # All other numeric columns: round, but blank when NA
      across(fc_p10:emi_p90,
             ~ ifelse(is.na(.), "", sprintf("%.2f", .)))
    )
  
  # Panel titles
  idx_panelA <- which(table_desc$row_label == "Panel A: By fuel importer status")
  idx_panelB <- which(table_desc$row_label == "Panel B: by NACE 2-digit sectors")
  
  
  # Obs: on the document I did some changes that I found easier to do manually
  latex_tab <- table_print %>%
    knitr::kable(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      align    = "lrrrrrrrrrr",
      col.names = c(
        "",
        "",
        "10th pct", "Median", "90th pct",
        "10th pct", "Median", "90th pct",
        "10th pct", "Median", "90th pct"
      )
    ) %>%
    kableExtra::add_header_above(
      c(
        " "                               = 1,
        "# Firms"                         = 1,
        "Fuel consumption (% of revenue)" = 3,
        "Revenue (mi euros)"              = 3,
        "Emissions (ton CO\\textsubscript{2})" = 3
      )
    ) %>%
    # IMPORTANT: no trailing \\ here
    kableExtra::row_spec(
      idx_panelA,
      extra_latex_after = "\\cmidrule(lr){1-1}"
    ) %>%
    kableExtra::row_spec(
      idx_panelB,
      extra_latex_after = "\\cmidrule(lr){1-1}"
    )
  
  cat(latex_tab)
  
  