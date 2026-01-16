###############################################################################
# diagnose_sector_support_overlap.R
#
# PURPOSE:
#   Diagnose whether sector fixed effects estimated on EUETS firms
#   are usable when predicting emissions for non-EUETS firms.
#
#   For nace5d and nace2d, the script reports:
#     - how many non-EUETS firms are in-support
#     - how many are out-of-support (sector never seen in EUETS)
#
# OUTPUT:
#   Prints a small summary table to console.
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))

# ---- Load required datasets ----
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))

# ---- Identify firm ID variables ----
aa <- df_annual_accounts_selected_sample_key_variables
aa_id <- intersect(names(aa), c("vat_ano", "vat"))
stopifnot(length(aa_id) == 1)
aa_id <- aa_id[[1]]

euets_id <- intersect(names(firm_year_belgian_euets), c("vat_ano", "vat"))
stopifnot(length(euets_id) == 1)
euets_id <- euets_id[[1]]

# ---- Build firm-level sector table ----
firm_sector <- aa %>%
  transmute(
    firm_id = .data[[aa_id]],
    nace5d = as.character(nace5d),
    nace2d = substr(gsub("\\D", "", nace5d), 1, 2)
  ) %>%
  distinct()

# ---- Identify EUETS vs non-EUETS firms ----
euets_firms <- firm_year_belgian_euets %>%
  transmute(firm_id = .data[[euets_id]]) %>%
  distinct()

firm_sector <- firm_sector %>%
  mutate(
    is_euets = firm_id %in% euets_firms$firm_id
  )

# ---- Helper: sector support diagnostics ----
sector_support_report <- function(sector_var) {
  
  euets_sectors <- firm_sector %>%
    filter(is_euets) %>%
    pull(.data[[sector_var]]) %>%
    unique()
  
  non_euets <- firm_sector %>%
    filter(!is_euets)
  
  tibble(
    sector_level = sector_var,
    n_non_euets_firms = n_distinct(non_euets$firm_id),
    n_in_support = non_euets %>%
      filter(.data[[sector_var]] %in% euets_sectors) %>%
      summarise(n = n_distinct(firm_id)) %>%
      pull(n),
    n_out_of_support = non_euets %>%
      filter(!(.data[[sector_var]] %in% euets_sectors)) %>%
      summarise(n = n_distinct(firm_id)) %>%
      pull(n)
  ) %>%
    mutate(
      share_out_of_support = n_out_of_support / n_non_euets_firms
    )
}

# ---- Run diagnostics ----
report <- bind_rows(
  sector_support_report("nace2d"),
  sector_support_report("nace5d")
)

print(report)
