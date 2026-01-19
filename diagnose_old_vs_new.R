###############################################################################
# diagnose_old_vs_new.R
#
# PURPOSE:
#   EXACTLY reproduce:
#     (A) NEW pipeline result for one proxy/spec (from estimation_df_<proxy>.rds)
#     (B) OLD LOOCV-style result built from amount_spent_on_fuel_by_firm_year
#   and then decompose the difference mechanically into:
#     1) FE level effect (nace2d vs nace5d) holding proxy+support fixed
#     2) Proxy definition effect (NEW vs OLD) holding FE+support fixed
#     3) Support differences (which firm-years differ)
#
# WHAT YOU MUST SET:
#   - proxy_name_new: the pipeline proxy name you want (your chosen one below)
#   - old_proxy_var:  the column name in amount_spent_on_fuel_by_firm_year that
#                     matches the old spec (exclude EUETS importers)
#
# OUTPUT:
#   Prints and saves a bundle to /output/loocv/results/
###############################################################################

rm(list = ls())

source("00_config.R")
source(file.path(FUN_DIR, "run_lofo_fixest.R"))  # run_lofo_fixest_log_fuel()

library(dplyr)
library(readr)
library(stringr)

# ----------------------------
# USER SETTINGS
# ----------------------------
proxy_name_new <- "proxy_siecAll0_nonEUETS1_buyerSIEC0_wEm0_nacenone"
old_proxy_var  <- "amount_spent_on_fuel_excl_euets_importers"  # change if needed

MIN_FIRMS_PER_SECTOR <- 3  # must match pipeline

# ----------------------------
# Helpers
# ----------------------------
metrics_only <- function(out) out$perf

run_modelA <- function(df, sector_col) {
  run_lofo_fixest_log_fuel(
    df,
    firm_col = "firm_id",
    year_col = "year",
    sector_col = sector_col,
    emissions_col = "emissions",
    proxy_col = "fuel_proxy",
    min_firms_per_sector = MIN_FIRMS_PER_SECTOR,
    verbose = FALSE
  )
}

# ----------------------------
# (A) NEW: reproduce pipeline exactly
# ----------------------------
res <- readRDS(file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))
res_row <- res %>%
  filter(name == proxy_name_new) %>%
  select(name, A_nRMSE, A_MAPD, A_R2_LOO, A_Rho_Spearman, A_n_obs)

stopifnot(nrow(res_row) == 1)

df_new_path <- file.path(RESULTS_DIR, paste0("estimation_df_", proxy_name_new, ".rds"))
stopifnot(file.exists(df_new_path))
df_new <- readRDS(df_new_path)

# df_new is the authoritative pipeline estimation dataset
# It should already be filtered to emissions>0 and fuel_proxy>0
stopifnot(all(df_new$emissions > 0))
stopifnot(all(df_new$fuel_proxy > 0))

out_new <- run_modelA(
  df_new %>% transmute(firm_id, year, sector = sector, emissions, fuel_proxy),
  sector_col = "sector"
)

new_perf <- metrics_only(out_new) %>% mutate(which = "NEW_pipeline_recomputed")

# Must match res_row exactly (up to tolerance)
stopifnot(nrow(new_perf) == 1)
check_new <- all.equal(
  as.numeric(res_row %>% select(-name)),
  as.numeric(new_perf %>% select(nRMSE, MAPD, R2_LOO, Rho_Spearman, n_obs)),
  tolerance = 1e-10
)

cat("\n=== NEW pipeline row (from res) ===\n")
print(res_row)
cat("\n=== NEW pipeline recomputed (from estimation_df) ===\n")
print(new_perf)
cat("\nall.equal(res_row, recomputed) =\n")
print(check_new)

# ----------------------------
# (B) OLD: reproduce old LOOCV-style dataset deterministically
# ----------------------------
load(paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

# Keep EUETS emissions as truth to avoid .x/.y confusion
df_old_raw <- amount_spent_on_fuel_by_firm_year %>%
  left_join(
    firm_year_belgian_euets %>%
      transmute(vat, year, euets_emissions = emissions),
    by = c("vat_j_ano" = "vat", "year")
  )

stopifnot(old_proxy_var %in% names(df_old_raw))

df_old <- df_old_raw %>%
  transmute(
    firm_id  = vat_j_ano,
    year     = year,
    emissions = euets_emissions,
    nace5d   = as.character(nace5d),
    nace2d   = substr(gsub("\\D","", as.character(nace5d)), 1, 2),
    fuel_proxy = as.numeric(.data[[old_proxy_var]])
  ) %>%
  filter(
    emissions > 0,
    !is.na(fuel_proxy), fuel_proxy > 0,
    !is.na(nace2d), !is.na(nace5d)
  )

# OLD regression as you described historically:
# log(emissions) ~ year FE + nace5d FE + log(old_proxy)
out_old_nace5d <- run_modelA(
  df_old %>% transmute(firm_id, year, sector = nace5d, emissions, fuel_proxy),
  sector_col = "sector"
)
old_perf_nace5d <- metrics_only(out_old_nace5d) %>% mutate(which = "OLD_dataset_nace5dFE")

# Also compute OLD dataset with nace2d FE (to isolate FE effect later)
out_old_nace2d <- run_modelA(
  df_old %>% transmute(firm_id, year, sector = nace2d, emissions, fuel_proxy),
  sector_col = "sector"
)
old_perf_nace2d <- metrics_only(out_old_nace2d) %>% mutate(which = "OLD_dataset_nace2dFE")

cat("\n=== OLD (amount_spent_on_fuel_by_firm_year) results ===\n")
print(bind_rows(old_perf_nace5d, old_perf_nace2d))

# ----------------------------
# DECOMPOSITION (mechanical, no guessing)
# ----------------------------

# ---- Step 3 support differences: NEW vs OLD (at firm-year level) ----
keys_new <- df_new %>% distinct(firm_id, year)
keys_old <- df_old %>% distinct(firm_id, year)

only_old <- anti_join(keys_old, keys_new, by = c("firm_id","year"))
only_new <- anti_join(keys_new, keys_old, by = c("firm_id","year"))
overlap  <- inner_join(keys_new, keys_old, by = c("firm_id","year"))

support_summary <- tibble(
  n_new = nrow(keys_new),
  n_old = nrow(keys_old),
  n_overlap = nrow(overlap),
  n_only_new = nrow(only_new),
  n_only_old = nrow(only_old)
)

# ---- Step 1 FE effect holding PROXY+SUPPORT fixed ----
# Use NEW dataset + NEW proxy, but swap FE between nace2d and nace5d on SAME support.
# We need nace5d for firms in df_new (pipeline estimation df doesn’t carry it).
load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))
aa <- df_annual_accounts_selected_sample_key_variables
aa_id <- intersect(names(aa), c("vat_ano","vat")); stopifnot(length(aa_id)>0); aa_id <- aa_id[[1]]

firm_to_nace5d <- aa %>%
  transmute(firm_id = .data[[aa_id]], nace5d = as.character(nace5d)) %>%
  distinct()

df_new_with5d <- df_new %>%
  left_join(firm_to_nace5d, by = "firm_id") %>%
  filter(!is.na(nace5d))

out_new_nace2d <- run_modelA(
  df_new_with5d %>% transmute(firm_id, year, sector = sector, emissions, fuel_proxy),
  sector_col = "sector"
)
out_new_nace5d <- run_modelA(
  df_new_with5d %>% transmute(firm_id, year, sector = nace5d, emissions, fuel_proxy),
  sector_col = "sector"
)

step1_fe_table <- bind_rows(
  metrics_only(out_new_nace2d) %>% mutate(which = "NEW_support_NEWproxy_nace2dFE"),
  metrics_only(out_new_nace5d) %>% mutate(which = "NEW_support_NEWproxy_nace5dFE")
)

# ---- Step 2 proxy effect holding FE+SUPPORT fixed ----
# Use OVERLAP support (firm-years present in both datasets), FE fixed to nace2d.
df_overlap <- overlap %>%
  left_join(df_new %>% select(firm_id, year, emissions, sector, fuel_proxy_new = fuel_proxy),
            by = c("firm_id","year")) %>%
  left_join(df_old %>% select(firm_id, year, fuel_proxy_old = fuel_proxy, nace2d),
            by = c("firm_id","year"))

# Use pipeline nace2d FE (= sector) for consistency with the pipeline spec.
df_overlap <- df_overlap %>%
  filter(!is.na(fuel_proxy_new), fuel_proxy_new > 0,
         !is.na(fuel_proxy_old), fuel_proxy_old > 0,
         !is.na(sector))

out_overlap_newproxy <- run_modelA(
  df_overlap %>% transmute(firm_id, year, sector = sector, emissions, fuel_proxy = fuel_proxy_new),
  sector_col = "sector"
)
out_overlap_oldproxy <- run_modelA(
  df_overlap %>% transmute(firm_id, year, sector = sector, emissions, fuel_proxy = fuel_proxy_old),
  sector_col = "sector"
)

step2_proxy_table <- bind_rows(
  metrics_only(out_overlap_newproxy) %>% mutate(which = "OVERLAP_support_nace2dFE_NEWproxy"),
  metrics_only(out_overlap_oldproxy) %>% mutate(which = "OVERLAP_support_nace2dFE_OLDproxy")
)

# ----------------------------
# Summarize everything in one table
# ----------------------------
summary_table <- bind_rows(
  new_perf %>% mutate(which = "NEW_pipeline_baseline"),
  old_perf_nace5d,
  old_perf_nace2d,
  step1_fe_table,
  step2_proxy_table
) %>%
  select(which, everything())

cat("\n\n============================\n")
cat("SUMMARY TABLE (copy this)\n")
cat("============================\n")
print(summary_table)

cat("\n\n============================\n")
cat("SUPPORT SUMMARY (NEW vs OLD)\n")
cat("============================\n")
print(support_summary)

# Save a bundle for later inspection
bundle <- list(
  proxy_name_new = proxy_name_new,
  old_proxy_var = old_proxy_var,
  res_row = res_row,
  new_perf = new_perf,
  old_perf_nace5d = old_perf_nace5d,
  old_perf_nace2d = old_perf_nace2d,
  step1_fe_table = step1_fe_table,
  step2_proxy_table = step2_proxy_table,
  support_summary = support_summary,
  only_old = only_old,
  only_new = only_new,
  overlap_keys = overlap
)

out_file <- file.path(RESULTS_DIR, paste0("old_vs_new_decomposition_", proxy_name_new, ".rds"))
saveRDS(bundle, out_file)
cat("\nSaved decomposition bundle to:\n", out_file, "\n")
