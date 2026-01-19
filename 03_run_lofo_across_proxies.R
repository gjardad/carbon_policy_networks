###############################################################################
# 03_run_lofo_across_proxies.R
#
# PURPOSE:
#   For each cached proxy:
#     - Build EUETS estimation dataset (emissions + nace2d + nace5d + proxy)
#     - Run:
#         sector5d: year + nace5d FE + log(fuel_proxy) [LOFO]
#         sector2d: year + nace2d FE + log(fuel_proxy) [LOFO]
#         nosector: year FE only      + log(fuel_proxy) [LOFO]
#
# OUTPUT:
#   - /output/loocv/results/model_comparison_by_proxy.{rds,csv}
#   - per-proxy prediction files (optional)
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))

# Core runners
source(file.path(FUN_DIR, "run_lofo_fixest.R"))        # run_lofo_fixest_log_fuel()
source(file.path(FUN_DIR, "run_modelB_fixest.R"))      # run_lofo_modelB()

# New wrapper (renamed models)
source(file.path(
  FUN_DIR,
  "run_lofo_models_sector5d_sector2d_nosector_fixest.R"
))

tic("03_run_lofo_across_proxies")

library(dplyr)
library(purrr)
library(readr)

# ---- Load EUETS outcomes ----
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))

aa <- df_annual_accounts_selected_sample_key_variables

aa_id <- intersect(names(aa), c("vat_ano","vat"))
stopifnot(length(aa_id) > 0)
aa_id <- aa_id[[1]]

euets_id <- intersect(names(firm_year_belgian_euets), c("vat_ano","vat"))
stopifnot(length(euets_id) > 0)
euets_id <- euets_id[[1]]

# ---- Firm -> (nace5d,nace2d) mapping ----
aa_sector <- aa %>%
  transmute(
    firm_id = .data[[aa_id]],
    nace5d = as.character(nace5d),
    nace2d = substr(gsub("\\D", "", as.character(nace5d)), 1, 2)
  ) %>%
  distinct()

# ---- EUETS base sample (outcome + both sector codes) ----
euets_base <- firm_year_belgian_euets %>%
  transmute(
    firm_id = .data[[euets_id]],
    year = year,
    emissions = emissions
  ) %>%
  left_join(aa_sector, by = "firm_id") %>%
  filter(!is.na(nace2d), !is.na(nace5d))

# ---- Proxy files ----
proxy_files <- list.files(CACHE_DIR, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
stopifnot(length(proxy_files) > 0)

log_step(paste0("Found ", length(proxy_files), " proxy files in cache."))
start_all <- Sys.time()

MIN_FIRMS_PER_SECTOR <- 3
SAVE_PREDS <- TRUE

results <- purrr::map_dfr(seq_along(proxy_files), function(i) {
  progress_eta(i, length(proxy_files), start_all, every = 1, prefix = "PROXIES ")
  
  obj <- readRDS(proxy_files[[i]])
  nm <- obj$name
  mods <- obj$mods
  proxy <- obj$proxy
  
  log_step(paste0("=== Proxy: ", nm, " ==="))
  
  # Estimation dataset for THIS proxy (EUETS only)
  df <- euets_base %>%
    inner_join(proxy, by = c("firm_id" = "buyer_id", "year" = "year")) %>%
    transmute(
      firm_id, year, emissions,
      nace2d, nace5d,
      fuel_proxy = as.numeric(fuel_proxy)
    ) %>%
    filter(emissions > 0, !is.na(fuel_proxy), fuel_proxy > 0)
  
  # Save exact estimation df used (key for replication/debugging)
  saveRDS(df, file.path(RESULTS_DIR, paste0("estimation_df_", nm, ".rds")))
  
  # Run the three models
  out <- run_lofo_models_sector5d_sector2d_nosector(
    df = df,
    firm_col = "firm_id",
    year_col = "year",
    emissions_col = "emissions",
    proxy_col = "fuel_proxy",
    nace2d_col = "nace2d",
    nace5d_col = "nace5d",
    min_firms_per_sector = MIN_FIRMS_PER_SECTOR,
    verbose = FALSE
  )
  
  # Save predictions (optional)
  if (isTRUE(SAVE_PREDS)) {
    saveRDS(out$sector2d$df_lofo, file.path(RESULTS_DIR, paste0("preds_sector2d_", nm, ".rds")))
    saveRDS(out$sector5d$df_lofo, file.path(RESULTS_DIR, paste0("preds_sector5d_", nm, ".rds")))
    saveRDS(out$nosector$df_lofo, file.path(RESULTS_DIR, paste0("preds_nosector_", nm, ".rds")))
  }
  
  # One output row per proxy
  tibble(name = nm) %>%
    bind_cols(tibble(!!!mods)) %>%
    bind_cols(out$sector5d$perf %>% rename_with(~ paste0("sector5d_", .x))) %>%
    bind_cols(out$sector2d$perf %>% rename_with(~ paste0("sector2d_", .x))) %>%
    bind_cols(out$nosector$perf %>% rename_with(~ paste0("nosector_", .x)))
})

saveRDS(results, file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))
write_csv(results, file.path(RESULTS_DIR, "model_comparison_by_proxy.csv"))
log_step("Saved model_comparison_by_proxy.{rds,csv}")

log_step("Top 20 by sector5d nRMSE:")
print(results %>% arrange(sector5d_nRMSE) %>% slice(1:20))

log_step("Top 20 by sector2d nRMSE:")
print(results %>% arrange(sector2d_nRMSE) %>% slice(1:20))

log_step("Top 20 by nosector nRMSE:")
print(results %>% arrange(nosector_nRMSE) %>% slice(1:20))

toc()
