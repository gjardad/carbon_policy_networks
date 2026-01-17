###############################################################################
# 03_run_lofo_across_proxies.R
#
# PURPOSE:
#   For each cached proxy in /output/loocv/cache:
#     - Build EUETS training dataset (firm-year emissions + nace2d + proxy)
#     - Run Model A: year FE + nace2d FE + log(fuel_proxy)  [LOFO]
#     - Run Model B: year FE only + log(fuel_proxy)         [LOFO]
#     - Run Model B: year FE only + log(fuel_proxy)         [LOSO]  (optional)
#
# OUTPUT:
#   - /output/loocv/results/model_comparison_by_proxy.{rds,csv}
#   - (optional) per-proxy prediction files
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))
source(file.path(FUN_DIR, "run_lofo_fixest.R"))        # Model A
source(file.path(FUN_DIR, "run_modelB_fixest.R"))      # Model B

tic("03_run_lofo_across_proxies")

# ---- Load EUETS outcomes ----
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))

# ---- Identify IDs ----
aa <- df_annual_accounts_selected_sample_key_variables
aa_id <- intersect(names(aa), c("vat_ano","vat"))
stopifnot(length(aa_id) > 0)
aa_id <- aa_id[[1]]

euets_id <- intersect(names(firm_year_belgian_euets), c("vat_ano","vat"))
stopifnot(length(euets_id) > 0)
euets_id <- euets_id[[1]]

# ---- Build firm -> nace2d mapping ----
aa_sector <- aa %>%
  transmute(
    firm_id = .data[[aa_id]],
    nace5d = as.character(nace5d),
    sector = substr(gsub("\\D", "", nace5d), 1, 2)   # nace2d
  ) %>%
  distinct()

# ---- Build EUETS base sample (outcome + sector) ----
euets_base <- firm_year_belgian_euets %>%
  transmute(
    firm_id = .data[[euets_id]],
    year = year,
    emissions = emissions
  ) %>%
  left_join(aa_sector %>% select(firm_id, sector), by = "firm_id") %>%
  filter(!is.na(sector))

# ---- Proxy files ----
proxy_files <- list.files(CACHE_DIR, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
stopifnot(length(proxy_files) > 0)

log_step(paste0("Found ", length(proxy_files), " proxy files in cache."))
start_all <- Sys.time()

# Toggle this off if you don't want LOSO (it will add runtime)
RUN_LOSO <- TRUE

results <- purrr::map_dfr(seq_along(proxy_files), function(i) {
  progress_eta(i, length(proxy_files), start_all, every = 1, prefix = "PROXIES ")
  
  obj <- readRDS(proxy_files[[i]])
  nm <- obj$name
  mods <- obj$mods
  proxy <- obj$proxy
  
  log_step(paste0("=== Proxy: ", nm, " ==="))
  
  df <- euets_base %>%
    left_join(proxy, by = c("firm_id" = "buyer_id", "year" = "year")) %>%
    mutate(fuel_proxy = as.numeric(fuel_proxy)) %>%
    filter(emissions > 0, !is.na(fuel_proxy), fuel_proxy > 0)
  
  saveRDS(df, file.path(RESULTS_DIR, paste0("estimation_df_", nm, ".rds")))
  
  # ---- Model A: year + sector(nace2d) FE ----
  log_step("Running Model A (year + nace2d FE) LOFO")
  tA <- Sys.time()
  outA <- run_lofo_fixest_log_fuel(
    df,
    firm_col = "firm_id",
    year_col = "year",
    sector_col = "sector",
    emissions_col = "emissions",
    proxy_col = "fuel_proxy",
    min_firms_per_sector = 3,
    verbose = TRUE
  )
  log_step(paste0("Model A done | ", round(as.numeric(difftime(Sys.time(), tA, "secs"))/60, 2), " min"))
  
  # ---- Model B: year FE only ----
  log_step("Running Model B (year FE only) LOFO")
  tB <- Sys.time()
  outB <- run_lofo_modelB(
    df,
    firm_col = "firm_id",
    year_col = "year",
    emissions_col = "emissions",
    proxy_col = "fuel_proxy",
    verbose = TRUE
  )
  log_step(paste0("Model B LOFO done | ", round(as.numeric(difftime(Sys.time(), tB, "secs"))/60, 2), " min"))
  
  # ---- Model B: LOSO (sector holdout stress test) ----
  if (isTRUE(RUN_LOSO)) {
    log_step("Running Model B LOSO (leave-one-nace2d-out)")
    tS <- Sys.time()
    outS <- run_loso_modelB(
      df,
      sector_col = "sector",
      year_col = "year",
      emissions_col = "emissions",
      proxy_col = "fuel_proxy",
      min_firms_per_sector = 3,
      verbose = TRUE
    )
    log_step(paste0("Model B LOSO done | ", round(as.numeric(difftime(Sys.time(), tS, "secs"))/60, 2), " min"))
  } else {
    outS <- NULL
  }
  
  # Optional: save predictions (can be large)
  saveRDS(outA$df_lofo, file.path(RESULTS_DIR, paste0("preds_modelA_", nm, ".rds")))
  saveRDS(outB$df_lofo, file.path(RESULTS_DIR, paste0("preds_modelB_lofo_", nm, ".rds")))
  if (RUN_LOSO) saveRDS(outS$df_loso, file.path(RESULTS_DIR, paste0("preds_modelB_loso_", nm, ".rds")))
  
  # Combine performance into one row (wide)
  row <- tibble(name = nm) %>%
    bind_cols(tibble(!!!mods)) %>%
    bind_cols(
      outA$perf %>% rename_with(~ paste0("A_", .x)),
      outB$perf %>% rename_with(~ paste0("B_LOFO_", .x))
    )
  
  if (RUN_LOSO) {
    row <- row %>% bind_cols(outS$perf %>% rename_with(~ paste0("B_LOSO_", .x)))
  }
  
  row
})

saveRDS(results, file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))
write_csv(results, file.path(RESULTS_DIR, "model_comparison_by_proxy.csv"))

log_step("Saved model_comparison_by_proxy.{rds,csv}")

log_step("Top 20 by Model A nRMSE:")
print(results %>% arrange(A_nRMSE) %>% slice(1:20))

log_step("Top 20 by Model B LOFO nRMSE:")
print(results %>% arrange(B_LOFO_nRMSE) %>% slice(1:20))

if (RUN_LOSO) {
  log_step("Top 20 by Model B LOSO nRMSE (sector extrapolation stress):")
  print(results %>% arrange(B_LOSO_nRMSE) %>% slice(1:20))
}

toc()
