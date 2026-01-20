###############################################################################
# 03_run_lofo_across_proxies.R
#
# PURPOSE:
#   For each cached proxy:
#     - Build EUETS estimation dataset (emissions + nace2d + nace5d + revenue + proxy)
#     - Evaluate multiple LOFO models:
#         * FE structure: additive (year + sector) vs interactive (year^sector)
#         * RHS: fuel only, revenue only (benchmark), fuel+revenue
#         * Sector granularity: sector5d, sector2d, nosector
#
# OUTPUT:
#   - /output/loocv/results/model_comparison_by_proxy.{rds,csv}
#   - /output/loocv/results/estimation_df_<proxy>.rds
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))
source(file.path(FUN_DIR, "run_lofo_fixest_generic.R"))

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

# ---- firm-year revenue + nace mapping ----
aa_firm_year <- aa %>%
  transmute(
    firm_id = .data[[aa_id]],
    year = year,
    nace5d = as.character(nace5d),
    nace2d = substr(gsub("\\D", "", as.character(nace5d)), 1, 2),
    revenue = as.numeric(turnover_VAT)
  ) %>%
  distinct()

# ---- EUETS base ----
euets_base <- firm_year_belgian_euets %>%
  transmute(
    firm_id = .data[[euets_id]],
    year = year,
    emissions = emissions
  ) %>%
  left_join(aa_firm_year, by = c("firm_id","year")) %>%
  filter(!is.na(nace2d), !is.na(nace5d))

# ---- Proxy files ----
proxy_files <- list.files(CACHE_DIR, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
stopifnot(length(proxy_files) > 0)

log_step(paste0("Found ", length(proxy_files), " proxy files in cache."))
start_all <- Sys.time()

MIN_FIRMS_PER_SECTOR <- 3
SAVE_PREDS <- FALSE  # set TRUE if you want per-proxy predictions for every model (can be huge)

# ---- Model specification grid ----
model_grid <- tidyr::expand_grid(
  sector_level = c("sector5d","sector2d","nosector"),
  fe_structure = c("additive","interactive"),
  rhs = c("fuel", "revenue", "fuel_revenue")
) %>%
  # interactive FE makes no difference without a sector
  filter(!(sector_level == "nosector" & fe_structure == "interactive")) %>%
  mutate(
    model_name = paste0(sector_level, "__", fe_structure, "__", rhs)
  )

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
      revenue,
      fuel_proxy = as.numeric(fuel_proxy)
    ) %>%
    filter(emissions > 0, !is.na(fuel_proxy), fuel_proxy > 0)
  
  saveRDS(df, file.path(RESULTS_DIR, paste0("estimation_df_", nm, ".rds")))
  
  # Run all model variants and store metrics in a wide row
  out_row <- tibble(name = nm) %>%
    bind_cols(tibble(!!!mods))
  
  for (k in seq_len(nrow(model_grid))) {
    spec <- model_grid[k, ]
    
    sector_col <- switch(spec$sector_level,
                         sector5d = "nace5d",
                         sector2d = "nace2d",
                         nosector = NULL)
    
    fuel_col <- if (spec$rhs %in% c("fuel","fuel_revenue")) "fuel_proxy" else NULL
    rev_col  <- if (spec$rhs %in% c("revenue","fuel_revenue")) "revenue" else NULL
    
    # NOTE: revenue-only model is your "benchmark"
    out <- run_lofo_fixest_generic(
      df = df,
      firm_col = "firm_id",
      year_col = "year",
      emissions_col = "emissions",
      fuel_col = fuel_col,
      revenue_col = rev_col,
      sector_col = sector_col,
      fe_structure = spec$fe_structure,
      min_firms_per_sector = MIN_FIRMS_PER_SECTOR,
      verbose = FALSE
    )
    
    # Optional: save predictions (big!)
    if (isTRUE(SAVE_PREDS)) {
      saveRDS(out$df_lofo,
              file.path(RESULTS_DIR, paste0("preds_", spec$model_name, "_", nm, ".rds")))
    }
    
    # Bind metrics with prefix = model_name
    perf <- out$perf %>% rename_with(~ paste0(spec$model_name, "__", .x))
    out_row <- bind_cols(out_row, perf)
  }
  
  out_row
})

saveRDS(results, file.path(RESULTS_DIR, "model_comparison_by_proxy.rds"))
write_csv(results, file.path(RESULTS_DIR, "model_comparison_by_proxy.csv"))
log_step("Saved model_comparison_by_proxy.{rds,csv}")

toc()
