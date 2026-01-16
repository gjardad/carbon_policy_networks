###############################################################################
# 02_build_proxies.R
#
# PURPOSE:
#   Enumerates all proxy definitions (grid over modifications) and builds each
#   proxy once. Results are cached to /output/loocv/cache as .rds files.
#
# OUTPUT:
#   Writes many: proxy_*.rds to /output/loocv/cache
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))
source(file.path(FUN_DIR, "fuel_proxy_builders.R"))

tic("02_build_proxies")

aux <- load_aux()

grid <- tidyr::expand_grid(
  use_siec_all = c(FALSE, TRUE),
  supplier_non_euets = c(FALSE, TRUE),
  buyer_sector_siec = c(FALSE, TRUE),
  emissions_weighted = c(FALSE, TRUE),
  supplier_nace_filter = c("none","high","high_medium")
)

make_proxy_name <- function(row) {
  paste0(
    "proxy",
    "_siecAll", as.integer(row$use_siec_all),
    "_nonEUETS", as.integer(row$supplier_non_euets),
    "_buyerSIEC", as.integer(row$buyer_sector_siec),
    "_wEm", as.integer(row$emissions_weighted),
    "_nace", row$supplier_nace_filter
  )
}

start_all <- Sys.time()

for (k in seq_len(nrow(grid))) {
  progress_eta(k, nrow(grid), start_all, every = 5, prefix = "  ")
  
  row <- grid[k, ]
  nm  <- make_proxy_name(row)
  out_path <- file.path(CACHE_DIR, paste0(nm, ".rds"))
  
  if (file.exists(out_path)) next
  
  log_step(paste0("Building ", nm))
  t0 <- Sys.time()
  
  mods <- as.list(row)
  proxy <- build_fuel_proxy(mods, aux) %>%
    mutate(fuel_proxy = as.numeric(fuel_proxy))
  
  saveRDS(list(name = nm, mods = mods, proxy = proxy), out_path)
  
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  log_step(paste0("Saved ", nm, " | build time ", round(elapsed, 1), "s"))
}

toc()
