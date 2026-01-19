###############################################################################
# run_lofo_models_sector5d_sector2d_nosector_fixest.R
#
# PURPOSE:
#   Runs LOFO evaluation for three specifications using fixest:
#     - sector5d: year FE + nace5d FE + log(fuel_proxy)
#     - sector2d: year FE + nace2d FE + log(fuel_proxy)
#     - nosector: year FE only         + log(fuel_proxy)
#
#   Designed to be called from 03_run_lofo_across_proxies.R
###############################################################################

run_lofo_models_sector5d_sector2d_nosector <- function(df,
                                                       firm_col,
                                                       year_col,
                                                       emissions_col,
                                                       proxy_col,
                                                       nace2d_col,
                                                       nace5d_col,
                                                       min_firms_per_sector = 3,
                                                       verbose = TRUE) {
  stopifnot(all(c(firm_col, year_col, emissions_col, proxy_col, nace2d_col, nace5d_col) %in% names(df)))
  
  # ---- sector2d ----
  if (verbose) message("[sector2d] LOFO: year + nace2d FE")
  out_sector2d <- run_lofo_fixest_log_fuel(
    df = df,
    firm_col = firm_col,
    year_col = year_col,
    sector_col = nace2d_col,
    emissions_col = emissions_col,
    proxy_col = proxy_col,
    min_firms_per_sector = min_firms_per_sector,
    verbose = verbose
  )
  
  # ---- sector5d ----
  if (verbose) message("[sector5d] LOFO: year + nace5d FE")
  out_sector5d <- run_lofo_fixest_log_fuel(
    df = df,
    firm_col = firm_col,
    year_col = year_col,
    sector_col = nace5d_col,
    emissions_col = emissions_col,
    proxy_col = proxy_col,
    min_firms_per_sector = min_firms_per_sector,
    verbose = verbose
  )
  
  # ---- nosector ----
  if (verbose) message("[nosector] LOFO: year FE only")
  out_nosector <- run_lofo_modelB(
    df = df,
    firm_col = firm_col,
    year_col = year_col,
    emissions_col = emissions_col,
    proxy_col = proxy_col,
    verbose = verbose
  )
  
  list(
    sector5d = out_sector5d,
    sector2d = out_sector2d,
    nosector = out_nosector
  )
}
