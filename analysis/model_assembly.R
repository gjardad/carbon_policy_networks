###############################################################################
# model_assembly.R  —  assemble the model-input bundle from raw NBB data.
#
# Sourced by run_counterfactuals_rmd.R and profile_solver.R (pure functions, no
# top-level execution). Caller must have proc_data / out_data in scope (paths.R).
###############################################################################
suppressMessages({ library(dplyr); library(stringr); library(Matrix) })

# Assemble the bundle {Omega, e_bar, gamma, tau, x, ...} for one base year.
#   scope: "ets_neighbors" (ETS firms + their direct B2B partners) or "full".
assemble_bundle <- function(yr, scope, proc, out) {
  load(file.path(proc, "b2b_selected_sample.RData"))
  load(file.path(proc, "firm_year_belgian_euets.RData"))
  load(file.path(proc, "annual_accounts_selected_sample_key_variables.RData"))
  load(file.path(proc, "deployment_panel.RData"))
  load(file.path(proc, "deflator_nace4d_2005base.RData"))
  load(file.path(proc, "allocation_glo_balanced", sprintf("alloc_%d.RData", yr)))  # year_firms (vat,scope1)

  message("  [assemble] data loaded; filtering B2B for year ", yr, " ...")
  aa <- df_annual_accounts_selected_sample_key_variables
  colnames(df_b2b_selected_sample)[1:4] <- c("vat_supplier", "vat_buyer", "year", "corr_sales")
  b2b <- df_b2b_selected_sample %>% filter(year == yr, corr_sales > 0)
  message(sprintf("  [assemble] B2B edges this year: %d", nrow(b2b)))

  ets_vat <- firm_year_belgian_euets %>%
    filter(year == yr, in_sample == 1, !is.na(vat)) %>% distinct(vat) %>% pull(vat)

  if (scope == "ets_neighbors") {
    touch <- b2b %>% filter(vat_supplier %in% ets_vat | vat_buyer %in% ets_vat)
    subset_vat <- unique(c(ets_vat, touch$vat_supplier, touch$vat_buyer))
  } else {
    subset_vat <- unique(c(b2b$vat_supplier, b2b$vat_buyer))
  }
  subset_vat <- subset_vat[!is.na(subset_vat)]
  n <- length(subset_vat); idx <- setNames(seq_len(n), subset_vat)
  message(sprintf("  [assemble] subset = %d firms; building cost base + Omega ...", n))

  # Total cost = FULL B2B purchases + labor (no carbon, closed economy)
  # (group only over subset buyers, not the whole economy)
  full_inputs <- b2b %>% filter(vat_buyer %in% subset_vat) %>%
    group_by(vat_buyer) %>% summarise(inputs = sum(corr_sales), .groups = "drop")
  message("  [assemble] input costs aggregated; assembling Omega ...")
  wages_aa  <- aa %>% filter(year == yr, !is.na(wage_bill)) %>% select(vat, wage_bill)
  wages_ets <- firm_year_belgian_euets %>% filter(year == yr, !is.na(wage_bill)) %>%
    select(vat, wage_bill_ets = wage_bill)
  ftab <- tibble(vat = subset_vat) %>%
    left_join(full_inputs, by = c("vat" = "vat_buyer")) %>% mutate(inputs = coalesce(inputs, 0)) %>%
    left_join(wages_aa, by = "vat") %>% left_join(wages_ets, by = "vat") %>%
    mutate(wage_bill = coalesce(wage_bill, wage_bill_ets, 0), total_cost = inputs + wage_bill)
  total_cost <- setNames(ftab$total_cost, ftab$vat); total_cost[total_cost <= 0] <- NA_real_

  # Omega on within-subset edges (no-carbon cost base)
  b2b_sub <- b2b %>% filter(vat_supplier %in% subset_vat, vat_buyer %in% subset_vat)
  exp_mat <- sparseMatrix(i = idx[b2b_sub$vat_buyer], j = idx[b2b_sub$vat_supplier],
                          x = b2b_sub$corr_sales, dims = c(n, n))
  Omega <- exp_mat
  rc <- total_cost[Omega@i + 1L]; ok <- !is.na(rc) & rc > 0
  Omega@x[ok] <- Omega@x[ok] / rc[ok]; Omega@x[!ok] <- 0
  rs <- rowSums(Omega)
  # Cap materials share at MAX_MAT (floor value-added/labor share at 1-MAX_MAT).
  # Bounds the price-map spectral radius away from 1 -> the price fixed point
  # converges in ~hundreds of iters instead of thousands. Row-scaling via a
  # diagonal (correct for dgCMatrix; the old per-@p loop indexed columns as rows).
  MAX_MAT <- 0.95
  scl <- ifelse(rs > MAX_MAT, MAX_MAT / rs, 1)
  if (any(scl < 1)) Omega <- as(Diagonal(x = scl) %*% Omega, "CsparseMatrix")
  rs <- rowSums(Omega)
  gamma <- 1 - rs
  message(sprintf("  [assemble] %d firms capped at materials share %.2f; max rowsum now %.4f",
                  sum(scl < 1), MAX_MAT, max(rs, na.rm = TRUE)))

  # Emissions (from the allocation file) and intensity
  z <- setNames(rep(0, n), subset_vat)
  em <- year_firms %>% filter(vat %in% subset_vat) %>% select(vat, scope1)
  z[em$vat] <- em$scope1
  e_bar <- as.numeric(z / total_cost); e_bar[!is.finite(e_bar)] <- 0

  # Size (deflated revenue) + NACE
  rev_tab <- bind_rows(
    firm_year_belgian_euets %>% filter(year == yr, in_sample == 1) %>% select(vat, revenue, nace5d),
    deployment_panel %>% filter(year == yr) %>% select(vat, revenue, nace5d)
  ) %>% distinct(vat, .keep_all = TRUE) %>% filter(vat %in% subset_vat) %>%
    mutate(nace4d = str_sub(nace5d, 1, 4), nace2d = str_sub(nace5d, 1, 2)) %>%
    left_join(deflator %>% filter(year == yr) %>% select(nace4d, ppi), by = "nace4d") %>%
    left_join(deflator_2d_only %>% filter(year == yr) %>% select(nace2d, ppi_2d = ppi), by = "nace2d") %>%
    mutate(ppi = coalesce(ppi, ppi_2d),
           real_revenue = ifelse(!is.na(ppi) & ppi > 0, revenue / ppi * 100, revenue))
  x <- setNames(rep(NA_real_, n), subset_vat); x[rev_tab$vat] <- rev_tab$real_revenue
  nace2d <- setNames(rep(NA_character_, n), subset_vat); nace2d[rev_tab$vat] <- rev_tab$nace2d

  tau <- as.integer(subset_vat %in% ets_vat)
  message("  [assemble] done.")

  list(meta = list(year = yr, scope = scope, n = n, n_ets = sum(tau),
                   max_rowsum = max(rowSums(Omega), na.rm = TRUE)),
       firms = subset_vat, Omega = Omega, e_bar = e_bar, gamma = gamma,
       tau = tau, x = x, z = z, total_cost = total_cost, nace2d = nace2d)
}

# Targeting vector tau for a given scheme.
get_tau <- function(scheme, bundle) {
  if (scheme == "actual_ets") return(bundle$tau)               # actual EU ETS
  if (scheme == "universal_industrial") {                      # all industrial emitters
    ind <- sprintf("%02d", c(5:9, 10:33, 35))                  # mining, manufacturing, energy
    return(as.integer(!is.na(bundle$nace2d) & bundle$nace2d %in% ind))
  }
  stop(sprintf("scheme '%s' not handled by get_tau (centrality is built in cf_common.R)", scheme))
}
