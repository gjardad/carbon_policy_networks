###############################################################################
# run_counterfactuals_rmd.R  —  end-to-end runner (assembly -> solve -> calibrate
#                               -> counterfactuals), meant to run on RMD where the
#                               FULL B2B network is available.
#
# Chains: assemble bundle (full B2B) -> source phase5 solver -> calibrate
#   alpha(sigma,rho) to the Belgian within-cell change -> price x targeting matrix.
#
# Self-contained except for phase5_model_solver.R (sourced for the solver fns).
# Paths auto-resolve by machine (utils/paths.R) -- no editing needed; just Rscript it.
###############################################################################

suppressMessages({ library(dplyr); library(stringr); library(Matrix) })

# ---- Paths (auto-detect machine via utils/paths.R: jardang=RMD, jota_=local) ----
.path_candidates <- c(
  "C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
  "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
.p <- .path_candidates[file.exists(.path_candidates)]
if (length(.p) == 0L) stop("Cannot locate utils/paths.R; add a candidate.")
source(.p[1])                   # sets project_root, proc_data, out_data, ...
CODE_DIR <- file.path(project_root, "analysis")
rm(.path_candidates, .p)

# ================================ CONFIG ===================================
YEAR  <- 2019
SCOPE <- "ets_neighbors"        # "ets_neighbors" (tractable, clean) | "full" (whole panel)

SIGMA_GRID <- c(0.034, 0.5, 1, 4.7)
RHO_GRID   <- c(0.25, 0.5, 0.75, 1)
DEF_SIGMA  <- 0.5
DEF_RHO    <- 1

# Calibration target: Belgian within-cell emission change, 2005->2022 = -24pp.
# Framing A (default): endpoints 18 -> 80 EUR/tCO2. Robustness framing B below.
CAL_PLO <- 18; CAL_PHI <- 80; TARGET_DLOGZ <- log(0.76)
ROBUST_FRAMINGS <- list(            # (p_lo, p_hi) variants for the alpha robustness axis
  endpoints_18_80 = c(18, 80),
  nopolicy_term80 = c(0,  80),
  nopolicy_avg19  = c(0,  19)
)
PRICE_GRID <- c(80, 100, 150, 200, 250)     # counterfactual columns
SCHEMES    <- c("T0")                        # add "T1" once industrial NACE confirmed
FULL_GRID  <- FALSE   # FALSE: calibrate only the default (sigma,rho) cell + price sweep
                      #        (fast first run). TRUE: full sigma/rho sweep + robustness.
# ===========================================================================

# ---- Direct sparse Leontief apply: (I - Omega)^{-1} v  (replaces Neumann) ----
leontief_apply <- function(Omega, v) {
  n <- nrow(Omega)
  as.numeric(Matrix::solve(Diagonal(n) - Omega, v))
}

# =================== ASSEMBLY (full B2B; adapted from phase4) ================
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
  # NB: the network-adjusted intensity nu = (I-Omega)^{-1} e_bar is a DIAGNOSTIC only
  # (the solver computes its own equilibrium objects). It is a large near-singular
  # sparse solve, so we skip it here to keep assembly fast on the full network.
  message("  [assemble] done.")

  list(meta = list(year = yr, scope = scope, n = n, n_ets = sum(tau),
                   max_rowsum = max(rowSums(Omega), na.rm = TRUE)),
       firms = subset_vat, Omega = Omega, e_bar = e_bar, gamma = gamma,
       tau = tau, x = x, z = z, total_cost = total_cost, nace2d = nace2d)
}

# ============================ TARGETING SCHEMES =============================
get_tau <- function(scheme, bundle) {
  if (scheme == "T0") return(bundle$tau)                       # actual EU ETS
  if (scheme == "T1") {                                        # universal industrial
    ind <- sprintf("%02d", c(5:9, 10:33, 35))                  # mining, manufacturing, energy
    return(as.integer(!is.na(bundle$nace2d) & bundle$nace2d %in% ind))
  }
  stop(sprintf("scheme %s not implemented (T2 centrality / T3 sector pending)", scheme))
}

# ================================== RUN ====================================
cat(sprintf("Assembling bundle: year=%d scope=%s ...\n", YEAR, SCOPE))
bundle <- assemble_bundle(YEAR, SCOPE, proc_data, out_data)
cat(sprintf("  %d firms, %d ETS, max rowsum=%.4f\n", bundle$meta$n, bundle$meta$n_ets, bundle$meta$max_rowsum))

source(file.path(CODE_DIR, "phase5_model_solver.R"))        # solver + calibrate_alpha (fns only)
bshare <- base_final_shares(bundle)

decomp <- function(base, cf) {                                 # scale/technique/reallocation
  wz <- base$z / sum(base$z)
  technique <- sum(wz * (log(cf$e + 1e-300) - log(base$e + 1e-300)))
  dlogZ <- log(cf$Z) - log(base$Z)
  c(dlogZ = dlogZ, technique = technique, quantity = dlogZ - technique)
}

# ---- timing probe: how long is one solve? (calibration does ~25-35 per cell) ----
cat("\nTiming one full_solve ...\n"); .t0 <- Sys.time()
.probe <- full_solve(80, DEF_SIGMA, DEF_RHO, 2, bundle, bshare)
cat(sprintf("  one full_solve = %.1f sec  (Z=%.4g) -> expect ~%.0f min per calibrated cell\n",
            as.numeric(difftime(Sys.time(), .t0, units = "secs")), .probe$Z,
            as.numeric(difftime(Sys.time(), .t0, units = "secs")) * 30 / 60))

# ---- 1. Calibrate alpha(sigma,rho): default cell (+ sweeps if FULL_GRID) ----
cells <- if (FULL_GRID) unique(rbind(
  data.frame(sigma = DEF_SIGMA, rho = DEF_RHO),
  data.frame(sigma = SIGMA_GRID, rho = DEF_RHO),
  data.frame(sigma = DEF_SIGMA, rho = RHO_GRID)
)) else data.frame(sigma = DEF_SIGMA, rho = DEF_RHO)
cat(sprintf("\nCalibrating alpha(sigma,rho) over %d cell(s)%s ...\n",
            nrow(cells), if (FULL_GRID) "" else " [FULL_GRID=FALSE: default cell only]"))
cal <- lapply(seq_len(nrow(cells)), function(i) {
  s <- cells$sigma[i]; r <- cells$rho[i]
  cat(sprintf("  cell %d/%d: sigma=%.3f rho=%.2f -> solving ...\n", i, nrow(cells), s, r))
  res <- calibrate_alpha(s, r, bundle, bshare, CAL_PLO, CAL_PHI, TARGET_DLOGZ)
  cat(sprintf("    alpha=%s bracketed=%s\n",
              ifelse(is.na(res$alpha), "NA", sprintf("%.3f", res$alpha)), res$bracketed))
  gc()    # release sparse-LU memory between cells (avoids SuiteSparse fault over many solves)
  data.frame(sigma = s, rho = r, alpha = res$alpha, bracketed = res$bracketed)
}) %>% bind_rows()
write.csv(cal, file.path(out_data, "cf_calibrated_alpha.csv"), row.names = FALSE)

# ---- 2. Robustness: alpha under alternative price framings (only if FULL_GRID) ----
if (FULL_GRID) {
  robust <- lapply(names(ROBUST_FRAMINGS), function(nm) {
    pr <- ROBUST_FRAMINGS[[nm]]
    res <- calibrate_alpha(DEF_SIGMA, DEF_RHO, bundle, bshare, pr[1], pr[2], TARGET_DLOGZ)
    gc()
    data.frame(framing = nm, p_lo = pr[1], p_hi = pr[2], alpha = res$alpha, bracketed = res$bracketed)
  }) %>% bind_rows()
  print(robust)
  write.csv(robust, file.path(out_data, "cf_alpha_robustness.csv"), row.names = FALSE)
}

# ---- 3. Counterfactual matrix: scheme x price, at each calibrated cell ----
cat("\nRunning counterfactual matrix ...\n")
results <- list(); k <- 1
for (i in seq_len(nrow(cal))) {
  s <- cal$sigma[i]; r <- cal$rho[i]; a <- cal$alpha[i]
  if (is.na(a)) next
  base0 <- full_solve(0, s, r, a, bundle, bshare)            # no-policy reference
  for (sch in SCHEMES) {
    tau_s <- get_tau(sch, bundle); bsch <- bundle; bsch$tau <- tau_s
    for (pz in PRICE_GRID) {
      cf <- full_solve(pz, s, r, a, bsch, bshare)
      d  <- decomp(base0, cf)
      results[[k]] <- data.frame(scheme = sch, p_z = pz, sigma = s, rho = r, alpha = a,
                                 dlogZ = d["dlogZ"], technique = d["technique"], quantity = d["quantity"],
                                 mean_price = mean(cf$p)); k <- k + 1
    }
  }
  gc()    # release sparse-LU memory between calibrated cells
}
results <- bind_rows(results)
write.csv(results, file.path(out_data, "cf_results.csv"), row.names = FALSE)
cat(sprintf("\nDone. Wrote: cf_calibrated_alpha.csv, cf_alpha_robustness.csv, cf_results.csv to %s\n", out_data))
print(utils::head(results, 12))
