###############################################################################
# analysis/phase3_build_upstream_emissions.R
#
# PURPOSE
#   Cost-normalized network build of firm-level emission intensities, for each
#   scenario (S1 zero / S2 GLO) and year. Adapted from
#   facts-emissions-across-network/analysis/build_upstream_emissions_glo.R,
#   stripped to a single point estimate (no uncertainty draws), with a 4-digit
#   decomposition added and outputs expressed as cost-based INTENSITIES.
#
# DEFINITIONS (cost normalization; see plan "Definitions"):
#   total_cost_i = wages + domestic B2B inputs + imports + carbon cost
#   Omega_{ij}   = expenditure_ij / total_cost_i           (row sums < 1)
#   e_cost_i     = z_i / total_cost_i                       (direct EI, cost)
#   nu_i = (I-Omega)^{-1} e_cost                            (network-adj EI, incl direct)
#   u_i  = nu_i - e_cost_i = (Omega %*% nu)_i               (upstream part)
#   across_g_i = (Omega %*% nu_bar_g)_i ; within_g_i = u_i - across_g_i
#     (nu_bar_g = mean nu over supplier's sector g; g in {2d, 4d})
#   => nu = e_cost + across_g + within_g exactly (verify to machine precision).
#
# SCENARIO emission vector z (tonnes CO2):
#   S2: alloc scope1 (all sources: ets + imputed + pre_ets)
#   S1: alloc scope1 where source == "ets" only (non-ETS = 0)
#
# INPUT (NBB processed):
#   allocation_glo_balanced/alloc_YYYY.RData, b2b_selected_sample.RData,
#   firm_year_belgian_euets.RData, annual_accounts_selected_sample_key_variables.RData,
#   firm_year_total_imports.RData, deployment_panel.RData
#
# OUTPUT {PROJ}/data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
#   firms: vat, year, source, nace5d, nace4d, nace2d, total_cost, revenue,
#          scope1 (z), e_cost, nu, u, across_2d, within_2d, across_4d, within_4d
#   conv_info, max_rowsum
#
# RUNS ON: RMD for real results (full B2B). Locally runs on DOWNSAMPLED B2B for
#   code validation only (magnitudes meaningless).
###############################################################################

rm(list = ls())
suppressPackageStartupMessages({ library(dplyr); library(Matrix) })

# ---- Paths (centralized in utils/paths.R) ----
.path_candidates <- c(
  "C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
  "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
.p <- .path_candidates[file.exists(.path_candidates)]
if (length(.p) == 0L) stop("Cannot locate utils/paths.R; add a candidate.")
source(.p[1])
source(file.path(project_root, "utils", "sector_conventions.R"))
rm(.path_candidates, .p)

# ---- Parameters ----
SCENARIOS     <- c("s1", "s2")
YEARS         <- 2005:2021
NEUMANN_MAXIT <- 50L
NEUMANN_TOL   <- 1e-8
ALLOC_DIR     <- file.path(proc_data, "allocation_glo_balanced")

# Allow a single-year quick validation run: Rscript phase3_build...R 2015 s2
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) >= 1 && .args[1] != "") YEARS     <- as.integer(strsplit(.args[1], ",")[[1]])
if (length(.args) >= 2 && .args[2] != "") SCENARIOS <- strsplit(.args[2], ",")[[1]]

CARBON_PRICE <- c(
  "2005" = 25.29, "2006" = 21.53, "2007" =  0.86, "2008" = 25.74,
  "2009" = 18.41, "2010" = 18.98, "2011" = 18.08, "2012" =  9.49,
  "2013" =  5.94, "2014" =  7.89, "2015" =  8.52, "2016" =  5.92,
  "2017" =  6.63, "2018" = 18.55, "2019" = 27.84, "2020" = 27.94,
  "2021" = 62.25)

neumann_series <- function(A, epsilon, maxit, tol) {
  m <- as.numeric(epsilon); term <- as.numeric(epsilon)
  k_final <- 0L; rel_final <- Inf
  for (k in seq_len(maxit)) {
    term <- as.numeric(A %*% term); m <- m + term
    rel_final <- max(abs(term)) / (max(abs(m)) + 1e-15); k_final <- k
    if (rel_final < tol) break
  }
  list(m = m, k = k_final, converged = rel_final < tol, rel_err = rel_final)
}

cat("== phase3_build_upstream_emissions ==\n")
cat(sprintf("  user=%s | years=%s | scenarios=%s\n",
            Sys.info()[["user"]], paste(range(YEARS), collapse="-"),
            paste(SCENARIOS, collapse=",")))

# ---- Shared data ----
load(file.path(proc_data, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample %>% filter(year %in% YEARS)
rm(df_b2b_selected_sample)
cat(sprintf("  B2B rows: %d\n", nrow(b2b)))

load(file.path(proc_data, "firm_year_belgian_euets.RData"))
eutl <- firm_year_belgian_euets %>% filter(year %in% YEARS) %>% select(vat, year, emissions)
ets_rev <- firm_year_belgian_euets %>% filter(year %in% YEARS) %>%
  transmute(vat, year, nace5d, revenue)
rm(firm_year_belgian_euets)

load(file.path(proc_data, "annual_accounts_selected_sample_key_variables.RData"))
accounts <- df_annual_accounts_selected_sample_key_variables %>%
  filter(year %in% YEARS) %>%
  transmute(vat, year, nace5d, wage_bill = pmax(coalesce(wage_bill, 0), 0))
rm(df_annual_accounts_selected_sample_key_variables)

load(file.path(proc_data, "firm_year_total_imports.RData"))
if ("vat_ano" %in% names(firm_year_total_imports))
  firm_year_total_imports <- rename(firm_year_total_imports, vat = vat_ano)
imports <- firm_year_total_imports %>% filter(year %in% YEARS) %>%
  select(vat, year, total_imports)
rm(firm_year_total_imports)

# Revenue + NACE lookup (union of deployment_panel non-ETS and euets ETS firms)
load(file.path(proc_data, "deployment_panel.RData"))
firm_chars <- bind_rows(
    deployment_panel %>% filter(year %in% YEARS) %>% transmute(vat, year, nace5d, revenue),
    ets_rev
  ) %>%
  filter(!is.na(nace5d)) %>%
  distinct(vat, year, .keep_all = TRUE) %>%
  mutate(nace2d = make_nace2d(nace5d), nace4d = make_nace4d(nace5d))
rm(deployment_panel)

# ---- Scenario emission vectors from allocation ----
load_alloc <- function() {
  L <- list()
  for (t in YEARS) {
    p <- file.path(ALLOC_DIR, sprintf("alloc_%d.RData", t))
    if (file.exists(p)) { load(p); L[[as.character(t)]] <- year_firms %>% select(vat, year, scope1, source) }
  }
  bind_rows(L)
}
alloc_all <- load_alloc()
z_for <- function(scn) if (scn == "s1") filter(alloc_all, source == "ets") else alloc_all

# =============================================================================
# Per-year worker (called from parallel cluster or single-threaded fallback)
# =============================================================================
build_year_one <- function(t, b2b_t, eutl_t, acc_t, imp_t, chars_t, z_t,
                           out_dir, scn) {
  if (nrow(b2b_t) == 0L) return(NULL)

  all_vats <- sort(unique(c(b2b_t$vat_i_ano, b2b_t$vat_j_ano)))
  N <- length(all_vats); vat_idx <- setNames(seq_len(N), all_vats)

  b2b_agg <- b2b_t %>% group_by(vat_i_ano, vat_j_ano) %>%
    summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    filter(sales > 0)

  # cost_vec = wages + domestic inputs + imports + carbon cost
  cost_vec <- rep(1e-6, N)
  rs <- b2b_agg %>% group_by(vat_j_ano) %>% summarise(d = sum(sales), .groups = "drop")
  bi <- vat_idx[rs$vat_j_ano]; ok <- !is.na(bi)
  cost_vec[bi[ok]] <- cost_vec[bi[ok]] + rs$d[ok]
  ai <- match(acc_t$vat, all_vats); oka <- !is.na(ai) & acc_t$wage_bill > 0
  cost_vec[ai[oka]] <- cost_vec[ai[oka]] + acc_t$wage_bill[oka]
  ii <- match(imp_t$vat, all_vats); oki <- !is.na(ii) & !is.na(imp_t$total_imports) & imp_t$total_imports > 0
  cost_vec[ii[oki]] <- cost_vec[ii[oki]] + imp_t$total_imports[oki]
  ei <- match(eutl_t$vat, all_vats); oke <- !is.na(ei) & !is.na(eutl_t$emissions) & eutl_t$emissions > 0
  cost_vec[ei[oke]] <- cost_vec[ei[oke]] + eutl_t$emissions[oke] * CARBON_PRICE[as.character(t)]

  # A[buyer, supplier] = expenditure / total_cost(buyer)
  row_i <- vat_idx[b2b_agg$vat_j_ano]; col_j <- vat_idx[b2b_agg$vat_i_ano]
  okij  <- !is.na(row_i) & !is.na(col_j)
  A <- sparseMatrix(i = row_i[okij], j = col_j[okij],
                    x = b2b_agg$sales[okij] / cost_vec[row_i[okij]], dims = c(N, N))
  max_rowsum <- max(rowSums(A))

  # emission intensity vector e_cost = z / cost
  eps <- rep(0, N)
  zi <- match(z_t$vat, all_vats); okz <- !is.na(zi)
  eps[zi[okz]] <- z_t$scope1[okz] / cost_vec[zi[okz]]
  source_vec <- rep("none", N); source_vec[zi[okz]] <- z_t$source[okz]
  scope1_vec <- cost_vec * eps

  ns <- neumann_series(A, eps, NEUMANN_MAXIT, NEUMANN_TOL)
  nu <- ns$m; u <- nu - eps

  # decomposition (one application of A onto sector-mean nu)
  cm <- match(all_vats, chars_t$vat)
  nace2d_vec <- chars_t$nace2d[cm]; nace4d_vec <- chars_t$nace4d[cm]
  nace5d_vec <- chars_t$nace5d[cm]; rev_vec <- chars_t$revenue[cm]

  decomp <- function(sec_vec) {
    nb <- nu; ok <- !is.na(sec_vec)
    if (any(ok)) { sm <- tapply(nu[ok], sec_vec[ok], mean); nb[ok] <- sm[sec_vec[ok]] }
    across <- as.numeric(A %*% nb)
    list(across = across, within = u - across)
  }
  d2 <- decomp(nace2d_vec); d4 <- decomp(nace4d_vec)

  # Same-sector / other-sector decomposition of u (first-step by immediate
  # supplier sector). Since u = A %*% nu, the split is clean:
  #   u_same  = (A masked to same-sector pairs) %*% nu
  #   u_other = u - u_same
  # Higher-order chains are captured because nu propagates through them.
  triplet_rows <- row_i[okij]; triplet_cols <- col_j[okij]
  triplet_vals <- b2b_agg$sales[okij] / cost_vec[triplet_rows]

  build_A_same <- function(sec_vec) {
    same <- !is.na(sec_vec[triplet_rows]) & !is.na(sec_vec[triplet_cols]) &
            sec_vec[triplet_rows] == sec_vec[triplet_cols]
    sparseMatrix(i = triplet_rows[same], j = triplet_cols[same],
                 x = triplet_vals[same], dims = c(N, N))
  }
  A_same_4d <- build_A_same(nace4d_vec)
  A_same_2d <- build_A_same(nace2d_vec)

  u_same_4d  <- as.numeric(A_same_4d %*% nu)
  u_same_2d  <- as.numeric(A_same_2d %*% nu)
  u_other_4d <- u - u_same_4d
  u_other_2d <- u - u_same_2d

  firms <- data.frame(
    vat = all_vats, year = t, source = source_vec,
    nace5d = nace5d_vec, nace4d = nace4d_vec, nace2d = nace2d_vec,
    total_cost = cost_vec, revenue = rev_vec,
    scope1 = scope1_vec, e_cost = eps, nu = nu, u = u,
    across_2d = d2$across, within_2d = d2$within,
    across_4d = d4$across, within_4d = d4$within,
    u_same_4d = u_same_4d, u_other_4d = u_other_4d,
    u_same_2d = u_same_2d, u_other_2d = u_other_2d,
    stringsAsFactors = FALSE)
  firms <- firms[firms$nu > 0, ]

  id_err <- max(abs(firms$nu - (firms$e_cost + firms$across_4d + firms$within_4d)))
  conv_info <- data.frame(year = t, scenario = scn, N = N, n_firms = nrow(firms),
                          max_rowsum = max_rowsum, k = ns$k,
                          rel_err = ns$rel_err, converged = ns$converged,
                          id_err = id_err)
  save(firms, conv_info, max_rowsum,
       file = file.path(out_dir, sprintf("firms_%d.RData", t)))
  conv_info
}

# =============================================================================
# Pre-slice scenario-invariant inputs by year. Workers receive only their
# year's slice, so memory does not blow up across the cluster.
# =============================================================================
slice_by_year <- function(df, years) {
  setNames(lapply(years, function(t) df[df$year == t, , drop = FALSE]),
           as.character(years))
}

static_slices <- list(
  b2b      = slice_by_year(b2b,        YEARS),
  eutl     = slice_by_year(eutl,       YEARS),
  accounts = slice_by_year(accounts,   YEARS),
  imports  = slice_by_year(imports,    YEARS),
  chars    = slice_by_year(firm_chars, YEARS))
rm(b2b, eutl, accounts, imports, firm_chars); gc(verbose = FALSE)

# =============================================================================
# Parallel (or serial fallback) build per scenario x year
# =============================================================================
N_CORES_SET <- if (tolower(Sys.info()[["user"]]) == "jardang") {
  40L
} else {
  max(1L, parallel::detectCores(logical = FALSE) - 2L)
}
n_workers   <- min(length(YEARS), N_CORES_SET)
use_par     <- n_workers > 1L

for (scn in SCENARIOS) {
  out_dir <- file.path(out_data, sprintf("upstream_emissions_%s", scn))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  z_panel  <- z_for(scn)
  z_slices <- slice_by_year(z_panel, YEARS)

  inputs <- lapply(YEARS, function(t) {
    ts <- as.character(t)
    list(t = t,
         b2b_t   = static_slices$b2b[[ts]],
         eutl_t  = static_slices$eutl[[ts]],
         acc_t   = static_slices$accounts[[ts]],
         imp_t   = static_slices$imports[[ts]],
         chars_t = static_slices$chars[[ts]],
         z_t     = z_slices[[ts]],
         out_dir = out_dir,
         scn     = scn)
  })

  cat(sprintf("\n-- Scenario %s | %s | workers=%d --\n",
              toupper(scn), if (use_par) "PARALLEL" else "SERIAL", n_workers))
  t0 <- Sys.time()

  if (use_par) {
    cl <- parallel::makeCluster(n_workers)
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
    parallel::clusterEvalQ(cl, suppressPackageStartupMessages({
      library(dplyr); library(Matrix)
    }))
    parallel::clusterExport(cl,
      varlist = c("build_year_one", "neumann_series",
                  "CARBON_PRICE", "NEUMANN_MAXIT", "NEUMANN_TOL"),
      envir   = environment())
    convs <- parallel::clusterApplyLB(cl, inputs, function(inp) {
      with(inp, build_year_one(t, b2b_t, eutl_t, acc_t, imp_t, chars_t, z_t,
                               out_dir, scn))
    })
    parallel::stopCluster(cl); on.exit()
  } else {
    convs <- lapply(inputs, function(inp) {
      with(inp, build_year_one(t, b2b_t, eutl_t, acc_t, imp_t, chars_t, z_t,
                               out_dir, scn))
    })
  }

  conv_df <- bind_rows(convs[!sapply(convs, is.null)])
  save(conv_df, file = file.path(out_dir, "conv_summary.RData"))
  cat(sprintf("  finished %d years in %.1f min\n",
              nrow(conv_df),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  for (i in seq_len(nrow(conv_df))) {
    r <- conv_df[i, ]
    cat(sprintf("    %d: N=%d kept=%d | k=%d rel=%.1e rowsum=%.4f | id_err=%.1e\n",
                r$year, r$N, r$n_firms, r$k, r$rel_err, r$max_rowsum, r$id_err))
  }
}
cat("\nDone.\n")
