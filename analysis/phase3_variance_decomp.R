###############################################################################
# analysis/phase3_variance_decomp.R
#
# PURPOSE
#   For each (sector, year) cell, decompose the LINEAR variance of the
#   supplier-embodied intensity u_i = nu_i - e^cost_i = (Omega %*% nu)_i into:
#
#     var(u) = var(across) + var(within) + 2 cov(across, within)
#
#   where across_g_i = (Omega %*% nu_bar_g)_i (input mix times sector-mean
#   multiplier) and within_g_i = u_i - across_g_i (firm-level supplier
#   selection), with g in {2-digit, 4-digit}. Report shares:
#
#     share_across = var(across) / var(u)
#     share_within = var(within) / var(u)
#     share_cov    = 2 cov(across, within) / var(u)        (sum to 1)
#
#   The "across" component captures dispersion driven by *which supplier
#   sectors* a firm sources from; "within" captures *which specific firms
#   within a supplier sector* it sources from.
#
# INPUT  {PROJ}/data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUT {PROJ}/data/processed/variance_decomp_{s1,s2}.RData
#          (vd_4d_2dgroup, vd_4d_4dgroup, vd_2d_2dgroup, vd_2d_4dgroup per scn)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

if (Sys.info()[["user"]] == "JARDANG") {
  project_root <- "X:/Documents/JARDANG/carbon_policy_networks"
} else {
  project_root <- "c:/Users/jota_/Documents/carbon_policy_networks"
}
proc <- file.path(project_root, "data", "processed")
source(file.path(project_root, "utils", "sector_conventions.R"))

cat("== phase3_variance_decomp ==\n")

load_build <- function(scn) {
  d <- file.path(proc, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L)
}

# vd_cell: per (cell, year), variance decomposition using the supplier-sector
# grouping `supp_g` (one of 2d, 4d). `cell_g` is the granularity at which we
# define the buyer-sector cell over which the variance is taken.
vd_cell <- function(firms, cell_g, supp_g) {
  across_col <- sprintf("across_%s", supp_g)
  within_col <- sprintf("within_%s", supp_g)
  firms %>%
    mutate(grp = .data[[cell_g]],
           a   = .data[[across_col]],
           w   = .data[[within_col]],
           u   = nu - e_cost) %>%
    filter(!is.na(grp), is.finite(u)) %>%
    group_by(grp, year) %>%
    summarise(n_firms      = n(),
              var_u        = var(u),
              var_across   = var(a),
              var_within   = var(w),
              cov2_aw      = 2 * cov(a, w),
              .groups      = "drop") %>%
    filter(n_firms >= MIN_N_STATS, var_u > 0) %>%
    mutate(share_across = var_across / var_u,
           share_within = var_within / var_u,
           share_cov    = cov2_aw   / var_u,
           identity_err = share_across + share_within + share_cov - 1) %>%
    rename(!!cell_g := grp)
}

summarize_shares <- function(vd) {
  # Per-cell shares can have magnitude > 1 when var(u) is small relative to
  # var(a) and var(w) (negatively correlated across/within: firms compensate
  # sourcing pattern against within-sector firm choice). Mean across cells is
  # then dominated by tiny-var(u) cells; report median (robust) plus a pooled
  # total-variance ratio sum(var_a)/sum(var_u) that weights cells by their
  # variance magnitude.
  vd %>% summarise(
    n_cells     = n(),
    n_extreme   = sum(abs(share_cov) > 1, na.rm = TRUE),
    med_across  = median(share_across, na.rm = TRUE),
    med_within  = median(share_within, na.rm = TRUE),
    med_cov     = median(share_cov,    na.rm = TRUE),
    pool_across = sum(var_across, na.rm = TRUE) / sum(var_u, na.rm = TRUE),
    pool_within = sum(var_within, na.rm = TRUE) / sum(var_u, na.rm = TRUE),
    pool_cov    = sum(cov2_aw,    na.rm = TRUE) / sum(var_u, na.rm = TRUE),
    max_id_err  = max(abs(identity_err), na.rm = TRUE))
}

# Sanity-check: how many cells fall in the degenerate regime (|share_cov| > 1)
# at progressively stricter firm-count thresholds? On the full RMD network the
# fraction degenerate should drop sharply as n_firms grows; on a sparse network
# (e.g. local downsampled B2B) it stays high at every threshold and tells you
# the share magnitudes are not interpretable.
sanity_check <- function(vd, thresholds = c(3L, 10L, 30L, 100L)) {
  do.call(rbind, lapply(thresholds, function(k) {
    sub <- vd[vd$n_firms >= k, , drop = FALSE]
    if (nrow(sub) == 0L)
      return(data.frame(min_n = k, n_cells = 0L,
                        frac_degen = NA_real_,
                        med_across_int = NA_real_,
                        med_within_int = NA_real_,
                        med_cov_int    = NA_real_))
    interp <- sub[abs(sub$share_cov) <= 1, , drop = FALSE]
    data.frame(min_n = k,
               n_cells     = nrow(sub),
               frac_degen  = mean(abs(sub$share_cov) > 1, na.rm = TRUE),
               med_across_int = if (nrow(interp) > 0L)
                                  median(interp$share_across, na.rm = TRUE) else NA_real_,
               med_within_int = if (nrow(interp) > 0L)
                                  median(interp$share_within, na.rm = TRUE) else NA_real_,
               med_cov_int    = if (nrow(interp) > 0L)
                                  median(interp$share_cov,    na.rm = TRUE) else NA_real_)
  }))
}

for (scn in c("s1", "s2")) {
  firms <- load_build(scn)
  cat(sprintf("\n[%s] firms loaded: %d\n", toupper(scn), nrow(firms)))

  vd_4d_2dgroup <- vd_cell(firms, "nace4d", "2d")
  vd_4d_4dgroup <- vd_cell(firms, "nace4d", "4d")
  vd_2d_2dgroup <- vd_cell(firms, "nace2d", "2d")
  vd_2d_4dgroup <- vd_cell(firms, "nace2d", "4d")

  objs <- list("4d cells, 2d suppliers" = vd_4d_2dgroup,
               "4d cells, 4d suppliers" = vd_4d_4dgroup,
               "2d cells, 2d suppliers" = vd_2d_2dgroup,
               "2d cells, 4d suppliers" = vd_2d_4dgroup)

  # Robust summary table (median + pooled shares)
  cat("                                  | cells |extreme| med_across | med_within | med_cov | pool_across | pool_within | pool_cov | id_err\n")
  for (lbl in names(objs)) {
    s <- summarize_shares(objs[[lbl]])
    cat(sprintf("   %-30s | %5d | %5d | %+.3f     | %+.3f     | %+.3f  | %+.3f      | %+.3f      | %+.3f   | %.1e\n",
                lbl, s$n_cells, s$n_extreme,
                s$med_across, s$med_within, s$med_cov,
                s$pool_across, s$pool_within, s$pool_cov, s$max_id_err))
  }

  # Sanity-check report: fraction degenerate (|share_cov| > 1) at progressively
  # stricter firm-count thresholds. Medians shown are over interpretable cells
  # only (|share_cov| <= 1). On full data the frac_degen column should fall
  # sharply with min_n; if it stays high, the shares are not interpretable.
  sanity_tbl <- list()
  cat("\n  -- Sanity: degeneracy by firm-count threshold (interpretable = |share_cov| <= 1) --\n")
  cat("                                  | min_n | n_cells | frac_degen | med_across_int | med_within_int | med_cov_int\n")
  for (lbl in names(objs)) {
    sc <- sanity_check(objs[[lbl]])
    sanity_tbl[[lbl]] <- sc
    for (i in seq_len(nrow(sc))) {
      cat(sprintf("   %-30s | %5d | %7d | %.3f      | %+.3f         | %+.3f         | %+.3f\n",
                  if (i == 1L) lbl else "",
                  sc$min_n[i], sc$n_cells[i], sc$frac_degen[i],
                  sc$med_across_int[i], sc$med_within_int[i], sc$med_cov_int[i]))
    }
  }

  save(vd_4d_2dgroup, vd_4d_4dgroup, vd_2d_2dgroup, vd_2d_4dgroup, sanity_tbl,
       file = file.path(proc, sprintf("variance_decomp_%s.RData", scn)))
}

cat("\nDone.\n")
