###############################################################################
# analysis/phase4_upstream_dispersion_comparisons.R
#
# PURPOSE
#   Extend the Phase B dispersion / correlation analysis with three new things
#   that feed sections 3.4.2 (upstream / network-adjusted dispersion) and 3.4.4
#   (within-sector correlations):
#
#   (a) Dispersion of u = nu - e^cost (upstream-only intensity), in parallel
#       with the existing dispersion-of-nu summary. 4d + 2d, S1 + S2.
#
#   (b) Within-cell S1-vs-S2 comparison for both nu and u:
#         - Spearman rank correlation between the S1 and S2 measure across
#           firms in the same (sector, year) cell. "Does adding the imputed
#           non-ETS tail reshuffle the within-cell ranking of firms by their
#           network-adjusted intensity?"
#         - Mean (and median) log(S2 / S1) within a cell. "By how much does
#           the imputation inflate nu (or u) on average?"
#
#   (c) Within-cell, within-scenario cor(u, nu) -- both Spearman and Pearson
#       on logs. "Do firms with high upstream-only intensity also have high
#       total network-adjusted intensity?"
#
# INPUTS  data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUTS output_{rmd,local}/tables/
#           - dispersion_u_summary.csv
#           - s1_vs_s2_within_cell.csv
#           - u_nu_corr_within_cell.csv
#
# RUNS ON: RMD for real numbers; locally on downsampled build output for
# code validation only.
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

# ---- Paths (centralized in utils/paths.R) ----
.path_candidates <- c(
  "C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
  "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
.p <- .path_candidates[file.exists(.path_candidates)]
if (length(.p) == 0L) stop("Cannot locate utils/paths.R; add a candidate.")
source(.p[1])
source(file.path(project_root, "utils", "sector_conventions.R"))
rm(.path_candidates, .p)

TBL_DIR <- file.path(output_dir, "tables")
dir.create(TBL_DIR, recursive = TRUE, showWarnings = FALSE)

cat("== phase4_upstream_dispersion_comparisons ==\n")

# Drop cells smaller than these thresholds for stats / correlations.
MIN_N_FOR_STATS <- 3L
MIN_N_FOR_CORR  <- 10L

# ============================================================================
# Load both scenarios. Each firms_YYYY.RData has: vat, year, nace4d, nace2d,
# total_cost, revenue, scope1, e_cost, nu, u, across_{2d,4d}, within_{2d,4d}.
# ============================================================================
load_build <- function(scn) {
  d <- file.path(out_data, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  if (length(fs) == 0L) stop("No build files in ", d)
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L) %>% select(vat, year, nace4d, nace2d, e_cost, nu, u)
}

firms_s1 <- load_build("s1")
firms_s2 <- load_build("s2")
cat(sprintf("S1 rows: %d  |  S2 rows: %d\n", nrow(firms_s1), nrow(firms_s2)))

# Joint panel for cross-scenario comparisons -- inner-join on (vat, year)
joint <- firms_s1 %>%
  rename(nu_s1 = nu, u_s1 = u, e_cost_s1 = e_cost) %>%
  inner_join(
    firms_s2 %>% select(vat, year, nu_s2 = nu, u_s2 = u, e_cost_s2 = e_cost),
    by = c("vat", "year"))
cat(sprintf("Joint S1xS2 firm-years: %d\n\n", nrow(joint)))

# ============================================================================
# Helpers
# ============================================================================

# Mean p9010 log gap per cell, then mean across cells.
disp_cells <- function(d, value_col, group_col,
                       min_n = MIN_N_FOR_STATS) {
  d %>%
    mutate(grp = .data[[group_col]], val = .data[[value_col]]) %>%
    filter(!is.na(grp), is.finite(val), val > 0) %>%
    group_by(grp, year) %>%
    summarise(n_firms   = n(),
              p9010_log = quantile(log(val), 0.9, na.rm = TRUE) -
                          quantile(log(val), 0.1, na.rm = TRUE),
              .groups   = "drop") %>%
    filter(n_firms >= min_n) %>%
    rename(!!group_col := grp)
}

# Per-cell stats: Spearman(s1,s2) + mean & median log(s2/s1).
s1s2_cells <- function(d, val_s1, val_s2, group_col,
                       min_n = MIN_N_FOR_CORR) {
  d %>%
    mutate(grp = .data[[group_col]],
           x   = .data[[val_s1]], y = .data[[val_s2]]) %>%
    filter(!is.na(grp), is.finite(x), is.finite(y), x > 0, y > 0) %>%
    group_by(grp, year) %>%
    summarise(
      n_firms          = n(),
      spearman         = if (n() >= 2L) suppressWarnings(cor(x, y, method = "spearman")) else NA_real_,
      mean_log_ratio   = mean(log(y) - log(x), na.rm = TRUE),
      median_log_ratio = median(log(y) - log(x), na.rm = TRUE),
      .groups          = "drop") %>%
    filter(n_firms >= min_n) %>%
    rename(!!group_col := grp)
}

# Per-cell stats: cor(u, nu) within scenario.
unu_cells <- function(d, group_col, min_n = MIN_N_FOR_CORR) {
  d %>%
    mutate(grp = .data[[group_col]]) %>%
    filter(!is.na(grp), is.finite(u), is.finite(nu), u > 0, nu > 0) %>%
    group_by(grp, year) %>%
    summarise(
      n_firms     = n(),
      spearman    = if (n() >= 2L) suppressWarnings(cor(u, nu, method = "spearman")) else NA_real_,
      pearson_log = if (n() >= 2L) suppressWarnings(cor(log(u), log(nu), method = "pearson")) else NA_real_,
      .groups     = "drop") %>%
    filter(n_firms >= min_n) %>%
    rename(!!group_col := grp)
}

# Compress a per-cell table into one summary row.
summarise_cells <- function(cells, value_cols) {
  out <- list(n_cells = nrow(cells),
              n_firms_mean = round(mean(cells$n_firms, na.rm = TRUE)))
  for (c in value_cols) out[[c]] <- mean(cells[[c]], na.rm = TRUE)
  as_tibble(out)
}

# ============================================================================
# (a) u dispersion summary, parallel to existing dispersion-of-nu
# ============================================================================
cat("-- (a) u dispersion --\n")

u_rows <- list()
for (scn_name in c("s1", "s2")) {
  d <- if (scn_name == "s1") firms_s1 else firms_s2
  for (gran in c("4d", "2d")) {
    grp_col <- paste0("nace", gran)
    cells   <- disp_cells(d, "u", grp_col)
    u_rows[[length(u_rows) + 1L]] <- tibble(
      scenario     = scn_name,
      granularity  = gran,
      object       = "u",
      n_cells      = nrow(cells),
      n_firms_mean = round(mean(cells$n_firms, na.rm = TRUE)),
      mean_log_gap = mean(cells$p9010_log, na.rm = TRUE),
      ratio_x      = exp(mean(cells$p9010_log, na.rm = TRUE)))
  }
}
dispersion_u <- bind_rows(u_rows) %>%
  mutate(across(c(mean_log_gap, ratio_x), ~round(.x, 3)))
print(dispersion_u, row.names = FALSE)
write.csv(dispersion_u, file.path(TBL_DIR, "dispersion_u_summary.csv"),
          row.names = FALSE, na = "")

# ============================================================================
# (b) S1-vs-S2 within-cell rank correlation + level change, for nu and u
# ============================================================================
cat("\n-- (b) S1 vs S2 within-cell comparison --\n")

s1s2_rows <- list()
for (m in c("nu", "u")) {
  v_s1 <- paste0(m, "_s1"); v_s2 <- paste0(m, "_s2")
  for (gran in c("4d", "2d")) {
    grp_col <- paste0("nace", gran)
    cells   <- s1s2_cells(joint, v_s1, v_s2, grp_col)
    s1s2_rows[[length(s1s2_rows) + 1L]] <- tibble(
      measure          = m,
      granularity      = gran,
      n_cells          = nrow(cells),
      n_firms_mean     = round(mean(cells$n_firms, na.rm = TRUE)),
      mean_spearman    = mean(cells$spearman, na.rm = TRUE),
      mean_log_ratio   = mean(cells$mean_log_ratio, na.rm = TRUE),
      median_log_ratio = mean(cells$median_log_ratio, na.rm = TRUE))
  }
}
s1_vs_s2 <- bind_rows(s1s2_rows) %>%
  mutate(across(c(mean_spearman, mean_log_ratio, median_log_ratio),
                ~round(.x, 3)))
print(s1_vs_s2, row.names = FALSE)
write.csv(s1_vs_s2, file.path(TBL_DIR, "s1_vs_s2_within_cell.csv"),
          row.names = FALSE, na = "")

# ============================================================================
# (c) Within-cell, within-scenario cor(u, nu)
# ============================================================================
cat("\n-- (c) within-cell cor(u, nu) per scenario --\n")

unu_rows <- list()
for (scn_name in c("s1", "s2")) {
  d <- if (scn_name == "s1") firms_s1 else firms_s2
  for (gran in c("4d", "2d")) {
    grp_col <- paste0("nace", gran)
    cells   <- unu_cells(d, grp_col)
    unu_rows[[length(unu_rows) + 1L]] <- tibble(
      scenario         = scn_name,
      granularity      = gran,
      n_cells          = nrow(cells),
      n_firms_mean     = round(mean(cells$n_firms, na.rm = TRUE)),
      mean_spearman    = mean(cells$spearman,    na.rm = TRUE),
      mean_pearson_log = mean(cells$pearson_log, na.rm = TRUE))
  }
}
u_nu_corr <- bind_rows(unu_rows) %>%
  mutate(across(c(mean_spearman, mean_pearson_log), ~round(.x, 3)))
print(u_nu_corr, row.names = FALSE)
write.csv(u_nu_corr, file.path(TBL_DIR, "u_nu_corr_within_cell.csv"),
          row.names = FALSE, na = "")

cat(sprintf("\nSaved 3 CSVs to %s/tables/\n", output_dir))
cat("Done.\n")
