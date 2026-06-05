###############################################################################
# analysis/phase4_direct_vs_indirect.R
#
# PURPOSE
#   Two further stats for section 3.4.2:
#
#   (a) Direct share of firm-year network-adjusted emission LEVELS:
#         share_i = scope1_i / (scope1_i + u_i * cost_i) = e^cost_i / nu_i
#       Distribution (mean, median, quartiles) stratified by ETS vs non-ETS,
#       per scenario. Answers "for a given firm-year, do direct or indirect
#       network-adjusted emissions dominate, and does the answer differ
#       between ETS-regulated and non-ETS-regulated firms?"
#
#   (b) Within-(sector, year) cor(e^cost, u): Spearman and Pearson-on-logs.
#       Answers "do firms with high direct intensity also have high
#       upstream-only intensity, or does the network add independent
#       information?"
#
# INPUTS  data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUTS output_{rmd,local}/tables/
#           - direct_share_summary.csv
#           - cor_e_u_within_cell.csv
#
# RUNS ON: RMD for real numbers; locally on downsampled build output for
# code validation only.
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

# ---- Paths ----
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

cat("== phase4_direct_vs_indirect ==\n")

MIN_N_FOR_CORR <- 10L

load_build <- function(scn) {
  d <- file.path(out_data, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  if (length(fs) == 0L) stop("No build files in ", d)
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L) %>%
    select(vat, year, nace4d, nace2d, source, total_cost, scope1, e_cost, nu, u,
           u_same_4d, u_other_4d, u_same_2d, u_other_2d)
}

# ============================================================================
# (a) Direct share of firm-year network-adjusted emission LEVELS
# ============================================================================
cat("\n-- (a) direct share = e^cost / nu, by scenario and ETS status --\n")

dshare_summary <- list()
for (scn_name in c("s1", "s2")) {
  firms <- load_build(scn_name)
  # Direct share for firm-year. nu must be > 0 to define the share; if nu == 0
  # the firm has no embodied emissions at all, so they drop out.
  firms <- firms %>%
    filter(is.finite(nu), nu > 0) %>%
    mutate(direct_share = e_cost / nu,
           ets_group    = ifelse(source == "ets", "ETS-regulated", "non-ETS"))
  # Distribution per (scenario, ETS-group)
  for (g in c("ETS-regulated", "non-ETS", "all")) {
    sub <- if (g == "all") firms else firms %>% filter(ets_group == g)
    if (nrow(sub) == 0L) next
    dshare_summary[[length(dshare_summary) + 1L]] <- tibble(
      scenario     = scn_name,
      group        = g,
      n_firmyears  = nrow(sub),
      mean_share   = mean(sub$direct_share,   na.rm = TRUE),
      median_share = median(sub$direct_share, na.rm = TRUE),
      p25_share    = quantile(sub$direct_share, 0.25, na.rm = TRUE),
      p75_share    = quantile(sub$direct_share, 0.75, na.rm = TRUE),
      share_above_half = mean(sub$direct_share > 0.5, na.rm = TRUE),
      share_below_001  = mean(sub$direct_share < 0.01, na.rm = TRUE))
  }
}
dshare_summary <- bind_rows(dshare_summary) %>%
  mutate(across(c(mean_share, median_share, p25_share, p75_share,
                  share_above_half, share_below_001), ~round(.x, 3)))
print(dshare_summary, row.names = FALSE)
write.csv(dshare_summary, file.path(TBL_DIR, "direct_share_summary.csv"),
          row.names = FALSE, na = "")

# ============================================================================
# (b) Within-(sector, year) cor(e^cost, u)
# ============================================================================
cat("\n-- (b) within-(sector, year) cor(e^cost, u), Spearman & Pearson-on-logs --\n")

log_gap <- function(x) {
  if (length(x) < 2L) return(NA_real_)
  q <- quantile(x, c(0.1, 0.9), na.rm = TRUE)
  unname(log(q[2]) - log(q[1]))
}

# For each (sector, year) cell, compute the within-cell correlation between
# e^cost and the chosen `other` measure (Spearman + Pearson-on-logs), plus
# flatness diagnostics that flag when `other` is near-zero for most firms in
# the cell -- in which case a low correlation is mechanical (no signal in the
# variable) rather than evidence of independent organizational variation.
cor_eu_cells <- function(firms, group_col, other_col, min_n = MIN_N_FOR_CORR) {
  d <- firms %>%
    mutate(grp = .data[[group_col]], other = .data[[other_col]]) %>%
    filter(!is.na(grp), is.finite(e_cost), is.finite(other), e_cost > 0,
           is.finite(u), u > 0)
  d %>%
    group_by(grp, year) %>%
    summarise(
      n_firms          = n(),
      frac_other_pos   = mean(other > 0),
      mean_share_other = mean(other / u),     # u > 0 enforced above
      log_gap_u        = log_gap(u),
      log_gap_other    = log_gap(other[other > 0]),
      spearman    = {
        ok <- other > 0
        if (sum(ok) >= 2L)
          suppressWarnings(cor(e_cost[ok], other[ok], method = "spearman"))
        else NA_real_
      },
      pearson_log = {
        ok <- other > 0
        if (sum(ok) >= 2L)
          suppressWarnings(cor(log(e_cost[ok]), log(other[ok]), method = "pearson"))
        else NA_real_
      },
      .groups     = "drop") %>%
    filter(n_firms >= min_n) %>%
    rename(!!group_col := grp)
}

# Compare e^cost against three measures of the upstream component:
#   u            : full upstream (immediate-supplier-sector blind)
#   u_other_4d   : upstream from immediate suppliers in a different NACE 4-digit
#                  sector than the buyer (removes the cement-wholesaler-buys-from-
#                  cement-producer mechanical channel within 4-digit cells)
#   u_other_2d   : upstream from immediate suppliers in a different NACE 2-digit
#                  sector than the buyer (same removal at a coarser granularity)
# Granularity of the cell is matched to the granularity of the supplier-removal
# for u_other_*; for u we report both 4d and 2d cells.
cor_eu_rows <- list()
for (scn_name in c("s1", "s2")) {
  firms <- load_build(scn_name)
  for (spec in list(
      list(gran = "4d", other = "u"),
      list(gran = "2d", other = "u"),
      list(gran = "4d", other = "u_other_4d"),
      list(gran = "2d", other = "u_other_2d"))) {
    grp_col <- paste0("nace", spec$gran)
    cells   <- cor_eu_cells(firms, grp_col, spec$other)
    cor_eu_rows[[length(cor_eu_rows) + 1L]] <- tibble(
      scenario              = scn_name,
      granularity           = spec$gran,
      compared_to           = spec$other,
      n_cells               = nrow(cells),
      n_firms_mean          = round(mean(cells$n_firms, na.rm = TRUE)),
      mean_frac_other_pos   = mean(cells$frac_other_pos,   na.rm = TRUE),
      mean_share_other      = mean(cells$mean_share_other, na.rm = TRUE),
      mean_log_gap_u        = mean(cells$log_gap_u,        na.rm = TRUE),
      mean_log_gap_other    = mean(cells$log_gap_other,    na.rm = TRUE),
      mean_spearman         = mean(cells$spearman,         na.rm = TRUE),
      mean_pearson_log      = mean(cells$pearson_log,      na.rm = TRUE))
  }
}
cor_eu <- bind_rows(cor_eu_rows) %>%
  mutate(across(c(mean_frac_other_pos, mean_share_other, mean_log_gap_u,
                  mean_log_gap_other, mean_spearman, mean_pearson_log),
                ~round(.x, 3)))
print(cor_eu, row.names = FALSE)
write.csv(cor_eu, file.path(TBL_DIR, "cor_e_u_within_cell.csv"),
          row.names = FALSE, na = "")

cat(sprintf("\nSaved 2 CSVs to %s/tables/\n", output_dir))
cat("Done.\n")
