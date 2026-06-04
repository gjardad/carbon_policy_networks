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
    select(vat, year, nace4d, nace2d, source, total_cost, scope1, e_cost, nu, u)
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

cor_eu_cells <- function(firms, group_col, min_n = MIN_N_FOR_CORR) {
  firms %>%
    mutate(grp = .data[[group_col]]) %>%
    filter(!is.na(grp), is.finite(e_cost), is.finite(u), e_cost > 0, u > 0) %>%
    group_by(grp, year) %>%
    summarise(
      n_firms     = n(),
      spearman    = if (n() >= 2L) suppressWarnings(cor(e_cost, u, method = "spearman")) else NA_real_,
      pearson_log = if (n() >= 2L) suppressWarnings(cor(log(e_cost), log(u), method = "pearson")) else NA_real_,
      .groups     = "drop") %>%
    filter(n_firms >= min_n) %>%
    rename(!!group_col := grp)
}

cor_eu_rows <- list()
for (scn_name in c("s1", "s2")) {
  firms <- load_build(scn_name)
  for (gran in c("4d", "2d")) {
    grp_col <- paste0("nace", gran)
    cells   <- cor_eu_cells(firms, grp_col)
    cor_eu_rows[[length(cor_eu_rows) + 1L]] <- tibble(
      scenario         = scn_name,
      granularity      = gran,
      n_cells          = nrow(cells),
      n_firms_mean     = round(mean(cells$n_firms, na.rm = TRUE)),
      mean_spearman    = mean(cells$spearman,    na.rm = TRUE),
      mean_pearson_log = mean(cells$pearson_log, na.rm = TRUE))
  }
}
cor_eu <- bind_rows(cor_eu_rows) %>%
  mutate(across(c(mean_spearman, mean_pearson_log), ~round(.x, 3)))
print(cor_eu, row.names = FALSE)
write.csv(cor_eu, file.path(TBL_DIR, "cor_e_u_within_cell.csv"),
          row.names = FALSE, na = "")

cat(sprintf("\nSaved 2 CSVs to %s/tables/\n", output_dir))
cat("Done.\n")
