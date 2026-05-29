###############################################################################
# analysis/phase3_correlations.R
#
# PURPOSE
#   Within-sector firm-level correlations between the direct and the network-
#   adjusted emission measures, both as INTENSITIES (per EUR of cost) and as
#   LEVELS (tonnes CO2):
#
#     intensity:  cor(e^cost_i, nu_i)        within (sector, year)
#     levels:     cor(z_i, nu_i * total_cost_i) within (sector, year)
#
#   where z_i = scope1_i (direct emissions, tonnes) and nu_i * total_cost_i is
#   total embodied emissions (direct + upstream), tonnes. Spearman + Pearson-
#   on-logs. Computed for both scenarios (S1 zero / S2 GLO), at NACE 4d + 2d.
#
#   Cells with < MIN_N_CORR (10) firms with positive values are dropped.
#
# INPUT  {PROJ}/data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUT {PROJ}/data/processed/correlations_{s1,s2}.RData
#          (corr_intensity_4d, corr_intensity_2d,
#           corr_levels_4d,    corr_levels_2d per scenario)
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

cat("== phase3_correlations ==\n")

load_build <- function(scn) {
  d <- file.path(proc, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L)
}

cor_cells <- function(firms, group_col, x_col, y_col, log_pearson = TRUE) {
  d <- firms %>%
    mutate(grp = .data[[group_col]],
           x   = .data[[x_col]],
           y   = .data[[y_col]]) %>%
    filter(!is.na(grp), is.finite(x), is.finite(y), x > 0, y > 0)
  d %>%
    group_by(grp, year) %>%
    summarise(
      n_firms     = n(),
      spearman    = if (n() >= 2L) suppressWarnings(cor(x, y, method = "spearman")) else NA_real_,
      pearson_log = if (log_pearson && n() >= 2L)
                      suppressWarnings(cor(log(x), log(y), method = "pearson")) else NA_real_,
      .groups     = "drop") %>%
    filter(n_firms >= MIN_N_CORR) %>%
    rename(!!group_col := grp)
}

summ <- function(c) c(
  n_cells    = nrow(c),
  spearman   = mean(c$spearman,    na.rm = TRUE),
  pearson_lg = mean(c$pearson_log, na.rm = TRUE))

for (scn in c("s1", "s2")) {
  firms <- load_build(scn)
  firms <- firms %>% mutate(emb_level = nu * total_cost)
  cat(sprintf("\n[%s] firms loaded: %d\n", toupper(scn), nrow(firms)))

  corr_intensity_4d <- cor_cells(firms, "nace4d", "e_cost", "nu")
  corr_intensity_2d <- cor_cells(firms, "nace2d", "e_cost", "nu")
  corr_levels_4d    <- cor_cells(firms, "nace4d", "scope1", "emb_level")
  corr_levels_2d    <- cor_cells(firms, "nace2d", "scope1", "emb_level")

  save(corr_intensity_4d, corr_intensity_2d, corr_levels_4d, corr_levels_2d,
       file = file.path(proc, sprintf("correlations_%s.RData", scn)))

  for (nm in c("corr_intensity_4d", "corr_intensity_2d",
               "corr_levels_4d",    "corr_levels_2d")) {
    s <- summ(get(nm))
    cat(sprintf("   %s: %4d cells | mean Spearman = %+.3f | mean Pearson(log) = %+.3f\n",
                nm, s["n_cells"], s["spearman"], s["pearson_lg"]))
  }
}

cat("\nDone.\n")
