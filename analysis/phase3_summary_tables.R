###############################################################################
# analysis/phase3_summary_tables.R
#
# PURPOSE
#   Aggregate the per-cell Phase B .RData outputs (gitignored) into a small
#   set of committable CSV summary tables for the paper. After running the
#   Phase B build + stat scripts, run this last to produce the trackable
#   artifacts.
#
# INPUTS  data/processed/dispersion_direct_rev_{s1,s2}.RData         (Phase A)
#         data/processed/dispersion_network_{s1,s2}.RData            (Phase B)
#         data/processed/variance_decomp_{s1,s2}.RData                (Phase B)
#         data/processed/correlations_{s1,s2}.RData                   (Phase B)
#         data/processed/dispersion_direct_cost_bridge_{s1,s2}.RData (Phase B)
#         data/processed/emissions_vectors_s2.RData                   (Phase A)
#         data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData  (Phase B)
#
# OUTPUTS output/tables/dispersion_summary.csv
#         output/tables/dispersion_top10_4d_nu.csv
#         output/tables/variance_decomp_summary.csv
#         output/tables/correlations_summary.csv
#         output/tables/bridge_correlations.csv
#
# RUNS ON: local 1 or RMD
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
proc    <- out_data
out_dir <- file.path(project_root, "output", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
rm(.path_candidates, .p)

cat("== phase3_summary_tables ==\n")

TOP_N <- 10L

# Helpers ---------------------------------------------------------------------

summarise_disp <- function(stats_df) {
  # Common dispersion summary across a stats_df with per-cell `p9010_log`,
  # `p90p10`, `var_log`, `gini`, `n_firms`. Returns a one-row data.frame.
  data.frame(
    n_cells       = nrow(stats_df),
    n_firms_mean  = round(mean(stats_df$n_firms, na.rm = TRUE)),
    mean_log_gap  = mean(stats_df$p9010_log, na.rm = TRUE),
    ratio_x       = exp(mean(stats_df$p9010_log, na.rm = TRUE)),
    median_log_gap= median(stats_df$p9010_log, na.rm = TRUE),
    mean_gini     = mean(stats_df$gini, na.rm = TRUE),
    mean_var_log  = mean(stats_df$var_log, na.rm = TRUE))
}

load_scn <- function(file_stub, scn) {
  f <- file.path(proc, sprintf("%s_%s.RData", file_stub, scn))
  if (!file.exists(f)) {
    warning("missing input: ", f); return(NULL)
  }
  e <- new.env(); load(f, envir = e); e
}

# =============================================================================
# (1) Dispersion summary: scenario x granularity x measure ∈ {e_rev, e_cost, nu}
# =============================================================================
cat("-- (1) dispersion summary --\n")

rows <- list()
for (scn in c("s1", "s2")) {
  for (gran in c("4d", "2d")) {

    # e_rev (Phase A, revenue-based direct)
    e <- load_scn("dispersion_direct_rev", scn)
    if (!is.null(e)) {
      tbl <- if (gran == "4d") e$stats4d else e$stats2d
      r   <- summarise_disp(tbl)
      r$scenario <- scn; r$granularity <- gran; r$measure <- "e_rev"
      rows[[length(rows) + 1L]] <- r
    }

    # e_cost (cost-based direct, from cost-bridge output)
    e <- load_scn("dispersion_direct_cost_bridge", scn)
    if (!is.null(e)) {
      tbl <- if (gran == "4d") e$ecost_stats4d else e$ecost_stats2d
      r   <- summarise_disp(tbl)
      r$scenario <- scn; r$granularity <- gran; r$measure <- "e_cost"
      rows[[length(rows) + 1L]] <- r
    }

    # nu (cost-based network-adjusted)
    e <- load_scn("dispersion_network", scn)
    if (!is.null(e)) {
      tbl <- if (gran == "4d") e$nu_stats4d else e$nu_stats2d
      r   <- summarise_disp(tbl)
      r$scenario <- scn; r$granularity <- gran; r$measure <- "nu"
      rows[[length(rows) + 1L]] <- r
    }
  }
}
dispersion_summary <- bind_rows(rows) %>%
  select(scenario, granularity, measure, n_cells, n_firms_mean,
         mean_log_gap, ratio_x, median_log_gap, mean_gini, mean_var_log) %>%
  mutate(across(c(mean_log_gap, ratio_x, median_log_gap, mean_gini, mean_var_log),
                ~round(.x, 3)))
print(dispersion_summary, row.names = FALSE)
write.csv(dispersion_summary,
          file.path(out_dir, "dispersion_summary.csv"),
          row.names = FALSE, na = "")

# =============================================================================
# (2) Top-10 4d for nu (analogue of dispersion_comparison_top10_4d.csv but for
#     the network-adjusted measure)
# =============================================================================
cat("\n-- (2) top-10 4d for nu --\n")

ranking_path <- file.path(proc, "emissions_vectors_s2.RData")
if (!file.exists(ranking_path)) {
  warning("missing emissions_vectors_s2.RData; skipping top-10 nu table.")
} else {
  load(ranking_path)
  vol <- ev_s2 %>%
    filter(source == "ets") %>%
    group_by(nace4d) %>%
    summarise(ets_emissions = sum(z), .groups = "drop") %>%
    arrange(desc(ets_emissions))
  top10 <- head(vol, TOP_N)
  sectors4d <- top10$nace4d

  e_s1 <- load_scn("dispersion_network", "s1")
  e_s2 <- load_scn("dispersion_network", "s2")
  if (is.null(e_s1) || is.null(e_s2)) {
    warning("missing dispersion_network_{s1,s2}.RData; skipping top-10 nu table.")
  } else {
    nu_per_sector <- function(stats4d, label) {
      stats4d %>%
        filter(nace4d %in% sectors4d) %>%
        group_by(nace4d) %>%
        summarise(n_years = n(),
                  n_avg   = round(mean(n_firms)),
                  log9010 = mean(p9010_log, na.rm = TRUE),
                  .groups = "drop") %>%
        rename(!!paste0(label, "_n")        := n_avg,
               !!paste0(label, "_yrs")      := n_years,
               !!paste0(label, "_log9010")  := log9010)
    }
    ets4d  <- nu_per_sector(e_s1$nu_stats4d, "ets")
    full4d <- nu_per_sector(e_s2$nu_stats4d, "full")

    tab4d <- top10 %>%
      mutate(ets_Mt = round(ets_emissions / 1e6, 2)) %>%
      select(nace4d, ets_Mt) %>%
      left_join(ets4d,  by = "nace4d") %>%
      left_join(full4d, by = "nace4d") %>%
      mutate(ratio_full_ets = full_log9010 / ets_log9010) %>%
      arrange(desc(ets_Mt)) %>%
      select(nace4d, ets_Mt, ets_n, full_n,
             ets_log9010, full_log9010, ratio_full_ets) %>%
      mutate(across(c(ets_log9010, full_log9010, ratio_full_ets), ~round(.x, 2)))
    mean_row <- tibble(nace4d = "MEAN (top-10)", ets_Mt = NA_real_,
                       ets_n = NA_real_, full_n = NA_real_,
                       ets_log9010  = round(mean(tab4d$ets_log9010,  na.rm = TRUE), 2),
                       full_log9010 = round(mean(tab4d$full_log9010, na.rm = TRUE), 2),
                       ratio_full_ets = NA_real_)
    top10_nu <- bind_rows(tab4d, mean_row)
    print(top10_nu, row.names = FALSE)
    write.csv(top10_nu,
              file.path(out_dir, "dispersion_top10_4d_nu.csv"),
              row.names = FALSE, na = "")
  }
}

# =============================================================================
# (3) Variance decomposition summary (one row per (scenario, cell_g, supp_g, min_n))
# =============================================================================
cat("\n-- (3) variance decomposition summary --\n")

vd_rows <- list()
for (scn in c("s1", "s2")) {
  e <- load_scn("variance_decomp", scn)
  if (is.null(e) || is.null(e$sanity_tbl)) next
  specs <- c("4d cells, 2d suppliers" = "vd_4d_2dgroup",
             "4d cells, 4d suppliers" = "vd_4d_4dgroup",
             "2d cells, 2d suppliers" = "vd_2d_2dgroup",
             "2d cells, 4d suppliers" = "vd_2d_4dgroup")
  for (lbl in names(specs)) {
    sanity <- e$sanity_tbl[[lbl]]
    if (is.null(sanity)) next
    cg <- if (grepl("^4d cells", lbl)) "4d" else "2d"
    sg <- if (grepl("2d suppliers", lbl)) "2d" else "4d"
    # also compute pooled shares from the full per-cell table
    vd_full <- get(specs[[lbl]], envir = e)
    pooled <- vd_full %>% summarise(
      pool_across = sum(var_across, na.rm = TRUE) / sum(var_u, na.rm = TRUE),
      pool_within = sum(var_within, na.rm = TRUE) / sum(var_u, na.rm = TRUE),
      pool_cov    = sum(cov2_aw,    na.rm = TRUE) / sum(var_u, na.rm = TRUE))
    sanity$scenario     <- scn
    sanity$cell_g       <- cg
    sanity$supplier_g   <- sg
    sanity$pool_across  <- pooled$pool_across
    sanity$pool_within  <- pooled$pool_within
    sanity$pool_cov     <- pooled$pool_cov
    vd_rows[[length(vd_rows) + 1L]] <- sanity
  }
}
variance_decomp_summary <- bind_rows(vd_rows) %>%
  select(scenario, cell_g, supplier_g, min_n, n_cells, frac_degen,
         med_across_int, med_within_int, med_cov_int,
         pool_across, pool_within, pool_cov) %>%
  mutate(across(c(frac_degen, med_across_int, med_within_int, med_cov_int,
                  pool_across, pool_within, pool_cov), ~round(.x, 3)))
print(variance_decomp_summary, row.names = FALSE)
write.csv(variance_decomp_summary,
          file.path(out_dir, "variance_decomp_summary.csv"),
          row.names = FALSE, na = "")

# =============================================================================
# (4) Correlations summary: within-sector cor(e^cost, nu) intensity and
#     cor(z, nu*cost) levels, both scenarios, both granularities.
# =============================================================================
cat("\n-- (4) correlations summary --\n")

corr_rows <- list()
for (scn in c("s1", "s2")) {
  e <- load_scn("correlations", scn)
  if (is.null(e)) next
  specs <- list(
    intensity_4d = list(obj = "corr_intensity_4d", granularity = "4d", measure_type = "intensity"),
    intensity_2d = list(obj = "corr_intensity_2d", granularity = "2d", measure_type = "intensity"),
    levels_4d    = list(obj = "corr_levels_4d",    granularity = "4d", measure_type = "levels"),
    levels_2d    = list(obj = "corr_levels_2d",    granularity = "2d", measure_type = "levels"))
  for (k in names(specs)) {
    s <- specs[[k]]
    df <- get(s$obj, envir = e)
    corr_rows[[length(corr_rows) + 1L]] <- data.frame(
      scenario     = scn,
      granularity  = s$granularity,
      measure_type = s$measure_type,
      n_cells      = nrow(df),
      mean_spearman    = mean(df$spearman,    na.rm = TRUE),
      mean_pearson_log = mean(df$pearson_log, na.rm = TRUE))
  }
}
correlations_summary <- bind_rows(corr_rows) %>%
  mutate(across(c(mean_spearman, mean_pearson_log), ~round(.x, 3)))
print(correlations_summary, row.names = FALSE)
write.csv(correlations_summary,
          file.path(out_dir, "correlations_summary.csv"),
          row.names = FALSE, na = "")

# =============================================================================
# (5) Bridge correlations cor(log e^cost, log e^rev): pooled + within-sector
# =============================================================================
cat("\n-- (5) bridge correlations --\n")

bridge_rows <- list()
for (scn in c("s1", "s2")) {
  e <- load_scn("dispersion_direct_cost_bridge", scn)
  if (is.null(e)) next
  bp <- e$bridge_pooled
  bridge_rows[[length(bridge_rows) + 1L]] <- data.frame(
    scenario = scn, granularity = "pooled",
    n_obs_or_cells = bp$n_firms,
    spearman = bp$spearman, pearson = bp$pearson)
  for (gran in c("4d", "2d")) {
    df <- get(paste0("bridge_", gran), envir = e)
    bridge_rows[[length(bridge_rows) + 1L]] <- data.frame(
      scenario = scn, granularity = gran,
      n_obs_or_cells = nrow(df),
      spearman = mean(df$spearman, na.rm = TRUE),
      pearson  = mean(df$pearson,  na.rm = TRUE))
  }
}
bridge_correlations <- bind_rows(bridge_rows) %>%
  mutate(across(c(spearman, pearson), ~round(.x, 3)))
print(bridge_correlations, row.names = FALSE)
write.csv(bridge_correlations,
          file.path(out_dir, "bridge_correlations.csv"),
          row.names = FALSE, na = "")

cat(sprintf("\nSaved 5 CSVs to %s\n", out_dir))
cat("Done.\n")
