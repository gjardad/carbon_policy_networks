###############################################################################
# analysis/phase3_table_dispersion_comparison.R
#
# PURPOSE
#   Compare within-sector REVENUE-BASED direct emission intensity dispersion
#   (90-10 log gap) between:
#     - ETS-only (S1): only ETS firms emit (non-ETS = 0)
#     - All firms (S2): observed ETS + GLO-imputed non-ETS
#   for the "important manufacturing emitters" = the TOP-10 ETS-covered
#   NACE-4d sectors by observed ETS emission VOLUME (a sector is ETS-covered if
#   >=1 firm in it is ETS; ranked by total observed ETS emissions so the set is
#   fixed across scenarios), plus the 2-digit aggregate and literature rows.
#
# INPUT  {PROJ}/data/processed/emissions_vectors_{s1,s2}.RData
# OUTPUT {PROJ}/output/tables/dispersion_comparison_top10_4d.csv
#        {PROJ}/output/tables/dispersion_comparison_2d.csv
#
# RUNS ON: local 1
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

if (Sys.info()[["user"]] == "JARDANG") {
  project_root <- "X:/Documents/JARDANG/carbon_policy_networks"
} else {
  project_root <- "c:/Users/jota_/Documents/carbon_policy_networks"
}
proc    <- file.path(project_root, "data", "processed")
out_dir <- file.path(project_root, "output", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(project_root, "utils", "sector_conventions.R"))

TOP_N <- 10L

load(file.path(proc, "emissions_vectors_s1.RData"))  # ev_s1 (ETS observed)
load(file.path(proc, "emissions_vectors_s2.RData"))  # ev_s2 (all firms)

# ---- Per-(sector,year) dispersion, averaged across years ----
sector_disp <- function(ev, group_col, keep = NULL) {
  d <- ev %>%
    mutate(grp = .data[[group_col]], ei = z / revenue) %>%
    filter(!is.na(grp), is.finite(ei), ei > 0)
  if (!is.null(keep)) d <- d %>% filter(grp %in% keep)
  d %>%
    group_by(grp, year) %>%
    group_modify(~ disp_stats(.x$ei)) %>%
    ungroup() %>%
    filter(n_firms >= MIN_N_STATS) %>%
    group_by(grp) %>%
    summarise(n_years = n(),
              n_avg   = round(mean(n_firms)),
              log9010 = mean(p9010_log, na.rm = TRUE),
              .groups = "drop") %>%
    rename(!!group_col := grp)
}

# ---- Rank ETS-covered NACE-4d sectors by observed ETS emission volume ----
vol <- ev_s2 %>%
  filter(source == "ets") %>%
  group_by(nace4d) %>%
  summarise(ets_emissions = sum(z), n_ets_firmyears = n(), .groups = "drop") %>%
  arrange(desc(ets_emissions))
top10 <- head(vol, TOP_N)
cat("== Top-10 ETS-covered NACE-4d sectors by observed ETS emission volume ==\n")
print(as.data.frame(top10 %>% mutate(ets_Mt = round(ets_emissions / 1e6, 2))))

sectors4d <- top10$nace4d

# ---- 4-digit table: ETS-only (S1) vs all-firms (S2) for the top-10 ----
ets4d  <- sector_disp(ev_s1, "nace4d", keep = sectors4d) %>%
  rename(ets_n = n_avg, ets_yrs = n_years, ets_log9010 = log9010)
full4d <- sector_disp(ev_s2, "nace4d", keep = sectors4d) %>%
  rename(full_n = n_avg, full_yrs = n_years, full_log9010 = log9010)

tab4d <- top10 %>%
  select(nace4d, ets_Mt = ets_emissions) %>%
  mutate(ets_Mt = round(ets_Mt / 1e6, 2)) %>%
  left_join(ets4d,  by = "nace4d") %>%
  left_join(full4d, by = "nace4d") %>%
  mutate(ratio_full_ets = full_log9010 / ets_log9010) %>%
  arrange(desc(ets_Mt)) %>%
  select(nace4d, ets_Mt, ets_n, full_n, ets_log9010, full_log9010, ratio_full_ets)

mean_row4d <- tibble(nace4d = "MEAN (top-10)", ets_Mt = NA_real_,
                     ets_n = NA_real_, full_n = NA_real_,
                     ets_log9010  = mean(tab4d$ets_log9010,  na.rm = TRUE),
                     full_log9010 = mean(tab4d$full_log9010, na.rm = TRUE),
                     ratio_full_ets = NA_real_)
lit4d <- tibble(nace4d = "Lyubich et al. (2018), 6-digit NAICS",
                ets_Mt = NA_real_, ets_n = NA_real_, full_n = NA_real_,
                ets_log9010 = 2.27, full_log9010 = NA_real_, ratio_full_ets = NA_real_)
tab4d_out <- bind_rows(
  tab4d %>% mutate(across(c(ets_log9010, full_log9010, ratio_full_ets), ~round(.x, 2))),
  mean_row4d %>% mutate(across(c(ets_log9010, full_log9010), ~round(.x, 2))),
  lit4d)

cat("\n== Panel A: NACE 4-digit, top-10 emitter sectors (90-10 log gap of e^rev) ==\n")
print(as.data.frame(tab4d_out), row.names = FALSE, na.print = "---")
write.csv(tab4d_out, file.path(out_dir, "dispersion_comparison_top10_4d.csv"),
          row.names = FALSE, na = "")

# ---- 2-digit aggregate: 2d parents of the top-10 sectors ----
sectors2d <- ev_s2 %>% filter(nace4d %in% sectors4d) %>% pull(nace2d) %>% unique()
ets2d  <- sector_disp(ev_s1, "nace2d", keep = sectors2d) %>%
  rename(ets_n = n_avg, ets_log9010 = log9010) %>% select(nace2d, ets_n, ets_log9010)
full2d <- sector_disp(ev_s2, "nace2d", keep = sectors2d) %>%
  rename(full_n = n_avg, full_log9010 = log9010) %>% select(nace2d, full_n, full_log9010)
tab2d <- tibble(nace2d = sectors2d) %>%
  left_join(ets2d, by = "nace2d") %>%
  left_join(full2d, by = "nace2d") %>%
  arrange(nace2d) %>%
  mutate(across(c(ets_log9010, full_log9010), ~round(.x, 2)))
mean_row2d <- tibble(nace2d = "MEAN",
                     ets_n = NA_real_, ets_log9010 = round(mean(tab2d$ets_log9010, na.rm = TRUE), 2),
                     full_n = NA_real_, full_log9010 = round(mean(tab2d$full_log9010, na.rm = TRUE), 2))
lit2d <- tibble(nace2d = "De Lyon & Dechezlepretre (2025), 2-digit",
                ets_n = NA_real_, ets_log9010 = 3.18, full_n = NA_real_, full_log9010 = NA_real_)
tab2d_out <- bind_rows(tab2d, mean_row2d, lit2d)

cat("\n== Panel B: NACE 2-digit (parents of top-10), 90-10 log gap of e^rev ==\n")
print(as.data.frame(tab2d_out), row.names = FALSE, na.print = "---")
write.csv(tab2d_out, file.path(out_dir, "dispersion_comparison_2d.csv"),
          row.names = FALSE, na = "")

cat(sprintf("\nSaved tables to %s\n", out_dir))
cat("Done.\n")
