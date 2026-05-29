###############################################################################
# analysis/phase3_dispersion_direct.R
#
# PURPOSE
#   Within-sector dispersion of REVENUE-BASED direct emission intensity
#   e^rev_i = emissions_i / revenue_i, at NACE 4-digit (headline) and 2-digit
#   (literature comparability), under both scenarios:
#     S1 (zero): ETS observed only (non-ETS = 0)
#     S2 (GLO):  observed ETS + GLO-imputed non-ETS
#
#   Per (sector, year): 90-10 log gap, 90/10 ratio, var(log), Gini
#   (see utils/sector_conventions.R :: disp_stats). Cells with < MIN_N_STATS
#   firms are dropped.
#
#   NOTE: The cost-based direct EI (e^cost) and the bridge correlation
#   cor(log e^cost, log e^rev) are added in Phase B once total_cost exists
#   (phase3_build_upstream_emissions.R provides it).
#
# INPUT  {PROJ}/data/processed/emissions_vectors_{s1,s2}.RData
# OUTPUT {PROJ}/data/processed/dispersion_direct_rev_{s1,s2}.RData
#          (stats4d, stats2d per scenario)
#
# RUNS ON: local 1
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
proc <- out_data  # local alias used in this script's body
rm(.path_candidates, .p)

cat("== phase3_dispersion_direct (revenue-based) ==\n")

compute_disp <- function(ev, group_col) {
  ev %>%
    mutate(grp = .data[[group_col]], ei = z / revenue) %>%
    filter(!is.na(grp), is.finite(ei), ei > 0) %>%
    group_by(grp, year) %>%
    group_modify(~ disp_stats(.x$ei)) %>%
    ungroup() %>%
    filter(n_firms >= MIN_N_STATS) %>%
    rename(!!group_col := grp)
}

results <- list()
for (scn in c("s1", "s2")) {
  load(file.path(proc, sprintf("emissions_vectors_%s.RData", scn)))
  ev <- get(sprintf("ev_%s", scn))

  stats4d <- compute_disp(ev, "nace4d")
  stats2d <- compute_disp(ev, "nace2d")

  save(stats4d, stats2d,
       file = file.path(proc, sprintf("dispersion_direct_rev_%s.RData", scn)))

  # Report the ratio as exp(mean log gap) to match how Lyubich cites 9.7x
  # (= exp(2.27)); the arithmetic mean of per-cell ratios is Jensen-inflated.
  m4 <- mean(stats4d$p9010_log, na.rm = TRUE)
  m2 <- mean(stats2d$p9010_log, na.rm = TRUE)
  cat(sprintf("\n[%s] revenue-based direct EI dispersion (mean across sector-years)\n",
              toupper(scn)))
  cat(sprintf("   4d: %d sector-years | 90-10 log gap = %.2f (ratio %.1fx) | median gap = %.2f\n",
              nrow(stats4d), m4, exp(m4), median(stats4d$p9010_log, na.rm = TRUE)))
  cat(sprintf("   2d: %d sector-years | 90-10 log gap = %.2f (ratio %.1fx) | median gap = %.2f\n",
              nrow(stats2d), m2, exp(m2), median(stats2d$p9010_log, na.rm = TRUE)))
  results[[scn]] <- list(m4 = m4, m2 = m2)
}

cat("\n-- Literature reference: Lyubich et al. 2.27 log / 9.7x (6-digit NAICS ~ 4d);",
    "De Lyon & Dechezlepretre ~20x (2-digit) --\n")
cat("Done.\n")
