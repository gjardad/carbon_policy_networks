###############################################################################
# analysis/phase4_top_k_overlap.R
#
# PURPOSE
#   Complement to within-cell Spearman(e^cost, u) reported in
#   cor_e_u_within_cell.csv: how much do the top-K firms by direct intensity
#   overlap with the top-K firms by upstream-only intensity, within a cell?
#
#   For each (sector, year) cell with at least 2K firms with positive values
#   on both measures, compute |top-K(e^cost) intersect top-K(u)| / K. Report
#   K = 5 and K = 10; also report top-50% overlap, which uses the cell size.
#   Aggregate by unweighted mean across cells.
#
# INPUTS  data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUTS output_{rmd,local}/tables/top_k_overlap.csv
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
rm(.path_candidates, .p)

TBL_DIR <- file.path(output_dir, "tables")
dir.create(TBL_DIR, recursive = TRUE, showWarnings = FALSE)

cat("== phase4_top_k_overlap ==\n")

load_build <- function(scn) {
  d <- file.path(out_data, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  if (length(fs) == 0L) stop("No build files in ", d)
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L) %>% select(vat, year, nace4d, nace2d, e_cost, u, nu)
}

# Per-cell overlap stats. Computes overlap between (top-K by e^cost) and
# (top-K by `other`), where `other` is the column name to compare against
# (typically "u" for upstream-only or "nu" for total network-adjusted).
cell_overlap <- function(d, group_col, other_col) {
  d %>%
    mutate(grp = .data[[group_col]], other = .data[[other_col]]) %>%
    filter(!is.na(grp), is.finite(e_cost), is.finite(other),
           e_cost > 0, other > 0) %>%
    group_by(grp, year) %>%
    summarise(
      n_firms          = n(),
      overlap_top5     = if (n() >= 2L * 5L) {
        e_rank <- rank(-e_cost, ties.method = "average")
        o_rank <- rank(-other,  ties.method = "average")
        length(intersect(which(e_rank <= 5L), which(o_rank <= 5L))) / 5L
      } else NA_real_,
      overlap_top10    = if (n() >= 2L * 10L) {
        e_rank <- rank(-e_cost, ties.method = "average")
        o_rank <- rank(-other,  ties.method = "average")
        length(intersect(which(e_rank <= 10L), which(o_rank <= 10L))) / 10L
      } else NA_real_,
      overlap_top_half = {
        K <- max(1L, floor(n() / 2L))
        e_rank <- rank(-e_cost, ties.method = "average")
        o_rank <- rank(-other,  ties.method = "average")
        length(intersect(which(e_rank <= K), which(o_rank <= K))) / K
      },
      .groups = "drop") %>%
    rename(!!group_col := grp)
}

rows <- list()
for (scn_name in c("s1", "s2")) {
  firms <- load_build(scn_name)
  for (gran in c("4d", "2d")) {
    grp_col <- paste0("nace", gran)
    for (other in c("u", "nu")) {
      cells <- cell_overlap(firms, grp_col, other)
      rows[[length(rows) + 1L]] <- tibble(
        scenario              = scn_name,
        granularity           = gran,
        compared_to           = other,
        n_cells_top_half      = nrow(cells),
        n_cells_top10         = sum(!is.na(cells$overlap_top10)),
        n_cells_top5          = sum(!is.na(cells$overlap_top5)),
        n_firms_mean          = round(mean(cells$n_firms, na.rm = TRUE)),
        mean_top5_overlap     = mean(cells$overlap_top5,     na.rm = TRUE),
        mean_top10_overlap    = mean(cells$overlap_top10,    na.rm = TRUE),
        mean_top_half_overlap = mean(cells$overlap_top_half, na.rm = TRUE))
    }
  }
}
overlap <- bind_rows(rows) %>%
  mutate(across(c(mean_top5_overlap, mean_top10_overlap, mean_top_half_overlap),
                ~round(.x, 3)))
print(overlap, row.names = FALSE)
write.csv(overlap, file.path(TBL_DIR, "top_k_overlap.csv"),
          row.names = FALSE, na = "")

cat(sprintf("\nSaved %s/tables/top_k_overlap.csv\n", output_dir))
cat("Done.\n")
