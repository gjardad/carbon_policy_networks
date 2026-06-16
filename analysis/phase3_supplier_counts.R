###############################################################################
# analysis/phase3_supplier_counts.R
#
# PURPOSE
#   How many suppliers does a buyer have WITHIN a NACE 4-digit supplier sector?
#   For each (buyer, year, supplier-NACE4d) the count of distinct suppliers is
#   the number of same-sector alternatives a buyer could substitute among when
#   one of them is taxed. If most (buyer, sector) relationships have a single
#   supplier, the within-sector (sigma_W) reallocation margin has little room to
#   operate -- there is simply nobody in the same sector to substitute toward.
#   Distribution pooled over (buyer, year, supplier-sector).
#
#   B2B convention (phase3_build_upstream_emissions.R:142-151): vat_i_ano = SELLER
#   (supplier), vat_j_ano = BUYER, corr_sales_ij = i->j flow.
#
# INPUT  {nbb}/processed/b2b_selected_sample.RData
#        {nbb}/processed/deployment_panel.RData + firm_year_belgian_euets.RData  (vat,year,nace5d)
# OUTPUT {output_dir}/figures/suppliers_per_nace4d.png
#        {output_dir}/supplier_counts_summary.csv
# RUNS ON: local 1 (downsample) or RMD (full)
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
source(file.path(project_root, "utils", "sector_conventions.R"))   # make_nace4d
fig_dir <- file.path(output_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
rm(.path_candidates, .p)

cat("== supplier_counts ==\n")

# ---- vat -> NACE 4-digit lookup (per firm-year), union of non-ETS + ETS ----
load(file.path(proc_data, "deployment_panel.RData"))
load(file.path(proc_data, "firm_year_belgian_euets.RData"))
nace <- bind_rows(
    deployment_panel %>% transmute(vat, year, nace5d),
    firm_year_belgian_euets %>% transmute(vat, year, nace5d)
  ) %>%
  filter(!is.na(nace5d)) %>% distinct(vat, year, .keep_all = TRUE) %>%
  mutate(nace4d = make_nace4d(nace5d)) %>% select(vat, year, nace4d)
rm(deployment_panel, firm_year_belgian_euets)

# ---- B2B edges: distinct (buyer, supplier, year) ----
load(file.path(proc_data, "b2b_selected_sample.RData"))
edges <- df_b2b_selected_sample %>%
  filter(corr_sales_ij > 0) %>%
  transmute(buyer = vat_j_ano, supplier = vat_i_ano, year) %>%
  distinct()
rm(df_b2b_selected_sample)

# attach SUPPLIER's NACE 4-digit
edges <- edges %>%
  left_join(nace, by = c("supplier" = "vat", "year" = "year")) %>%
  filter(!is.na(nace4d))
cat(sprintf("  %d buyer-supplier-year edges with a supplier NACE4d over %s\n",
            nrow(edges), paste(range(edges$year), collapse = "-")))

# ---- count distinct suppliers per (buyer, year, supplier-sector) ----
counts <- edges %>%
  group_by(buyer, year, nace4d) %>%
  summarise(n_suppliers = n_distinct(supplier), .groups = "drop")
cat(sprintf("  %d (buyer, year, supplier-sector) relationships\n", nrow(counts)))

ns <- counts$n_suppliers
qn <- quantile(ns, c(.25,.50,.75,.90,.95,.99), na.rm = TRUE)
cat("  suppliers per (buyer, supplier-NACE4d):\n"); print(round(qn, 1))
cat(sprintf("  share with exactly 1 supplier: %.1f%% ; with <=2: %.1f%% ; with >=5: %.1f%%\n",
            100*mean(ns == 1), 100*mean(ns <= 2), 100*mean(ns >= 5)))

# ---- binned barplot (fraction of relationships by # same-sector suppliers) ----
brk  <- c(0, 1, 2, 3, 4, 5, 10, 25, Inf)
lab  <- c("1", "2", "3", "4", "5", "6-10", "11-25", "26+")
frac <- 100 * prop.table(table(cut(ns, breaks = brk, labels = lab, right = TRUE)))

png(file.path(fig_dir, "suppliers_per_nace4d.png"), width = 6.5, height = 4.2, units = "in", res = 150)
op <- par(mar = c(4.4, 4.4, 1, 1))
bp <- barplot(as.numeric(frac), names.arg = lab, col = "grey35", border = "white",
              xlab = "Distinct suppliers a buyer has within a NACE 4-digit sector",
              ylab = "% of (buyer, supplier-sector) relationships", ylim = c(0, max(frac) * 1.12))
text(bp, as.numeric(frac), sprintf("%.0f%%", as.numeric(frac)), pos = 3, cex = 0.8, xpd = NA)
par(op); dev.off()

write.csv(data.frame(
  years = paste(range(edges$year), collapse = "-"),
  n_relationships = nrow(counts), n_edges = nrow(edges),
  p50 = qn["50%"], p90 = qn["90%"], p99 = qn["99%"],
  pct_exactly_1 = 100*mean(ns == 1), pct_le_2 = 100*mean(ns <= 2), pct_ge_5 = 100*mean(ns >= 5)),
  file.path(output_dir, "supplier_counts_summary.csv"), row.names = FALSE)

cat("Done. Figure + summary in", output_dir, "\n")
