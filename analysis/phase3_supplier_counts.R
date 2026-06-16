###############################################################################
# analysis/phase3_supplier_counts.R
#
# PURPOSE
#   Characterise a buyer's sourcing structure -- the substitution base for the
#   two-nest production function. For each buyer (and year) we compute:
#     (1) AVG SUPPLIERS PER SECTOR: the average number of distinct suppliers the
#         buyer has within a NACE 4-digit sector, averaged over the sectors it
#         buys from = (total distinct suppliers) / (number of supplier-sectors).
#         This is the within-sector (sigma_W) substitution base.
#     (2) SECTORS SOURCED: the number of distinct NACE 4-digit supplier sectors
#         the buyer buys from. This is the across-sector (sigma_B) base.
#
#   B2B convention (phase3_build_upstream_emissions.R): vat_i_ano = SELLER
#   (supplier), vat_j_ano = BUYER, corr_sales_ij = i->j flow.
#
# INPUT  {nbb}/processed/b2b_selected_sample.RData
#        {nbb}/processed/deployment_panel.RData + firm_year_belgian_euets.RData (vat,year,nace5d)
# OUTPUT {output_dir}/tables/supplier_structure.tex   (booktabs table for the paper)
#        {output_dir}/supplier_counts_summary.csv
# RUNS ON: local 1 (downsample; understates supplier counts) or RMD (full)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages({ library(dplyr) })

.path_candidates <- c(
  "C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
  "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
.p <- .path_candidates[file.exists(.path_candidates)]
if (length(.p) == 0L) stop("Cannot locate utils/paths.R; add a candidate.")
source(.p[1])
source(file.path(project_root, "utils", "sector_conventions.R"))   # make_nace4d
tab_dir <- file.path(output_dir, "tables"); dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
rm(.path_candidates, .p)
cat("== supplier_structure ==\n")

# ---- vat -> NACE 4-digit lookup (per firm-year) ----
load(file.path(proc_data, "deployment_panel.RData"))
load(file.path(proc_data, "firm_year_belgian_euets.RData"))
nace <- bind_rows(
    deployment_panel %>% transmute(vat, year, nace5d),
    firm_year_belgian_euets %>% transmute(vat, year, nace5d)
  ) %>% filter(!is.na(nace5d)) %>% distinct(vat, year, .keep_all = TRUE) %>%
  transmute(vat, year, nace4d = make_nace4d(nace5d))
rm(deployment_panel, firm_year_belgian_euets)

# ---- B2B edges: distinct (buyer, supplier, year) with supplier NACE4d ----
load(file.path(proc_data, "b2b_selected_sample.RData"))
edges <- df_b2b_selected_sample %>% filter(corr_sales_ij > 0) %>%
  transmute(buyer = vat_j_ano, supplier = vat_i_ano, year) %>% distinct() %>%
  left_join(nace, by = c("supplier" = "vat", "year" = "year")) %>% filter(!is.na(nace4d))
rm(df_b2b_selected_sample)
cat(sprintf("  %d buyer-supplier-year edges (supplier NACE4d known), %s\n",
            nrow(edges), paste(range(edges$year), collapse = "-")))

# ---- per (buyer, year): suppliers per sector + sectors sourced ----
per_bs <- edges %>% group_by(buyer, year, nace4d) %>%
  summarise(n_sup = n_distinct(supplier), .groups = "drop")           # suppliers in each (buyer, sector)
per_buyer <- per_bs %>% group_by(buyer, year) %>%
  summarise(avg_sup_per_sector = mean(n_sup),                          # (1) within-sector base
            n_sectors          = n(),                                  # (2) across-sector base
            .groups = "drop")
cat(sprintf("  %d (buyer, year) observations\n", nrow(per_buyer)))

# ---- summary table ----
q <- function(x) c(mean = mean(x), p25 = quantile(x,.25), p50 = median(x),
                   p90 = quantile(x,.90), p99 = quantile(x,.99))
S <- rbind(`Suppliers per sector (avg, per buyer)` = q(per_buyer$avg_sup_per_sector),
           `Supplier sectors per buyer`            = q(per_buyer$n_sectors))
colnames(S) <- c("Mean","p25","Median","p90","p99")
print(round(S, 2))
cat(sprintf("  share of (buyer, sector) pairs that are single-sourced: %.1f%%\n",
            100*mean(per_bs$n_sup == 1)))

write.csv(data.frame(statistic = rownames(S), round(S, 3), row.names = NULL,
                     years = paste(range(edges$year), collapse = "-"),
                     pct_single_sourced = round(100*mean(per_bs$n_sup == 1), 1)),
          file.path(output_dir, "supplier_counts_summary.csv"), row.names = FALSE)

# ---- booktabs .tex for the paper (placed after the two-nest definition) ----
fmt <- function(r) paste(sprintf("%.2f", r), collapse = " & ")
tex <- c(
  "\\begin{tabular}{lccccc}", "\\toprule",
  "Buyer sourcing statistic & Mean & p25 & Median & p90 & p99 \\\\", "\\midrule",
  paste0("Suppliers per sector & ", fmt(S[1,]), " \\\\"),
  paste0("Supplier sectors sourced & ", fmt(S[2,]), " \\\\"),
  "\\bottomrule", "\\end{tabular}")
writeLines(tex, file.path(tab_dir, "supplier_structure.tex"))
cat("Done. Table (tables/supplier_structure.tex) + summary in", output_dir, "\n")
