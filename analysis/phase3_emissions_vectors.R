###############################################################################
# analysis/phase3_emissions_vectors.R
#
# PURPOSE
#   Build the firm-year direct-emission vector (tonnes CO2) under the two
#   imputation scenarios, joined to revenue and NACE codes. These vectors feed
#   the direct-EI dispersion (phase3_dispersion_direct.R), the comparison table
#   (phase3_table_dispersion_comparison.R), and the network build
#   (phase3_build_upstream_emissions.R).
#
#   S1 (zero):  non-ETS firms emit 0. Emitting firms = ETS firms with their
#               observed emissions (alloc rows with source == "ets").
#   S2 (GLO):   observed ETS + GLO-imputed non-ETS, from the already-run
#               allocation_glo_balanced/ (alloc rows, any source).
#
# INPUT
#   {NBB}/processed/allocation_glo_balanced/alloc_YYYY.RData (year_firms:
#         vat, year, crf_group, scope1, source, ...)
#   {NBB}/processed/deployment_panel.RData (vat, year, nace5d, revenue)
#
# OUTPUT
#   {PROJ}/data/processed/emissions_vectors_s1.RData  (ev_s1)
#   {PROJ}/data/processed/emissions_vectors_s2.RData  (ev_s2)
#     each: vat, year, z (tonnes CO2), revenue, nace5d, nace4d, nace2d, source
#
# RUNS ON: local 1 (consumes full-data allocation outputs)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages(library(dplyr))

# ---- Paths ----
if (tolower(Sys.info()[["user"]]) == "jardang") {
  nbb_data     <- "X:/Documents/JARDANG/NBB_data"
  project_root <- "X:/Documents/JARDANG/carbon_policy_networks"
} else {
  nbb_data     <- "c:/Users/jota_/Documents/NBB_data"
  project_root <- "c:/Users/jota_/Documents/carbon_policy_networks"
}
proc_data <- file.path(nbb_data, "processed")
out_data  <- file.path(project_root, "data", "processed")
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)
source(file.path(project_root, "utils", "sector_conventions.R"))

YEARS     <- 2005:2021
ALLOC_DIR <- file.path(proc_data, "allocation_glo_balanced")

cat("== phase3_emissions_vectors ==\n")

# ---- Firm characteristics (revenue + NACE) ----
# ETS firms are NOT in deployment_panel (that is the non-ETS deployment
# universe); their revenue/NACE come from firm_year_belgian_euets. Build the
# combined lookup from both (the two vat sets are disjoint).
load(file.path(proc_data, "deployment_panel.RData"))
dp_chars <- deployment_panel %>%
  filter(year %in% YEARS) %>%
  transmute(vat, year, nace5d, revenue)
rm(deployment_panel)

load(file.path(proc_data, "firm_year_belgian_euets.RData"))
ets_chars <- firm_year_belgian_euets %>%
  filter(year %in% YEARS) %>%
  transmute(vat, year, nace5d, revenue)
rm(firm_year_belgian_euets)

firm_chars <- bind_rows(dp_chars, ets_chars) %>%
  filter(!is.na(nace5d), !is.na(revenue), revenue > 0) %>%
  distinct(vat, year, .keep_all = TRUE) %>%
  mutate(nace4d = make_nace4d(nace5d),
         nace2d = make_nace2d(nace5d))
cat(sprintf("  firm_chars firm-years (deployment + euets, rev>0): %d\n",
            nrow(firm_chars)))

# ---- Stack allocation files ----
alloc_list <- list()
for (t in YEARS) {
  p <- file.path(ALLOC_DIR, sprintf("alloc_%d.RData", t))
  if (!file.exists(p)) { cat(sprintf("  year %d: no alloc file, skipped\n", t)); next }
  load(p)  # year_firms
  alloc_list[[as.character(t)]] <- year_firms %>%
    select(vat, year, scope1, source)
}
alloc <- bind_rows(alloc_list)
cat(sprintf("  allocation firm-years (scope1>0 universe): %d\n", nrow(alloc)))

# ---- Assemble scenario vectors ----
attach_chars <- function(df) {
  out <- df %>%
    left_join(firm_chars, by = c("vat", "year")) %>%
    filter(!is.na(nace5d), revenue > 0, z > 0)
  out
}

# S2: all allocated firms (observed ETS + GLO-imputed non-ETS)
ev_s2 <- alloc %>%
  transmute(vat, year, z = scope1, source) %>%
  attach_chars()

# S1: observed ETS only (non-ETS ignored / zero); drop pre_ets backcast & imputed
ev_s1 <- alloc %>%
  filter(source == "ets") %>%
  transmute(vat, year, z = scope1, source) %>%
  attach_chars()

cat(sprintf("  S2 emitting firm-years (z>0, rev>0, nace): %d\n", nrow(ev_s2)))
cat(sprintf("  S1 emitting firm-years (ETS observed):     %d\n", nrow(ev_s1)))
cat(sprintf("  S1 source breakdown: %s\n",
            paste(sprintf("%s=%d", names(table(ev_s1$source)), table(ev_s1$source)),
                  collapse = ", ")))
cat(sprintf("  S2 source breakdown: %s\n",
            paste(sprintf("%s=%d", names(table(ev_s2$source)), table(ev_s2$source)),
                  collapse = ", ")))

save(ev_s1, file = file.path(out_data, "emissions_vectors_s1.RData"))
save(ev_s2, file = file.path(out_data, "emissions_vectors_s2.RData"))
cat("  saved emissions_vectors_{s1,s2}.RData\n")
cat("Done.\n")
