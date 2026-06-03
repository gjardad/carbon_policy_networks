###############################################################################
# analysis/phase4_paper_coverage_tables.R
#
# PURPOSE
#   Generate two committable LaTeX tables for paper section 3.1:
#
#   1. tables/ets_coverage_belgium.tex
#        Year | Sample EU ETS (Mt) | Belgian total (Mt) | Stationary (Mt)
#             | % of total | % of stationary
#
#   2. tables/sample_coverage.tex
#        Year | Firms | EU ETS firms | VA bn EUR | VA % | WB bn EUR | WB %
#             | Em kt | % of EU ETS
#
#   Both tables share the same SAMPLE EU ETS numerator (verified Belgian-only
#   emissions of EU ETS firms that pass the De Loecker/Dhyne sample-selection
#   criteria). The coverage-share column in (1) and the old column 8 of the
#   inferring_emissions sample_coverage table use the same numerator + the
#   same "stationary" denominator from the NIR; we therefore drop column 8
#   from sample_coverage to avoid duplication.
#
# INPUTS
#   - NBB_data/raw/NIR/BEL-CRT-2025-V1.0-{2005,2008,2012,2020}-*.xlsx
#       (read directly via readxl; no PowerShell required)
#   - NBB_data/processed/annual_accounts_more_selected_sample.RData
#       (De Loecker / Dhyne sample; matches the sample used in inferring_emissions)
#   - NBB_data/processed/df_national_accounts_with_5digits.RData
#       (full annual-accounts universe; denominator for VA% and WB%)
#   - NBB_data/processed/firm_year_belgian_euets.RData
#       (firm-year EUTL panel; uses emissions_foreign flag if present)
#   - NBB_data/processed/installation_year_in_belgium.RData
#       (all Belgian ETS installations; denominator for % of EU ETS)
#
# OUTPUTS
#   - output_{rmd,local}/tables/ets_coverage_belgium.tex
#   - output_{rmd,local}/tables/sample_coverage.tex
#   - output_{rmd,local}/coverage_tables_summary.csv
#
#   Both tables are committed to the parent repo (no paper-submodule dance).
#   To publish into the paper, copy from output_{rmd,local}/tables/ into the
#   paper-submodule's tables/ and thesis/tables/ on whichever machine has the
#   paper submodule initialized (typically local 1):
#
#     cp output_rmd/tables/*.tex paper/tables/
#     cp output_rmd/tables/*.tex paper/thesis/tables/
#
# RUNS ON: RMD for real numbers (annual accounts are downsampled locally).
###############################################################################

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(stringr); library(tidyr)
})

# ---- Paths (centralized in utils/paths.R) ----
.path_candidates <- c(
  "C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
  "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
.p <- .path_candidates[file.exists(.path_candidates)]
if (length(.p) == 0L) stop("Cannot locate utils/paths.R; add a candidate.")
source(.p[1])
rm(.path_candidates, .p)

DISPLAY_YEARS <- c(2005, 2008, 2012, 2020)
NIR_DIR <- file.path(raw_data, "NIR")
TBL_DIR <- file.path(output_dir, "tables")
dir.create(TBL_DIR, recursive = TRUE, showWarnings = FALSE)

cat("== phase4_paper_coverage_tables ==\n  user:", Sys.info()[["user"]], "\n",
    sprintf("  years: %s\n", paste(DISPLAY_YEARS, collapse = ", ")))

# =============================================================================
# 1. NIR denominators from BEL-CRT XLSX (works on local and RMD)
#    stationary = 1.A.1 + 1.A.2 + (1.A.4 - 1.A.4.b) + 2   (per inferring's formula)
#    total      = Total national emissions - LULUCF (i.e. CO2 excluding LULUCF)
# =============================================================================
get_value <- function(f, sheet, label_regex) {
  x <- suppressMessages(read_excel(f, sheet = sheet, col_names = FALSE,
                                   .name_repair = "minimal"))
  lab <- as.character(x[[1]]); val <- suppressWarnings(as.numeric(as.character(x[[2]])))
  hit <- which(str_detect(lab, label_regex))
  if (length(hit) == 0L) NA_real_ else val[hit[1]]
}

read_crt_one <- function(year) {
  # Match any submission-year vintage (BEL-CRT-2024, BEL-CRT-2025, ...).
  # Prefer the newest vintage if multiple match.
  matches <- list.files(NIR_DIR,
                        pattern = sprintf("^BEL[-_]?CRT[-_]?\\d{4}[-_]?V?\\d*\\.?\\d*[-_]?%d[-_]", year),
                        full.names = TRUE)
  if (length(matches) == 0L)
    stop("Missing CRT file for year ", year, " in ", NIR_DIR,
         " (tried submission-year-agnostic pattern)")
  # Pick the file with the highest embedded submission year (most recent vintage)
  vint <- as.integer(stringr::str_extract(basename(matches), "(?<=BEL[-_]CRT[-_])\\d{4}"))
  f <- matches[which.max(vint)]
  d_1A1 <- get_value(f, "Table1",    "^1\\.A\\.1\\.\\s")
  d_1A2 <- get_value(f, "Table1",    "^1\\.A\\.2\\.\\s")
  d_1A4 <- get_value(f, "Table1",    "^1\\.A\\.4\\.\\s")
  d_1A4b<- get_value(f, "Table1",    "^1\\.A\\.4\\.b")
  d_2   <- get_value(f, "Table2(I)", "^2\\.\\s*Total")
  d_total_net <- get_value(f, "Summary1", "(?i)^\\s*Total national emissions")
  d_lulucf    <- get_value(f, "Summary1", "^\\s*4\\.\\s*Land")
  data.frame(year = year,
             stationary_kt = d_1A1 + d_1A2 + (d_1A4 - d_1A4b) + d_2,
             total_kt      = d_total_net - d_lulucf)
}

nir <- bind_rows(lapply(DISPLAY_YEARS, read_crt_one))
cat("\nNIR denominators (Mt):\n")
print(nir %>% mutate(stationary_Mt = round(stationary_kt/1e3, 1),
                     total_Mt = round(total_kt/1e3, 1)) %>%
      select(year, stationary_Mt, total_Mt))

# =============================================================================
# 2. Sample + full-universe aggregates from annual accounts
# =============================================================================
load(file.path(proc_data, "df_national_accounts_with_5digits.RData"))
load(file.path(proc_data, "annual_accounts_more_selected_sample.RData"))

# VA = v_0009800, wage_bill = v_0001023 (inferring convention)
agg_full <- df_national_accounts %>%
  filter(year %in% DISPLAY_YEARS) %>%
  group_by(year) %>%
  summarise(va_full = sum(v_0009800, na.rm = TRUE),
            wb_full = sum(v_0001023, na.rm = TRUE),
            n_full  = n_distinct(vat_ano),
            .groups = "drop")

agg_sample <- df_annual_accounts_more_selected_sample %>%
  filter(year %in% DISPLAY_YEARS) %>%
  group_by(year) %>%
  summarise(n_sample  = n_distinct(vat_ano),
            va_sample = sum(v_0009800, na.rm = TRUE),
            wb_sample = sum(v_0001023, na.rm = TRUE),
            .groups = "drop")

# =============================================================================
# 3. Sample EU ETS emissions (intersection of sample x EUTL, Belgian only)
# =============================================================================
load(file.path(proc_data, "firm_year_belgian_euets.RData"))

sample_firm_years <- df_annual_accounts_more_selected_sample %>%
  select(vat_ano, year) %>% distinct()

euets_in_sample <- firm_year_belgian_euets %>%
  inner_join(sample_firm_years, by = c("vat" = "vat_ano", "year" = "year")) %>%
  filter(year %in% DISPLAY_YEARS)

has_foreign_flag <- "emissions_foreign" %in% names(euets_in_sample)
if (!has_foreign_flag)
  message("NOTE: emissions_foreign not in firm_year_belgian_euets; ",
          "sample EU ETS will include foreign installations. ",
          "Re-run inferring_emissions/preprocess/build_firm_year_euets.R to add the flag.")

agg_ets_sample <- euets_in_sample %>%
  group_by(year) %>%
  summarise(n_ets_sample = n_distinct(vat),
            ets_sample_kt = if (has_foreign_flag) {
              sum(emissions - emissions_foreign, na.rm = TRUE) / 1e3
            } else {
              sum(emissions, na.rm = TRUE) / 1e3
            },
            .groups = "drop")

# =============================================================================
# 4. All Belgian ETS installation emissions (denominator for % of EU ETS)
# =============================================================================
load(file.path(proc_data, "installation_year_in_belgium.RData"))
agg_ets_all_be <- installation_year_in_belgium %>%
  filter(year %in% DISPLAY_YEARS, !is.na(verified)) %>%
  group_by(year) %>%
  summarise(ets_all_be_kt = sum(verified, na.rm = TRUE) / 1e3, .groups = "drop")

# =============================================================================
# 5. Assemble joined panel
# =============================================================================
tbl <- nir %>%
  left_join(agg_sample, by = "year") %>%
  left_join(agg_full,   by = "year") %>%
  left_join(agg_ets_sample, by = "year") %>%
  left_join(agg_ets_all_be, by = "year") %>%
  mutate(va_bn       = va_sample / 1e9,
         va_pct      = 100 * va_sample / va_full,
         wb_bn       = wb_sample / 1e9,
         wb_pct      = 100 * wb_sample / wb_full,
         ets_sample_Mt = ets_sample_kt / 1e3,
         total_Mt    = total_kt / 1e3,
         stationary_Mt = stationary_kt / 1e3,
         cov_total   = 100 * ets_sample_kt / total_kt,
         cov_stat    = 100 * ets_sample_kt / stationary_kt,
         cov_ets_all = 100 * ets_sample_kt / ets_all_be_kt)

cat("\nAssembled panel:\n")
print(tbl %>% select(year, n_sample, n_ets_sample, va_bn, va_pct, wb_bn, wb_pct,
                     ets_sample_Mt, total_Mt, stationary_Mt,
                     cov_total, cov_stat, cov_ets_all) %>%
      mutate(across(where(is.numeric), ~round(.x, 1))))

# =============================================================================
# 6. Write LaTeX tables
# =============================================================================
fmt1 <- function(x) ifelse(is.na(x), "$-$", sprintf("%.1f", x))
fmt0 <- function(x) ifelse(is.na(x), "$-$", formatC(round(x), format = "d", big.mark = ","))
fmt_pct <- function(x) ifelse(is.na(x), "$-$", sprintf("%.0f", x))

# ---- Table 1: ets_coverage_belgium ----
ets_tex <- paste(
"\\begin{table}[h]",
"    \\centering",
"    \\caption{EU ETS coverage of Belgian CO\\textsubscript{2} emissions in our sample, selected years}",
"    \\label{tab:ets_coverage_belgium}",
"    \\small",
"    \\begin{tabular}{l c c c c c}",
"        \\toprule",
"         & \\multicolumn{3}{c}{Emissions (Mt CO\\textsubscript{2})} & \\multicolumn{2}{c}{Coverage (\\%)} \\\\",
"        \\cmidrule(lr){2-4} \\cmidrule(lr){5-6}",
"        Year & Sample EU ETS & Belgian total & Stationary & of total & of stationary \\\\",
"        \\midrule",
sep = "\n")
ets_body <- paste(sapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  sprintf("        %d & %s & %s & %s & %s & %s \\\\",
          r$year, fmt1(r$ets_sample_Mt), fmt1(r$total_Mt), fmt1(r$stationary_Mt),
          fmt_pct(r$cov_total), fmt_pct(r$cov_stat))
}), collapse = "\n")
ets_tail <- paste(
"        \\bottomrule",
"    \\end{tabular}",
"    \\begin{minipage}{\\textwidth}",
"        {\\footnotesize \\vspace{0.25cm} \\noindent \\textit{Notes:} \\emph{Sample EU ETS} sums verified CO\\textsubscript{2} emissions of EU ETS firms in our analysis sample (firms meeting the De Loecker/Dhyne sample-selection criteria) from installations located in Belgium. \\emph{Belgian total} is Belgium's total CO\\textsubscript{2} emissions excluding land use, land-use change and forestry, from the National Inventory Report (NIR). \\emph{Stationary} sums CO\\textsubscript{2} emissions from stationary installations as reported in the NIR --- fuel combustion in energy industries (Common Reporting Format category 1.A.1), manufacturing and construction (1.A.2), other sectors excluding residential heating (1.A.4 -- 1.A.4.b), plus all industrial process emissions (2). Coverage shares are sample EU ETS emissions divided by each total.}",
"    \\end{minipage}",
"\\end{table}",
sep = "\n")
ets_full <- paste(ets_tex, ets_body, ets_tail, sep = "\n")

# ---- Table 2: sample_coverage (inferring's table minus old col 8) ----
sc_tex <- paste(
"\\begin{table}[h]",
"    \\centering",
"    \\caption{Coverage of the analysis sample, selected years}",
"    \\label{tab:sample_coverage}",
"    \\small",
"    \\begin{tabular}{l c c c c c c c c}",
"        \\toprule",
"         & & & \\multicolumn{2}{c}{Value added} & \\multicolumn{2}{c}{Wage bill} & \\multicolumn{2}{c}{Emissions} \\\\",
"        \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
"         & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\",
"        Year & Firms & EU\\,ETS firms & bn\\,\\euro{} & \\% of agg. & bn\\,\\euro{} & \\% of agg. & kt\\,CO$_2$ & \\% of EU\\,ETS \\\\",
"        \\midrule\\midrule",
sep = "\n")
sc_body <- paste(sapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  sprintf("        %d & %s & %s & %s & %s\\%% & %s & %s\\%% & %s & %s\\%% \\\\",
          r$year, fmt0(r$n_sample), fmt0(r$n_ets_sample),
          fmt0(r$va_bn), sprintf("%.1f", r$va_pct),
          fmt0(r$wb_bn), sprintf("%.1f", r$wb_pct),
          fmt0(r$ets_sample_kt),
          sprintf("%.1f", r$cov_ets_all))
}), collapse = " \\midrule\n")
sc_tail <- paste(
"        \\bottomrule",
"    \\end{tabular}",
"    \\begin{minipage}{\\textwidth}",
"        {\\footnotesize \\vspace{0.25cm} \\noindent \\textit{Notes:} Columns~(1) reports the number of firms in our analysis sample (De Loecker/Dhyne sample-selection criteria); Column~(2) reports the number of those that are also EU ETS-regulated. Columns~(3) and~(5) report sample aggregate value added and wage bill in billion euros (current prices). Columns~(4) and~(6) report these as a share of the corresponding totals across all Belgian firms filing annual accounts. Column~(7) reports verified CO$_2$ emissions of EU ETS firms in the sample from installations located in Belgium, in kilotonnes. Column~(8) reports this as a share of total verified emissions across all Belgian EU ETS installations; the gap reflects EU ETS firms that do not meet the sample-selection criteria.}",
"    \\end{minipage}",
"\\end{table}",
sep = "\n")
sc_full <- paste(sc_tex, sc_body, sc_tail, sep = "\n")

# ---- Write tables + CSV to output_{rmd,local}/ ----
writeLines(ets_full, file.path(TBL_DIR, "ets_coverage_belgium.tex"))
writeLines(sc_full,  file.path(TBL_DIR, "sample_coverage.tex"))
cat(sprintf("  wrote %s/ets_coverage_belgium.tex\n", TBL_DIR))
cat(sprintf("  wrote %s/sample_coverage.tex\n",  TBL_DIR))

csv_out <- tbl %>%
  select(year, n_sample, n_ets_sample, va_bn, va_pct, wb_bn, wb_pct,
         ets_sample_Mt, ets_sample_kt, total_Mt, stationary_Mt,
         cov_total, cov_stat, cov_ets_all)
write.csv(csv_out, file.path(output_dir, "coverage_tables_summary.csv"),
          row.names = FALSE)
cat(sprintf("\nSaved CSV: %s\n", file.path(output_dir, "coverage_tables_summary.csv")))
cat("\nDone.\n")
