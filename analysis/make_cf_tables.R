###############################################################################
# make_cf_tables.R  —  paper tables for the counterfactual subsections (5.4-5.6).
#
# Reads the canonical counterfactual results (output_rmd/cf_results.csv, written by
# run_counterfactuals_rmd.R) and renders LaTeX tables to output_rmd/tables/:
#   cf_ets_decomp.tex       5.4  EU ETS effect at the baseline calibration (p_z=80),
#                                decomposed into scale / technique / composition
#   cf_price_escalation.tex 5.4  EU ETS effect as the carbon price rises (baseline)
#   cf_schemes_compare.tex  5.5/5.6  ETS vs universal vs centrality (baseline, p_z=80)
#
# Single-baseline layout: sigma_B=0.1, sigma_W=2.5, rho=0.7, alpha=2 (quantitative.tex
# 5.2). Robustness sweeps (FULL_GRID in cf_common.R) add other cells to the CSV; we
# select the baseline cell here. Always reads/writes output_rmd (canonical RMD
# results), so the tables are machine-independent. Run after pulling cf_results.csv.
###############################################################################
.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1])                       # project_root
cf_dir <- file.path(project_root, "output_rmd")
tab_dir <- file.path(cf_dir, "tables"); dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

res <- read.csv(file.path(cf_dir, "cf_results.csv"))
BENCH_P <- 80
BASE_SB <- 0.1; BASE_SW <- 2.5; BASE_RHO <- 0.7; BASE_ALPHA <- 2   # mirrors cf_common.R baseline
f3  <- function(x) formatC(x, format = "f", digits = 3)
pct <- function(rule, ets) sprintf("%+.1f", 100 * (rule - ets) / ets)   # extra reduction, % of ETS

# baseline cell only (robust if the CSV also carries robustness sweeps)
near <- function(a, b) abs(a - b) < 1e-9
base_cell <- res[near(res$sigma_B, BASE_SB) & near(res$sigma_W, BASE_SW) &
                 near(res$rho, BASE_RHO) & near(res$alpha, BASE_ALPHA), ]
if (!nrow(base_cell)) stop("no baseline cell in cf_results.csv (sigma_B=", BASE_SB,
                           ", sigma_W=", BASE_SW, ", rho=", BASE_RHO, ", alpha=", BASE_ALPHA, ")")
b <- base_cell[base_cell$p_z == BENCH_P, ]

# ---- 5.4 Table 1: ETS decomposition at the baseline calibration (p_z = 80) ----
e <- b[b$scheme == "actual_ets", ]
t1 <- c("\\begin{tabular}{lr}", "\\toprule",
        "Channel & $\\Delta\\log Z$ \\\\", "\\midrule",
        sprintf("Total & %s \\\\", f3(e$dlogZ)),
        sprintf("Scale & %s \\\\", f3(e$scale)),
        sprintf("Technique & %s \\\\", f3(e$technique)),
        sprintf("Composition & %s \\\\", f3(e$composition)),
        "\\bottomrule", "\\end{tabular}")
writeLines(t1, file.path(tab_dir, "cf_ets_decomp.tex"))

# ---- 5.4 Table 2: price escalation at the baseline (p_z varies) ----
p <- base_cell[base_cell$scheme == "actual_ets", ]; p <- p[order(p$p_z), ]
t2 <- c("\\begin{tabular}{lrrrr}", "\\toprule",
        "$p_z$ (EUR/t) & $\\Delta\\log Z$ & Scale & Technique & Composition \\\\", "\\midrule",
        sprintf("%d & %s & %s & %s & %s \\\\", p$p_z, f3(p$dlogZ), f3(p$scale),
                f3(p$technique), f3(p$composition)),
        "\\bottomrule", "\\end{tabular}")
writeLines(t2, file.path(tab_dir, "cf_price_escalation.tex"))

# ---- 5.5/5.6 Table 3: scheme comparison at the baseline (p_z = 80) ----
getd <- function(sch) b$dlogZ[b$scheme == sch]
ets <- getd("actual_ets"); uni <- getd("universal_industrial"); cen <- getd("centrality")
t3 <- c("\\begin{tabular}{lrr}", "\\toprule",
        "Scheme & $\\Delta\\log Z$ & vs ETS \\\\", "\\midrule",
        sprintf("EU ETS & %s & --- \\\\", f3(ets)),
        sprintf("Universal industrial & %s & %s\\%% \\\\", f3(uni), pct(uni, ets)),
        sprintf("Centrality (coverage-matched) & %s & %s\\%% \\\\", f3(cen), pct(cen, ets)),
        "\\bottomrule", "\\end{tabular}")
writeLines(t3, file.path(tab_dir, "cf_schemes_compare.tex"))

cat("Wrote cf_ets_decomp.tex, cf_price_escalation.tex, cf_schemes_compare.tex to", tab_dir, "\n\n")
cat(sprintf("Baseline: sigma_B=%.2f sigma_W=%.2f rho=%.2f alpha=%.0f\n", BASE_SB, BASE_SW, BASE_RHO, BASE_ALPHA))
cat("=== ETS decomposition (p_z=80) ===\n")
print(e[, c("dlogZ","scale","technique","composition")], row.names = FALSE)
cat("\n=== Scheme comparison (p_z=80) ===\n")
print(data.frame(ets = ets, universal = uni, uni_pct = (uni-ets)/ets*100,
                 centrality = cen, cen_pct = (cen-ets)/ets*100), row.names = FALSE, digits = 3)
