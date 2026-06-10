###############################################################################
# make_cf_tables.R  —  paper tables for the counterfactual subsections (4.3-4.5).
#
# Reads the canonical counterfactual results (output_rmd/cf_results.csv, written by
# run_counterfactuals_rmd.R) and renders LaTeX tables to output_rmd/tables/:
#   cf_ets_decomp.tex       4.3  EU ETS effect, decomposed by sigma (p_z=80)
#   cf_price_escalation.tex 4.3  EU ETS effect as the carbon price rises (sigma=0.5)
#   cf_schemes_compare.tex  4.4/4.5  ETS vs universal vs centrality, by sigma (p_z=80)
# Always reads/writes output_rmd (the canonical RMD results), so the tables are
# machine-independent. Run after pulling a fresh cf_results.csv.
###############################################################################
.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1])                       # project_root
cf_dir <- file.path(project_root, "output_rmd")
tab_dir <- file.path(cf_dir, "tables"); dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

res <- read.csv(file.path(cf_dir, "cf_results.csv"))
BENCH_P <- 80; DEF_SIGMA <- 0.5
f3 <- function(x) formatC(x, format = "f", digits = 3)
sig <- function(x) ifelse(x == round(x), sprintf("%.1f", x), formatC(x, format = "g", digits = 3))
pct <- function(rule, ets) sprintf("%+.1f", 100 * (rule - ets) / ets)   # extra reduction, % of ETS

b <- res[res$p_z == BENCH_P, ]

# ---- 4.3 Table 1: ETS decomposition by sigma (p_z = 80) ----
e <- b[b$scheme == "actual_ets", ]; e <- e[order(e$sigma), ]
t1 <- c("\\begin{tabular}{lrrrr}", "\\toprule",
        "$\\sigma$ & $d\\log Z$ & Scale & Technique & Composition \\\\", "\\midrule",
        sprintf("%s & %s & %s & %s & %s \\\\", sig(e$sigma), f3(e$dlogZ), f3(e$scale),
                f3(e$technique), f3(e$composition)),
        "\\bottomrule", "\\end{tabular}")
writeLines(t1, file.path(tab_dir, "cf_ets_decomp.tex"))

# ---- 4.3 Table 2: price escalation (sigma = 0.5) ----
p <- res[res$scheme == "actual_ets" & res$sigma == DEF_SIGMA, ]; p <- p[order(p$p_z), ]
t2 <- c("\\begin{tabular}{lrrrr}", "\\toprule",
        "$p_z$ (EUR/t) & $d\\log Z$ & Scale & Technique & Composition \\\\", "\\midrule",
        sprintf("%d & %s & %s & %s & %s \\\\", p$p_z, f3(p$dlogZ), f3(p$scale),
                f3(p$technique), f3(p$composition)),
        "\\bottomrule", "\\end{tabular}")
writeLines(t2, file.path(tab_dir, "cf_price_escalation.tex"))

# ---- 4.4/4.5 Table 3: scheme comparison by sigma (p_z = 80) ----
w <- reshape(b[, c("scheme", "sigma", "dlogZ")], idvar = "sigma",
             timevar = "scheme", direction = "wide")
w <- w[order(w$sigma), ]
ets <- w[["dlogZ.actual_ets"]]; uni <- w[["dlogZ.universal_industrial"]]; cen <- w[["dlogZ.centrality"]]
t3 <- c("\\begin{tabular}{lrrrrr}", "\\toprule",
        " & \\multicolumn{1}{c}{EU ETS} & \\multicolumn{2}{c}{Universal industrial} & \\multicolumn{2}{c}{Centrality} \\\\",
        "\\cmidrule(lr){2-2}\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}",
        "$\\sigma$ & $d\\log Z$ & $d\\log Z$ & vs ETS & $d\\log Z$ & vs ETS \\\\", "\\midrule",
        sprintf("%s & %s & %s & %s\\%% & %s & %s\\%% \\\\", sig(w$sigma), f3(ets),
                f3(uni), pct(uni, ets), f3(cen), pct(cen, ets)),
        "\\bottomrule", "\\end{tabular}")
writeLines(t3, file.path(tab_dir, "cf_schemes_compare.tex"))

cat("Wrote cf_ets_decomp.tex, cf_price_escalation.tex, cf_schemes_compare.tex to", tab_dir, "\n\n")
cat("=== ETS decomposition (p_z=80) ===\n"); print(e[, c("sigma","dlogZ","scale","technique","composition")], row.names = FALSE)
cat("\n=== Scheme comparison (p_z=80) ===\n")
print(data.frame(sigma = w$sigma, ets = ets, universal = uni, uni_pct = (uni-ets)/ets*100,
                 centrality = cen, cen_pct = (cen-ets)/ets*100), row.names = FALSE, digits = 3)
