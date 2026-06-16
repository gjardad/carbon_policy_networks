###############################################################################
# characterize_centrality.R  —  paper subsection 4.2 tables + figure.
#
# Pure post-processing of cf_centrality.csv (written by phase6_centrality.R, with
# the firm covariates appended). Produces:
#   tables/centrality_descriptives.tex  central firms (top decile) vs the rest
#   tables/centrality_regression.tex    nested R^2: does the network add info
#                                        beyond direct emissions?
#   figures/centrality_plane.pdf         technique vs composition: abatement-central
#                                        vs reallocation-central firms
# Centrality is signed negative (a reduction); we work with cen = -total so that
# "larger = more central". Runs anywhere once cf_centrality.csv is present.
###############################################################################
suppressMessages({ library(dplyr) })
.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1])           # output_dir (machine-split), tables/ figures/

f <- file.path(output_dir, "cf_centrality.csv")
if (!file.exists(f)) stop("cf_centrality.csv not found in ", output_dir, " - run phase6_centrality.R")
cf <- read.csv(f, colClasses = c(vat = "character"))
if (!"psi_e" %in% names(cf))
  stop("cf_centrality.csv has no covariate columns - re-run the updated phase6_centrality.R")

ind_nace   <- sprintf("%02d", c(5:9, 10:33, 35))
cf <- cf %>% mutate(
  industrial = !is.na(nace2d) & sprintf("%02d", as.integer(nace2d)) %in% ind_nace,
  cen = -total, cen_comp = -composition)        # larger = more central
eps <- 1e-12

# ============================ 1. DESCRIPTIVES ==============================
K <- max(1L, round(0.10 * nrow(cf)))             # top decile by centrality
cf$central <- rank(-cf$cen, ties.method = "first") <= K
# Each row: label, value vector, and a scale exponent s. The table entry is
# value/10^s shown to two decimals, with the label carrying "(x 10^s)" when s!=0.
# Direct emissions are physical scope-1 tonnes, recovered as e_bar * total_cost
# (since e_bar = scope1/total_cost), not the model's normalized baseline flow.
specs <- list(
  list(l = "Emission intensity $e_i$",      v = cf$e_bar,                s = -2),
  list(l = "Direct emissions $z_i$",        v = cf$e_bar * cf$total_cost, s =  4),
  list(l = "Log size $\\log x_i$",          v = log(pmax(cf$size, eps)), s =  0),
  list(l = "Domar weight $\\lambda_i$",     v = cf$domar,                s = -4),
  list(l = "\\# suppliers (in-degree)",     v = cf$in_deg,               s =  0),
  list(l = "\\# customers (out-degree)",    v = cf$out_deg,              s =  0),
  list(l = "Upstream EI $(\\Omega E)_i$",   v = cf$up1,                  s = -4),
  list(l = "Network-adj. EI $(\\Psi E)_i$", v = cf$psi_e,                s = -2),
  list(l = "Share industrial",              v = as.numeric(cf$industrial), s = 0),
  list(l = "Share ETS-regulated",           v = as.numeric(cf$ets == 1),   s = 0))
desc <- data.frame(
  variable = sapply(specs, `[[`, "l"),
  central  = sapply(specs, function(sp) mean(sp$v[cf$central],  na.rm = TRUE)),
  rest     = sapply(specs, function(sp) mean(sp$v[!cf$central], na.rm = TRUE)), row.names = NULL)
desc$ratio <- desc$central / desc$rest
fmt_row <- function(i) {
  sp <- specs[[i]]; lab <- if (sp$s != 0) sprintf("%s ($\\times 10^{%d}$)", sp$l, sp$s) else sp$l
  sprintf("%s & %.2f & %.2f & %.2f \\\\", lab,
          desc$central[i] / 10^sp$s, desc$rest[i] / 10^sp$s, desc$ratio[i])
}
descr_tex <- c(
  "\\begin{tabular}{lccc}", "\\toprule",      # columns 2-4 centred
  sprintf("Variable & Central (top %d\\%%) & Rest & Ratio \\\\", round(100 * K / nrow(cf))),
  "\\midrule",
  sapply(seq_along(specs), fmt_row),
  "\\bottomrule", "\\end{tabular}")
writeLines(descr_tex, file.path(output_dir, "tables", "centrality_descriptives.tex"))

# ============================ 2. NESTED REGRESSIONS ========================
# LHS: cen (= total centrality) and cen_comp (= reallocation-only centrality).
# Blocks: (1) direct emissions; (2) + size, intensity; (3) + sector FE;
#         (4) + network position. Headline = adj-R^2 added by block 4 beyond 1-3.
d <- cf %>% transmute(
  cen, cen_comp, nace2d = factor(nace2d),
  lz = log(pmax(z, eps)), lsize = log(pmax(size, eps)), e_bar,
  lup1 = log(up1 + eps), lpsi = log(pmax(psi_e, eps)),
  ldomar = log(pmax(domar, eps)), loutdeg = log(out_deg + 1)) %>%
  filter(is.finite(lsize), is.finite(ldomar))    # drop rows with missing size

blocks <- list(
  b1 = "lz",
  b2 = c("lz", "lsize", "e_bar"),
  b3 = c("lz", "lsize", "e_bar", "nace2d"),
  b4 = c("lz", "lsize", "e_bar", "nace2d", "lup1", "lpsi", "ldomar", "loutdeg"))
adjr2 <- function(lhs, rhs) summary(lm(reformulate(rhs, lhs), data = d))$adj.r.squared
prog <- data.frame(
  block = c("(1) emissions", "(2) + size, intensity", "(3) + sector FE", "(4) + network"),
  total       = sapply(blocks, adjr2, lhs = "cen"),
  composition = sapply(blocks, adjr2, lhs = "cen_comp"), row.names = NULL)

reg_tex <- c(
  "\\begin{tabular}{lrr}", "\\toprule",
  "Predictor block (cumulative) & \\multicolumn{2}{c}{Adjusted $R^2$} \\\\",
  "\\cmidrule(lr){2-3}",
  " & Total centrality & Reallocation only \\\\", "\\midrule",
  sprintf("%s & %.3f & %.3f \\\\", prog$block, prog$total, prog$composition),
  "\\midrule",
  sprintf("$\\Delta R^2$ from network (4)$-$(3) & %.3f & %.3f \\\\",
          prog$total[4] - prog$total[3], prog$composition[4] - prog$composition[3]),
  "\\bottomrule", "\\end{tabular}")
writeLines(reg_tex, file.path(output_dir, "tables", "centrality_regression.tex"))

# ============================ 3. FIGURE ====================================
fig_path <- file.path(output_dir, "figures", "centrality_plane.png")   # PNG: paper figure convention
ok_gg <- requireNamespace("ggplot2", quietly = TRUE)
if (ok_gg) {
  library(ggplot2)
  p <- ggplot(cf, aes(x = technique, y = composition, size = z,
                      color = ifelse(industrial, "Industrial", "Non-industrial"))) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("Industrial" = "#D6604D", "Non-industrial" = "#4393C3"),
                       name = NULL) +
    scale_size_continuous(guide = "none") +
    labs(x = "Abatement", y = "Reallocation") +          # no title; detail lives in the table note
    theme_minimal(base_size = 16) +
    theme(axis.title = element_text(size = 22), axis.text = element_text(size = 16),
          legend.text = element_text(size = 16), legend.position = "bottom")
  ggsave(fig_path, p, width = 6.5, height = 5, dpi = 150)
} else {
  png(fig_path, width = 6.5, height = 5, units = "in", res = 150)
  par(cex.lab = 1.6, cex.axis = 1.3)
  plot(cf$technique, cf$composition, cex = 0.4 + 3 * cf$z / max(cf$z, na.rm = TRUE),
       col = ifelse(cf$industrial, "#D6604D", "#4393C3"),
       xlab = "Abatement", ylab = "Reallocation")
  legend("bottomleft", c("Industrial", "Non-industrial"), pch = 1,
         col = c("#D6604D", "#4393C3"), cex = 1.3)
  dev.off()
}

# ============================ 4. HORSE-RACE TABLES =========================
# If proxy_horserace.R has been run, render its csv(s) as paper tables.
rule_lbl <- c(actual_ets = "Actual EU ETS", centrality = "Network centrality (model)",
              emissions = "Direct emissions $z_i$", intensity = "Emission intensity $e_i$",
              size = "Size $x_i$", domar = "Domar weight $\\lambda_i$",
              upstream_psiE = "Network-adj. EI $(\\Psi E)_i$", upstream_1layer = "Upstream EI $(\\Omega E)_i$",
              out_degree = "\\# customers", downstream = "Downstream influence")
relabel <- function(r) ifelse(r %in% names(rule_lbl), rule_lbl[r], r)

# Panel A: coverage-matched (firm count varies).
hr_f <- file.path(output_dir, "cf_horserace.csv")
if (file.exists(hr_f)) {
  hr <- read.csv(hr_f); hr <- hr[hr$rule != "domar", ]
  hr$label <- relabel(hr$rule); hr <- hr[order(hr$dlogZ), ]
  hr_tex <- c(
    "\\begin{tabular}{lrrr}", "\\toprule",
    "Targeting rule & \\# firms & $d\\log Z$ & \\% of gain \\\\", "\\midrule",
    sprintf("%s & %d & %.4f & %s \\\\", hr$label, hr$n_targeted, hr$dlogZ,
            ifelse(is.finite(hr$pct_gain_captured), sprintf("%.0f", hr$pct_gain_captured), "--")),
    "\\bottomrule", "\\end{tabular}")
  writeLines(hr_tex, file.path(output_dir, "tables", "centrality_horserace.tex"))
  cat("\n=== Horse-race A (coverage-matched) ===\n")
  print(hr[, c("label","n_targeted","dlogZ","pct_gain_captured")], row.names = FALSE, digits = 4)
} else cat("\n(cf_horserace.csv not found - run proxy_horserace.R)\n")

# Panel B: fixed firm count (pure selection quality, vs the network ranking).
hrn_f <- file.path(output_dir, "cf_horserace_fixedn.csv")
if (file.exists(hrn_f)) {
  hb <- read.csv(hrn_f); hb <- hb[hb$rule != "domar", ]
  hb$label <- relabel(hb$rule); hb <- hb[order(hb$dlogZ), ]
  hb_tex <- c(
    "\\begin{tabular}{lrr}", "\\toprule",
    sprintf("Targeting rule (top %d firms) & $d\\log Z$ & \\%% of centrality \\\\", hb$n_targeted[1]),
    "\\midrule",
    sprintf("%s & %.4f & %.0f \\\\", hb$label, hb$dlogZ, hb$pct_of_centrality),
    "\\bottomrule", "\\end{tabular}")
  writeLines(hb_tex, file.path(output_dir, "tables", "centrality_horserace_fixedn.tex"))
  cat("\n=== Horse-race B (fixed n) ===\n")
  print(hb[, c("label","n_targeted","dlogZ","pct_of_centrality")], row.names = FALSE, digits = 4)
} else cat("\n(cf_horserace_fixedn.csv not found - run the updated proxy_horserace.R)\n")

# ============================ console summary ==============================
cat("=== Descriptives (central top decile vs rest) ===\n"); print(desc, row.names = FALSE, digits = 3)
cat("\n=== Nested adjusted R^2 ===\n"); print(prog, row.names = FALSE, digits = 3)
cat(sprintf("\nNetwork block adds dR2: total %.3f, reallocation %.3f\n",
            prog$total[4] - prog$total[3], prog$composition[4] - prog$composition[3]))
cat(sprintf("\nWrote tables/{centrality_descriptives,centrality_regression}.tex and %s\n",
            basename(fig_path)))
