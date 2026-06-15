###############################################################################
# proxy_horserace.R  —  can a regulator approximate centrality targeting with a
#                       simple, OBSERVABLE ranking rule?
#
# For each candidate rule (rank firms by one observable), select the top firms
# until the SAME emission coverage as the actual ETS, solve the model once, and
# record the realized aggregate reduction. Compare against the two benchmarks:
#   actual_ets  = what the realized policy did            (0% of the gain)
#   centrality  = the model-optimal set (by total dlogZ)  (100% of the gain)
# so  pct_gain_captured = (dlogZ_rule - dlogZ_ets) / (dlogZ_centrality - dlogZ_ets).
#
# Answers: does the network model earn its keep, or does ranking by a single
# observable (emissions, intensity, upstream EI, degree, ...) already capture most
# of the achievable reduction?
#
# TWO panels:
#   A. cf_horserace.csv         coverage-matched: each rule taxes down to the SAME
#                               EMISSION coverage as the ETS (firm count varies by rule).
#   B. cf_horserace_fixedn.csv  fixed firm count: each rule taxes the top-N firms, N =
#                               size of the centrality set, so the administrative footprint
#                               is held fixed -> pure selection quality (removes A's
#                               coverage-vs-count confound). Benchmarked against centrality.
# ~16 solves total; needs the cached bundle and cf_centrality.csv (run phase6_centrality.R).
###############################################################################
.cc <- c("C:/Users/jardang/Documents/carbon_policy_networks/analysis/cf_common.R",
         "c:/Users/jota_/Documents/carbon_policy_networks/analysis/cf_common.R")
source(.cc[file.exists(.cc)][1])              # cached bundle, solver, get_scheme_tau, output_dir

PZ <- 80                                       # benchmark price (realized 2022)
stats <- firm_network_stats(bundle)
em    <- which(bundle$e_bar > 0)               # emitters: the rule-rankable pool
base0 <- full_solve(0, SIGMA_B, SIGMA_W, DEF_RHO, DEF_ALPHA, bundle, bshare)
ets_cov <- sum(bundle$z[bundle$tau == 1])      # coverage budget to match
cat(sprintf("Horse-race: %d emitters, matching ETS coverage = %.4g (of Z0=%.4g)\n",
            length(em), ets_cov, sum(base0$z[em])))

# top firms by `val` (among emitters) until cumulative emissions reach the budget
select_to_coverage <- function(val, target) {
  ord <- em[order(val[em], decreasing = TRUE)]
  k <- which(cumsum(bundle$z[ord]) >= target)[1]; if (is.na(k)) k <- length(ord)
  ord[seq_len(k)]
}
solve_set <- function(tau) {
  bj <- bundle; bj$tau <- as.integer(tau)
  d  <- dec1(full_solve(PZ, SIGMA_B, SIGMA_W, DEF_RHO, DEF_ALPHA, bj, bshare), base0, em)
  c(n_targeted = sum(bj$tau), d[c("total", "technique", "scale", "composition")])
}

# observable ranking rules a regulator could actually implement
# (Domar weight omitted: lambda_i proportional to size_i, so it ranks identically to size.)
rules <- list(emissions = stats$z, intensity = stats$e_bar, size = stats$size,
              upstream_psiE = stats$psi_e, upstream_1layer = stats$up1,
              out_degree = stats$out_deg, downstream = stats$downstream)

rows <- list()
n_all <- length(bundle$gamma)
for (nm in names(rules)) {
  cat(sprintf("  rule %-16s ...\n", nm)); gc()
  sel <- select_to_coverage(rules[[nm]], ets_cov)
  rows[[nm]] <- data.frame(rule = nm, t(solve_set(seq_len(n_all) %in% sel)))
}
# benchmarks (reuse cf_common's exact tau construction)
cat("  benchmarks actual_ets, centrality ...\n")
rows[["actual_ets"]] <- data.frame(rule = "actual_ets", t(solve_set(get_scheme_tau("actual_ets"))))
rows[["centrality"]] <- data.frame(rule = "centrality", t(solve_set(get_scheme_tau("centrality"))))

res <- do.call(rbind, rows); rownames(res) <- NULL
names(res) <- c("rule", "n_targeted", "dlogZ", "technique", "scale", "composition")
ets_d <- res$dlogZ[res$rule == "actual_ets"]; cen_d <- res$dlogZ[res$rule == "centrality"]
res$pct_gain_captured <- 100 * (res$dlogZ - ets_d) / (cen_d - ets_d)
res <- res[order(res$dlogZ), ]

write.csv(res, file.path(output_dir, "cf_horserace.csv"), row.names = FALSE)
cat(sprintf("\n[A] Wrote cf_horserace.csv to %s  (p_z=%d, sigma_B=%.2f, sigma_W=%.2f, rho=%.2f, alpha=%.0f)\n",
            output_dir, PZ, SIGMA_B, SIGMA_W, DEF_RHO, DEF_ALPHA))
print(res, row.names = FALSE, digits = 4)

# ===========================================================================
# Panel B: FIXED FIRM COUNT. Target the top-N firms by each rule, N = size of the
# centrality set, so every rule taxes the same number of firms. Pure selection
# quality at fixed footprint. pct_of_centrality = 100 * dlogZ / dlogZ_centrality
# (>100 = beats the network ranking at equal firm count; <100 = worse).
# ===========================================================================
cen_tau <- get_scheme_tau("centrality"); N <- sum(cen_tau)
cat(sprintf("\nFixed-n panel: top-%d firms by each rule ...\n", N))
select_top_n <- function(val, N) em[order(val[em], decreasing = TRUE)][seq_len(min(N, length(em)))]

rowsB <- list()
for (nm in names(rules)) {
  cat(sprintf("  rule %-16s ...\n", nm)); gc()
  sel <- select_top_n(rules[[nm]], N)
  rowsB[[nm]] <- data.frame(rule = nm, t(solve_set(seq_len(n_all) %in% sel)))
}
rowsB[["centrality"]] <- data.frame(rule = "centrality", t(solve_set(cen_tau)))
resB <- do.call(rbind, rowsB); rownames(resB) <- NULL
names(resB) <- c("rule", "n_targeted", "dlogZ", "technique", "scale", "composition")
resB$pct_of_centrality <- 100 * resB$dlogZ / resB$dlogZ[resB$rule == "centrality"]
resB <- resB[order(resB$dlogZ), ]

write.csv(resB, file.path(output_dir, "cf_horserace_fixedn.csv"), row.names = FALSE)
cat(sprintf("\n[B] Wrote cf_horserace_fixedn.csv to %s  (N=%d firms each)\n", output_dir, N))
print(resB, row.names = FALSE, digits = 4)
