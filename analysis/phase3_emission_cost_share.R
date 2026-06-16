###############################################################################
# analysis/phase3_emission_cost_share.R
#
# PURPOSE
#   How large is the carbon-cost wedge? For a hypothetical carbon price PRICE
#   (EUR/tCO2), the emission-cost share of a firm's total input cost is
#       s_i = PRICE * scope1_i / total_cost_i = PRICE * e_cost_i
#   i.e. the direct carbon cost as a fraction of the firm's marginal cost.
#   Its LEVEL says where the reallocation channel can bite: a tiny s_i means a
#   tiny cross-supplier price gap when one supplier is taxed, so substitution is
#   muted regardless of sigma. (Denominator total_cost = wages + domestic B2B +
#   imports + the small historical EUTL carbon cost; it omits capital, so the
#   dirtiest firms can exceed 100%.)
#
#   Two figures, each overlaying both samples (s1 = ETS observed only,
#   s2 = full = observed ETS + GLO-imputed non-ETS), as Facts 1-2 do:
#     (1) FIRM level: density of s_i across firm-years (log10, %).
#     (2) SECTOR level: per-(NACE 4-digit, year) MEAN of s_i, then LOG-demeaned
#         by year (log deviation from the year's geometric cross-sector mean).
#
# INPUT  {PROJ}/data/processed/upstream_emissions_{s1,s2}/firms_YYYY.RData
# OUTPUT {output_dir}/figures/emission_cost_share_firm.png
#        {output_dir}/figures/emission_cost_share_sector.png
#        {output_dir}/emission_cost_share_summary.csv
#
# Price override:  Rscript phase3_emission_cost_share.R 80
# RUNS ON: local 1 (2014-2015 downsample) or RMD (2005-2021 full)
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
proc <- out_data
fig_dir <- file.path(output_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
rm(.path_candidates, .p)

# ---- Config ----
.args <- commandArgs(trailingOnly = TRUE)
PRICE <- if (length(.args) >= 1 && .args[1] != "") as.numeric(.args[1]) else 80  # EUR/tCO2 (paper benchmark p_z)
MIN_N <- 5      # min firms per (sector, year) for a sector average
cat(sprintf("== emission_cost_share ==  price = %.0f EUR/tCO2\n", PRICE))

load_build <- function(scn) {
  d <- file.path(proc, sprintf("upstream_emissions_%s", scn))
  fs <- sort(list.files(d, pattern = "^firms_\\d+\\.RData$", full.names = TRUE))
  if (length(fs) == 0L) stop("no firms_*.RData in ", d, " (build with phase3_build_upstream_emissions.R)")
  L <- list(); for (f in fs) { load(f); L[[length(L) + 1L]] <- firms }
  bind_rows(L)
}

# Build firm-level shares and log-demeaned sector means for one scenario.
build_scn <- function(scn) {
  fa <- load_build(scn) %>% filter(!is.na(nace4d), total_cost > 0, is.finite(e_cost))
  n_all <- nrow(fa); n_emit <- sum(fa$scope1 > 0)
  firms <- fa %>% filter(scope1 > 0) %>% mutate(share_pct = 100 * PRICE * e_cost)
  qf <- quantile(firms$share_pct, c(.10,.50,.90,.99), na.rm = TRUE)
  g1a <- 100 * sum(firms$share_pct > 1) / n_all; g5a <- 100 * sum(firms$share_pct > 5) / n_all
  g1e <- 100 * mean(firms$share_pct > 1);          g5e <- 100 * mean(firms$share_pct > 5)
  sec <- firms %>%
    group_by(nace4d, year) %>%
    summarise(n = n(), share_pct = mean(share_pct), .groups = "drop") %>%
    filter(n >= MIN_N) %>%
    group_by(year) %>% mutate(ldm = log(share_pct) - mean(log(share_pct))) %>% ungroup()
  cat(sprintf("[%s] %d firm-years (%.1f%% emitters); %d (NACE4d,year) cells | firm p50=%.2f%% p90=%.2f%% p99=%.2f%% | >1%%: %.1f%% of all\n",
              toupper(scn), n_all, 100*n_emit/n_all, nrow(sec), qf["50%"], qf["90%"], qf["99%"], g1a))
  list(scn = scn, firms = firms, sec = sec,
       row = data.frame(scenario = scn, price = PRICE, years = paste(range(firms$year), collapse = "-"),
                        n_all = n_all, n_emit = n_emit, n_sectoryears = nrow(sec),
                        firm_p50 = qf["50%"], firm_p90 = qf["90%"], firm_p99 = qf["99%"],
                        gt1_all = g1a, gt5_all = g5a, gt1_emit = g1e, gt5_emit = g5e))
}

S <- lapply(c("s1", "s2"), build_scn); names(S) <- c("s1", "s2")
write.csv(bind_rows(lapply(S, `[[`, "row")),
          file.path(output_dir, "emission_cost_share_summary.csv"), row.names = FALSE)

col_s1 <- "firebrick"; col_s2 <- "grey20"

# ---- (1) FIRM-LEVEL density (log10 %), both samples ----
png(file.path(fig_dir, "emission_cost_share_firm.png"), width = 6.5, height = 4.2, units = "in", res = 150)
op <- par(mar = c(4.4, 4.2, 1, 1))
d1 <- density(log10(S$s1$firms$share_pct)); d2 <- density(log10(S$s2$firms$share_pct))
xr <- range(c(d1$x, d2$x)); yr <- c(0, max(c(d1$y, d2$y)))
plot(d2, main = "", xlim = xr, ylim = yr, col = col_s2, lwd = 2, axes = FALSE,
     xlab = "Carbon cost as % of total input cost (log scale)", ylab = "Density")
lines(d1, col = col_s1, lwd = 2, lty = 2)
at <- seq(floor(xr[1]), ceiling(xr[2])); axis(1, at = at, labels = sprintf("%g%%", 10^at)); axis(2)
abline(v = log10(c(1, 5)), col = "grey60", lty = 3)
legend("topright", c("ETS observed (s1)", "ETS + imputed (s2)"), col = c(col_s1, col_s2),
       lwd = 2, lty = c(2, 1), bty = "n")
par(op); dev.off()

# ---- (2) SECTOR-LEVEL log-demeaned density, both samples ----
png(file.path(fig_dir, "emission_cost_share_sector.png"), width = 6.5, height = 4.2, units = "in", res = 150)
op <- par(mar = c(4.4, 4.2, 1, 1))
d1 <- density(S$s1$sec$ldm); d2 <- density(S$s2$sec$ldm)
xr <- range(c(d1$x, d2$x)); yr <- c(0, max(c(d1$y, d2$y)))
plot(d2, main = "", xlim = xr, ylim = yr, col = col_s2, lwd = 2,
     xlab = "Sector-mean carbon-cost share: log deviation from year mean",
     ylab = "Density")
lines(d1, col = col_s1, lwd = 2, lty = 2)
abline(v = 0, col = "grey60", lty = 3)
legend("topright", c("ETS observed (s1)", "ETS + imputed (s2)"), col = c(col_s1, col_s2),
       lwd = 2, lty = c(2, 1), bty = "n")
par(op); dev.off()

cat("Done. Figures + summary in", output_dir, "\n")
