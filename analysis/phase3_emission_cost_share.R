###############################################################################
# analysis/phase3_emission_cost_share.R
#
# PURPOSE
#   How large is the carbon-cost wedge, defined EXACTLY as in the quantitative
#   exercise? The carbon bill enters the denominator (total cost is post-tax):
#
#       s_i = p_z * z_i / ( total_cost_i + p_z * z_i )  =  (p_z e_i)/(1 + p_z e_i)
#
#   where e_i = z_i / total_cost_i is the model's baseline emission intensity
#   (model_assembly.R) and the cost base is built the SAME way as the model:
#       total_cost = IC (NBB code 60/61, INCLUDES imports) + wages (code 62),
#       fallback (60/61 missing) = domestic input cost + total imports + wages.
#   So inputs = domestic + imported intermediates, plus labor, plus the carbon
#   bill p_z*z. s_i is the share of firm i's post-tax marginal cost that is
#   carbon -- bounded in (0,1) -- and its level says where the reallocation
#   channel can bite.
#
#   Emissions z from allocation_glo_balanced (the model's allocation), split:
#     s1 = ETS observed only (source == "ets");  s2 = ETS + GLO-imputed (all).
#
#   Two figures, each overlaying both samples:
#     (1) FIRM level: density of s_i across firm-years (log10, %).
#     (2) SECTOR level: per-(NACE 4-digit, year) MEAN of s_i, then LOG-demeaned
#         by year (log deviation from the year's geometric cross-sector mean).
#
# INPUT  {nbb}/processed/{annual_accounts_selected_sample, firm_year_domestic_input_cost,
#        firm_year_total_imports, firm_year_belgian_euets, deployment_panel}.RData
#        {nbb}/processed/allocation_glo_balanced/alloc_YYYY.RData  (vat, scope1, source)
# OUTPUT {output_dir}/figures/emission_cost_share_{firm,sector}.png
#        {output_dir}/emission_cost_share_summary.csv
#
# Price override:  Rscript phase3_emission_cost_share.R 80
# RUNS ON: local 1 (downsample) or RMD (full)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

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

# ---- Config ----
.args <- commandArgs(trailingOnly = TRUE)
PRICE <- if (length(.args) >= 1 && .args[1] != "") as.numeric(.args[1]) else 80  # EUR/tCO2 (paper benchmark p_z)
MIN_N <- 5                                          # min firms per (sector, year) for a sector average
ALLOC_DIR <- file.path(proc_data, "allocation_glo_balanced")
cat(sprintf("== emission_cost_share ==  price = %.0f EUR/tCO2 (carbon in denominator)\n", PRICE))

# ---- Load shared firm-characteristic tables once ----
load(file.path(proc_data, "annual_accounts_selected_sample.RData"))   # df_annual_accounts_selected_sample: v_0060_61, v_0001023
load(file.path(proc_data, "firm_year_domestic_input_cost.RData"))     # firm_year_domestic_input_cost: input_cost
load(file.path(proc_data, "firm_year_total_imports.RData"))           # firm_year_total_imports: total_imports
load(file.path(proc_data, "firm_year_belgian_euets.RData"))           # firm_year_belgian_euets: wage_bill, nace5d
load(file.path(proc_data, "deployment_panel.RData"))                  # deployment_panel: nace5d

aa  <- df_annual_accounts_selected_sample %>%
  transmute(vat = vat_ano, year, ic = as.numeric(v_0060_61), wage_aa = as.numeric(v_0001023))
dom <- firm_year_domestic_input_cost %>% transmute(vat, year, dom = input_cost)
imp <- firm_year_total_imports
if ("vat_ano" %in% names(imp)) imp <- rename(imp, vat = vat_ano)
imp <- imp %>% transmute(vat, year, imp = total_imports)
wge <- firm_year_belgian_euets %>% filter(!is.na(wage_bill)) %>% transmute(vat, year, wage_ets = wage_bill)
nace <- bind_rows(
    deployment_panel %>% transmute(vat, year, nace5d),
    firm_year_belgian_euets %>% transmute(vat, year, nace5d)
  ) %>% filter(!is.na(nace5d)) %>% distinct(vat, year, .keep_all = TRUE) %>%
  transmute(vat, year, nace4d = make_nace4d(nace5d))
rm(df_annual_accounts_selected_sample, firm_year_domestic_input_cost,
   firm_year_total_imports, firm_year_belgian_euets, deployment_panel)

# Cost base (model_assembly.R:41-64): IC 60/61 + wages; fallback domestic+imports+wages.
cost_base_tab <- aa %>%
  full_join(dom, by = c("vat","year")) %>% full_join(imp, by = c("vat","year")) %>%
  full_join(wge, by = c("vat","year")) %>%
  mutate(wage = coalesce(wage_aa, wage_ets, 0), dom = coalesce(dom, 0), imp = coalesce(imp, 0),
         use_ic = !is.na(ic) & ic > 0,
         total_cost = ifelse(use_ic, ic + wage, dom + imp + wage)) %>%
  filter(total_cost > 0) %>% select(vat, year, total_cost, use_ic)

# ---- Emissions per (vat, year): z_all (s2) and z_ets (s1) from the model's allocation ----
alloc_files <- sort(list.files(ALLOC_DIR, pattern = "^alloc_\\d+\\.RData$", full.names = TRUE))
if (length(alloc_files) == 0L) stop("no alloc_*.RData in ", ALLOC_DIR)
zL <- list()
for (f in alloc_files) {
  load(f)  # year_firms: vat, scope1, source (+ year)
  yf <- year_firms
  if (!"year" %in% names(yf)) yf$year <- as.integer(sub(".*alloc_(\\d+)\\.RData$", "\\1", f))
  zL[[length(zL) + 1L]] <- yf %>% filter(scope1 > 0) %>%
    group_by(vat, year) %>%
    summarise(z_all = sum(scope1), z_ets = sum(scope1[source == "ets"]), .groups = "drop")
}
z <- bind_rows(zL)

# ---- Join, compute the post-tax carbon-cost share s_i = p_z z / (cost + p_z z) ----
dat <- z %>% inner_join(cost_base_tab, by = c("vat","year")) %>% inner_join(nace, by = c("vat","year"))
cat(sprintf("  %d emitter firm-years with a model cost base over %s (%.0f%% use IC 60/61)\n",
            nrow(dat), paste(range(dat$year), collapse = "-"), 100*mean(dat$use_ic)))

# per-sample firm-level share (only firms with positive emissions in that sample)
mk_firm <- function(zz) {
  d <- dat %>% filter({{ zz }} > 0) %>%
    mutate(share = PRICE * {{ zz }} / (total_cost + PRICE * {{ zz }}), share_pct = 100 * share)
  d
}
firms_s1 <- mk_firm(z_ets); firms_s2 <- mk_firm(z_all)

mk_sec <- function(fm) fm %>%
  group_by(nace4d, year) %>% summarise(n = n(), share_pct = mean(share_pct), .groups = "drop") %>%
  filter(n >= MIN_N) %>% group_by(year) %>% mutate(ldm = log(share_pct) - mean(log(share_pct))) %>% ungroup()
sec_s1 <- mk_sec(firms_s1); sec_s2 <- mk_sec(firms_s2)

summ <- function(fm, sc, sec) {
  q <- quantile(fm$share_pct, c(.10,.50,.90,.99), na.rm = TRUE)
  cat(sprintf("[%s] %d emitter firm-years; %d (NACE4d,year) cells | firm share p50=%.2f%% p90=%.2f%% p99=%.2f%% | >5%%: %.1f%%\n",
              toupper(sc), nrow(fm), nrow(sec), q["50%"], q["90%"], q["99%"], 100*mean(fm$share_pct > 5)))
  data.frame(scenario = sc, price = PRICE, years = paste(range(fm$year), collapse = "-"),
             n_emit = nrow(fm), n_sectoryears = nrow(sec),
             firm_p50 = q["50%"], firm_p90 = q["90%"], firm_p99 = q["99%"], gt5 = 100*mean(fm$share_pct > 5))
}
write.csv(rbind(summ(firms_s1, "s1", sec_s1), summ(firms_s2, "s2", sec_s2)),
          file.path(output_dir, "emission_cost_share_summary.csv"), row.names = FALSE)

col_s1 <- "firebrick"; col_s2 <- "grey20"

# ---- (1) FIRM-LEVEL density (log10 %), both samples ----
png(file.path(fig_dir, "emission_cost_share_firm.png"), width = 6.5, height = 4.2, units = "in", res = 150)
op <- par(mar = c(4.4, 4.2, 1, 1))
d1 <- density(log10(firms_s1$share_pct)); d2 <- density(log10(firms_s2$share_pct))
xr <- range(c(d1$x, d2$x)); yr <- c(0, max(c(d1$y, d2$y)))
plot(d2, main = "", xlim = xr, ylim = yr, col = col_s2, lwd = 2, axes = FALSE,
     xlab = "Carbon cost as % of (post-tax) total input cost (log scale)", ylab = "Density")
lines(d1, col = col_s1, lwd = 2, lty = 2)
at <- seq(floor(xr[1]), ceiling(xr[2])); axis(1, at = at, labels = sprintf("%g%%", 10^at)); axis(2)
abline(v = log10(c(1, 5)), col = "grey60", lty = 3)
legend("topright", c("ETS observed (s1)", "ETS + imputed (s2)"), col = c(col_s1, col_s2),
       lwd = 2, lty = c(2, 1), bty = "n")
par(op); dev.off()

# ---- (2) SECTOR-LEVEL log-demeaned density, both samples ----
png(file.path(fig_dir, "emission_cost_share_sector.png"), width = 6.5, height = 4.2, units = "in", res = 150)
op <- par(mar = c(4.4, 4.2, 1, 1))
d1 <- density(sec_s1$ldm); d2 <- density(sec_s2$ldm)
xr <- range(c(d1$x, d2$x)); yr <- c(0, max(c(d1$y, d2$y)))
plot(d2, main = "", xlim = xr, ylim = yr, col = col_s2, lwd = 2,
     xlab = "Sector-mean carbon-cost share: log deviation from year mean", ylab = "Density")
lines(d1, col = col_s1, lwd = 2, lty = 2)
abline(v = 0, col = "grey60", lty = 3)
legend("topright", c("ETS observed (s1)", "ETS + imputed (s2)"), col = c(col_s1, col_s2),
       lwd = 2, lty = c(2, 1), bty = "n")
par(op); dev.off()

cat("Done. Figures + summary in", output_dir, "\n")
