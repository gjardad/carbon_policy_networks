###############################################################################
# analysis/phase3_emission_cost_share.R
#
# PURPOSE
#   The carbon-cost wedge, defined EXACTLY as in the quantitative exercise --
#   carbon bill in the denominator (post-tax share, bounded in (0,1)):
#
#       s_{i,t} = p_z z / (total_cost + p_z z) = (p_z e)/(1 + p_z e),
#
#   cost base built like model_assembly.R: total_cost = IC (NBB 60/61, INCLUDES
#   imports) + wages (62), fallback domestic+imports+wages. e = z/total_cost.
#   Emissions z from allocation_glo_balanced (the model's allocation).
#
#   We summarise the cross-sectional dispersion of the wedge, net of year:
#   let d_{i,t} = log s_{i,t} - mean_t(log s) be the YEAR-DEMEANED log share
#   (demeaned within the sample being plotted). Then
#     - SECTOR histogram: one point per NACE4d = within-sector mean of d_{i,t}.
#     - FIRM histogram  : one point per firm   = within-firm, across-year mean of d_{i,t}.
#
#   TWO SAMPLES x TWO PRICES => 8 distributions:
#     samples: "ets"  = EU ETS firms only  (emissions z = ETS-observed, source=="ets")
#              "full" = everyone           (all emitters, z = ETS + GLO-imputed)
#     prices : p_z in {80, 250} EUR/tCO2
#   Laid out as 4 PNGs (sector|firm x price), each overlaying ETS vs full.
#
# INPUT  {nbb}/processed/{annual_accounts_selected_sample, firm_year_domestic_input_cost,
#        firm_year_total_imports, firm_year_belgian_euets, deployment_panel}.RData
#        {nbb}/processed/allocation_glo_balanced/alloc_YYYY.RData  (vat, scope1, source)
# OUTPUT {output_dir}/figures/emission_cost_share_{sector,firm}_p{80,250}.png
#        {output_dir}/emission_cost_share_summary.csv
# RUNS ON: local 1 (downsample) or RMD (full)
###############################################################################

rm(list = ls())
suppressPackageStartupMessages({ library(dplyr) })

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

PRICES <- c(80, 250)        # EUR/tCO2 (paper benchmark + high counterfactual)
MIN_N  <- 5                 # min firm-years per sector to report a sector mean
ALLOC_DIR <- file.path(proc_data, "allocation_glo_balanced")
cat("== emission_cost_share ==  year-demeaned log share; prices", paste(PRICES, collapse="/"), "\n")

# ---- Cost base (model_assembly.R:41-64): IC 60/61 + wages; fallback dom+imp+wages ----
load(file.path(proc_data, "annual_accounts_selected_sample.RData"))
load(file.path(proc_data, "firm_year_domestic_input_cost.RData"))
load(file.path(proc_data, "firm_year_total_imports.RData"))
load(file.path(proc_data, "firm_year_belgian_euets.RData"))
load(file.path(proc_data, "deployment_panel.RData"))

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

cost_base_tab <- aa %>%
  full_join(dom, by = c("vat","year")) %>% full_join(imp, by = c("vat","year")) %>%
  full_join(wge, by = c("vat","year")) %>%
  mutate(wage = coalesce(wage_aa, wage_ets, 0), dom = coalesce(dom, 0), imp = coalesce(imp, 0),
         use_ic = !is.na(ic) & ic > 0,
         total_cost = ifelse(use_ic, ic + wage, dom + imp + wage)) %>%
  filter(total_cost > 0) %>% select(vat, year, total_cost, use_ic)

# ---- Emissions per (vat, year): z_all (full) and z_ets (ETS only) ----
zL <- list()
for (f in sort(list.files(ALLOC_DIR, pattern = "^alloc_\\d+\\.RData$", full.names = TRUE))) {
  load(f); yf <- year_firms
  if (!"year" %in% names(yf)) yf$year <- as.integer(sub(".*alloc_(\\d+)\\.RData$", "\\1", f))
  zL[[length(zL)+1L]] <- yf %>% filter(scope1 > 0) %>% group_by(vat, year) %>%
    summarise(z_all = sum(scope1), z_ets = sum(scope1[source == "ets"]), .groups = "drop")
}
dat <- bind_rows(zL) %>% inner_join(cost_base_tab, by = c("vat","year")) %>%
  inner_join(nace, by = c("vat","year"))
cat(sprintf("  %d emitter firm-years with a model cost base over %s (%.0f%% use IC 60/61)\n",
            nrow(dat), paste(range(dat$year), collapse = "-"), 100*mean(dat$use_ic)))

# year-demeaned log share -> within-sector mean (per sector) and within-firm mean (per firm)
stats_for <- function(zcol, price) {
  d <- dat[dat[[zcol]] > 0, c("vat","year","nace4d","total_cost",zcol)]
  s  <- price * d[[zcol]] / (d$total_cost + price * d[[zcol]])
  d$lsd <- ave(log(s), d$year, FUN = function(v) v - mean(v))   # year-demeaned log share
  sec  <- aggregate(lsd ~ nace4d, d, mean); secN <- table(d$nace4d)
  sec  <- sec[secN[as.character(sec$nace4d)] >= MIN_N, ]
  frm  <- aggregate(lsd ~ vat, d, mean)
  list(sector = sec$lsd, firm = frm$lsd, share_pct = 100 * s,
       n_fy = nrow(d), n_sec = nrow(sec), n_frm = nrow(frm))
}

col_ets <- "firebrick"; col_full <- "grey20"
# Large fonts for axis titles (cex.lab) and tick labels (cex.axis); no plot title.
plot_pair <- function(a, b, xlab, file) {  # a = ETS, b = full (each a numeric vector)
  da <- density(a); db <- density(b)
  png(file.path(fig_dir, file), width = 6.6, height = 4.6, units = "in", res = 150)
  op <- par(mar = c(5.2, 5.2, 0.6, 0.8), mgp = c(3.2, 1.0, 0),
            cex.lab = 1.8, cex.axis = 1.6)
  plot(db, main = "", xlim = range(c(da$x, db$x)), ylim = c(0, max(c(da$y, db$y))),
       col = col_full, lwd = 2.5, xlab = xlab, ylab = "Density")
  lines(da, col = col_ets, lwd = 2.5, lty = 2); abline(v = 0, col = "grey60", lty = 3)
  legend("topright", c("EU ETS only", "Everyone"), col = c(col_ets, col_full),
         lwd = 2.5, lty = c(2, 1), bty = "n", cex = 1.4); par(op); dev.off()
}

# level (raw share %) summary -- answers "are emissions a HIGH share of input costs?"
lvl_rows <- list()
disp_rows <- list()
for (price in PRICES) {
  ets  <- stats_for("z_ets", price); full <- stats_for("z_all", price)
  plot_pair(ets$sector, full$sector, "Year-demeaned log carbon-cost share",
            sprintf("emission_cost_share_sector_p%d.png", price))
  plot_pair(ets$firm, full$firm, "Year-demeaned log carbon-cost share",
            sprintf("emission_cost_share_firm_p%d.png", price))
  for (nm in c("ets","full")) {
    o <- get(nm)
    ql <- quantile(o$share_pct, c(.50,.90,.99), na.rm = TRUE)
    lvl_rows[[length(lvl_rows)+1L]] <- data.frame(price = price, sample = nm,
      n_firmyears = length(o$share_pct), share_p50 = ql[1], share_p90 = ql[2],
      share_p99 = ql[3], pct_gt5 = 100*mean(o$share_pct > 5), pct_gt20 = 100*mean(o$share_pct > 20))
    for (lv in c("sector","firm")) {
      v <- o[[lv]]; q <- quantile(v, c(.10,.50,.90), na.rm = TRUE)
      disp_rows[[length(disp_rows)+1L]] <- data.frame(price = price, sample = nm, level = lv,
        n = length(v), p10 = q[1], p50 = q[2], p90 = q[3], sd = sd(v))
    }
  }
  cat(sprintf("  p_z=%d : ETS %d sectors / %d firms ; full %d sectors / %d firms (from %d / %d firm-years)\n",
              price, ets$n_sec, ets$n_frm, full$n_sec, full$n_frm, ets$n_fy, full$n_fy))
}
write.csv(do.call(rbind, disp_rows), file.path(output_dir, "emission_cost_share_summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, lvl_rows),  file.path(output_dir, "emission_cost_share_levels.csv"),  row.names = FALSE)
cat("== raw share LEVELS (%, answers 'is it a high share?') ==\n"); print(do.call(rbind, lvl_rows), row.names = FALSE)
cat("Done. 4 figures (8 distributions) + summary + levels in", output_dir, "\n")
