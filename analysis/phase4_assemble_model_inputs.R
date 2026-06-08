###############################################################################
# phase4_assemble_model_inputs.R
#
# PURPOSE:
#   Assemble the structural model's input bundle for one base-year cross-section,
#   reusing the network-construction logic from phase2. This is P3 of the
#   counterfactual plan (see COUNTERFACTUALS.md).
#
#   The bundle is what the global CES solver consumes:
#     - Omega : firm-to-firm cost-share matrix  Omega[i,j] = (i's spend on j)/(i's total cost)
#     - e_bar : no-policy emission intensity     e_bar_i = z_i / total_cost_i
#     - gamma : residual (labor + outside) cost share = 1 - rowSums(Omega)
#     - x      : firm size (deflated revenue), for Domar / aggregation weights
#     - tau    : EU ETS targeting vector (1 if firm is an ETS installation)
#     - nu     : network-adjusted emission intensity = (I-Omega)^{-1} e_bar
#
# DESIGN CHOICES (see COUNTERFACTUALS.md, P3/P4):
#   - Base year:  2019 (recent, representative; pass YEAR to change).
#   - Scope:      tractable subset = ETS firms + their direct B2B partners.
#                 Final paper runs full panel on RMD; this is the dev subset.
#   - Cost base:  NO-POLICY -> carbon costs are EXCLUDED from total cost.
#   - Closed economy (model assumption): total cost = domestic B2B inputs + labor.
#                 Imports are dropped (no import-leakage channel in this model).
#   - Subset embedding: each firm's total cost uses its FULL B2B purchases (incl.
#                 suppliers outside the subset); within-subset edges define Omega,
#                 so each row's residual gamma_i absorbs labor + outside-subset
#                 inputs. (Keeps the subset embedded in the real economy.)
#
# DATA (sibling NBB_data/processed):
#   - b2b_selected_sample.RData           (df_b2b_selected_sample)
#   - firm_year_belgian_euets.RData       (firm_year_belgian_euets)
#   - annual_accounts_selected_sample_key_variables.RData
#   - deployment_panel.RData              (deployment_panel)
#   - allocation_glo_balanced/alloc_<YEAR>.RData  (year_firms: vat, scope1, source)
#   - deflator_nace4d_2005base.RData      (deflator, deflator_2d_only)
#
# OUTPUT:
#   - data/processed/model_inputs_<YEAR>_subset.RData  (list `model_inputs`)
###############################################################################

rm(list = ls())

suppressMessages({
  library(dplyr)
  library(stringr)
  library(Matrix)
})

# ---- Parameters ----
YEAR  <- 2019           # base-year cross-section
SCOPE <- "ets_neighbors" # "ets_neighbors" (dev subset) or "full" (RMD)

nbb_data     <- "c:/Users/jota_/Documents/NBB_data"
project_root <- "c:/Users/jota_/Documents/carbon_policy_networks"
proc_data    <- file.path(nbb_data, "processed")
out_data     <- file.path(project_root, "data", "processed")
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)

# ---- Neumann series: (I - A)^{-1} v = v + Av + A^2 v + ... ----
neumann_series <- function(A, v, max_iter = 100, tol = 1e-9) {
  result <- v
  current <- as.numeric(A %*% v)
  for (k in seq_len(max_iter)) {
    result <- result + current
    current <- as.numeric(A %*% current)
    rel_err <- max(abs(current)) / (max(abs(result)) + 1e-15)
    if (rel_err < tol) return(list(result = result, k = k, rel_err = rel_err, converged = TRUE))
  }
  list(result = result, k = max_iter, rel_err = rel_err, converged = FALSE)
}

cat(sprintf("Assembling model inputs: YEAR=%d, SCOPE=%s\n", YEAR, SCOPE))

# ---- Load data ----
load(file.path(proc_data, "b2b_selected_sample.RData"))
load(file.path(proc_data, "firm_year_belgian_euets.RData"))
load(file.path(proc_data, "annual_accounts_selected_sample_key_variables.RData"))
load(file.path(proc_data, "deployment_panel.RData"))
load(file.path(proc_data, "deflator_nace4d_2005base.RData"))

aa <- df_annual_accounts_selected_sample_key_variables
colnames(df_b2b_selected_sample)[1:4] <- c("vat_supplier", "vat_buyer", "year", "corr_sales")

# Emissions: REUSE the descriptive emissions panel (no recompute). ev_s2 already
# holds firm-level emissions z (ETS observed + imputed non-ETS) for 2005-2021.
load(file.path(out_data, "emissions_vectors_s2.RData"))  # provides `ev_s2`
emis <- ev_s2 %>% filter(year == YEAR) %>% select(vat, scope1 = z, source)

# ---- B2B for the base year ----
b2b <- df_b2b_selected_sample %>% filter(year == YEAR, corr_sales > 0)
cat(sprintf("B2B edges in %d: %d\n", YEAR, nrow(b2b)))

# ---- ETS firms this year (the targeted set) ----
ets_vat <- firm_year_belgian_euets %>%
  filter(year == YEAR, in_sample == 1, !is.na(vat)) %>%
  distinct(vat) %>% pull(vat)
cat(sprintf("ETS firms (in_sample) in %d: %d\n", YEAR, length(ets_vat)))

# ---- Define the firm universe ----
if (SCOPE == "ets_neighbors") {
  touch <- b2b %>% filter(vat_supplier %in% ets_vat | vat_buyer %in% ets_vat)
  subset_vat <- unique(c(ets_vat, touch$vat_supplier, touch$vat_buyer))
} else {
  subset_vat <- unique(c(b2b$vat_supplier, b2b$vat_buyer))
}
subset_vat <- subset_vat[!is.na(subset_vat)]
n <- length(subset_vat)
idx <- setNames(seq_len(n), subset_vat)
cat(sprintf("Firm universe (%s): %d firms\n", SCOPE, n))

# ---- Total cost denominator: FULL B2B purchases + labor (no carbon) ----
# Full input purchases per buyer (over ALL suppliers, not just subset)
full_inputs <- b2b %>% group_by(vat_buyer) %>%
  summarise(inputs = sum(corr_sales), .groups = "drop")

# Wage bill: prefer annual accounts, fall back to euets
wages_aa <- aa %>% filter(year == YEAR) %>% select(vat, wage_bill) %>%
  filter(!is.na(wage_bill))
wages_ets <- firm_year_belgian_euets %>% filter(year == YEAR, !is.na(wage_bill)) %>%
  select(vat, wage_bill_ets = wage_bill)
wages <- tibble(vat = subset_vat) %>%
  left_join(wages_aa, by = "vat") %>%
  left_join(wages_ets, by = "vat") %>%
  mutate(wage_bill = coalesce(wage_bill, wage_bill_ets, 0)) %>%
  select(vat, wage_bill)

firm_tab <- tibble(vat = subset_vat) %>%
  left_join(full_inputs, by = c("vat" = "vat_buyer")) %>%
  mutate(inputs = coalesce(inputs, 0)) %>%
  left_join(wages, by = "vat") %>%
  mutate(total_cost = inputs + wage_bill)

total_cost <- firm_tab$total_cost
names(total_cost) <- firm_tab$vat
total_cost[total_cost <= 0] <- NA_real_

# ---- Build Omega on within-subset edges ----
# NOTE (redundancy): phase2 already saves a cost-share matrix `A` in
# io_matrices_by_year.RData, but it normalizes by total cost INCLUDING the carbon
# bill (emissions x EUA price), which mis-states Omega for heavy emitters at the
# no-policy point. We deliberately rebuild Omega here with a NO-CARBON cost base.
# Proper fix (deferred, see COUNTERFACTUALS.md): one shared network builder that
# saves the raw expenditure matrix + cost components, so descriptive and
# counterfactual code each re-normalize from the same raw ingredients.
b2b_sub <- b2b %>% filter(vat_supplier %in% subset_vat, vat_buyer %in% subset_vat)
exp_mat <- sparseMatrix(
  i = idx[b2b_sub$vat_buyer],
  j = idx[b2b_sub$vat_supplier],
  x = b2b_sub$corr_sales,
  dims = c(n, n)
)

Omega <- exp_mat
row_cost <- total_cost[Omega@i + 1L]          # @i is 0-indexed row (buyer)
ok <- !is.na(row_cost) & row_cost > 0
Omega@x[ok]  <- Omega@x[ok] / row_cost[ok]
Omega@x[!ok] <- 0

rowsum_O <- rowSums(Omega)
cat(sprintf("Omega: max row sum = %.4f (should be < 1)\n", max(rowsum_O, na.rm = TRUE)))
# Safety cap (should rarely bind, since denominator is FULL cost)
if (max(rowsum_O, na.rm = TRUE) >= 1) {
  bad <- which(rowsum_O >= 1)
  for (r in bad) {
    a <- Omega@p[r] + 1L; b <- Omega@p[r + 1L]
    if (b >= a) Omega@x[a:b] <- Omega@x[a:b] * 0.99 / rowsum_O[r]
  }
  rowsum_O <- rowSums(Omega)
  cat(sprintf("  capped -> max row sum = %.4f\n", max(rowsum_O)))
}

# residual cost share (labor + outside-subset inputs)
gamma <- 1 - rowsum_O

# ---- Emission intensity e_bar = z / total_cost (no-policy base) ----
z <- setNames(rep(0, n), subset_vat)
em <- emis %>% filter(vat %in% subset_vat)
z[em$vat] <- em$scope1
e_bar <- as.numeric(z / total_cost)
e_bar[!is.finite(e_bar)] <- 0
names(e_bar) <- subset_vat

# ---- Targeting vector ----
tau <- as.integer(subset_vat %in% ets_vat)

# ---- Firm size: deflated revenue ----
rev_tab <- bind_rows(
  firm_year_belgian_euets %>% filter(year == YEAR, in_sample == 1) %>%
    select(vat, revenue, nace5d),
  deployment_panel %>% filter(year == YEAR) %>% select(vat, revenue, nace5d)
) %>% distinct(vat, .keep_all = TRUE) %>%
  filter(vat %in% subset_vat) %>%
  mutate(nace4d = str_sub(nace5d, 1, 4), nace2d = str_sub(nace5d, 1, 2))

rev_tab <- rev_tab %>%
  left_join(deflator %>% select(nace4d, year, ppi) %>% filter(year == YEAR),
            by = "nace4d") %>%
  left_join(deflator_2d_only %>% select(nace2d, year, ppi_2d = ppi) %>% filter(year == YEAR),
            by = "nace2d") %>%
  mutate(ppi = coalesce(ppi, ppi_2d),
         real_revenue = ifelse(!is.na(ppi) & ppi > 0, revenue / ppi * 100, revenue))

x <- setNames(rep(NA_real_, n), subset_vat)
x[rev_tab$vat] <- rev_tab$real_revenue

# ---- Network-adjusted emission intensity nu = (I-Omega)^{-1} e_bar ----
nu_res <- neumann_series(Omega, e_bar)
cat(sprintf("nu: converged=%s, k=%d, rel_err=%.2e\n",
            nu_res$converged, nu_res$k, nu_res$rel_err))

# ---- Assemble + save ----
model_inputs <- list(
  meta = list(year = YEAR, scope = SCOPE, n_firms = n,
              n_ets = sum(tau), max_rowsum_Omega = max(rowSums(Omega), na.rm = TRUE),
              nu_converged = nu_res$converged, nu_k = nu_res$k,
              cost_base = "no-carbon, closed (inputs+labor)"),
  firms = subset_vat,
  Omega = Omega,        # sparse, rows = buyers, cols = suppliers
  e_bar = e_bar,        # no-policy emission intensity (z / total_cost)
  gamma = gamma,        # residual (labor + outside) cost share
  tau   = tau,          # ETS targeting
  x     = x,            # deflated revenue (size)
  z     = z,            # raw emissions (tonnes)
  total_cost = total_cost,
  nu    = nu_res$result # network-adjusted emission intensity
)

# ---- Diagnostics ----
cat("\n=== Diagnostics ===\n")
cat(sprintf("Firms: %d  | ETS: %d  | with emissions>0: %d\n",
            n, sum(tau), sum(z > 0)))
cat(sprintf("Firms missing total cost: %d | missing size(x): %d\n",
            sum(is.na(total_cost)), sum(is.na(x))))
cat(sprintf("e_bar: mean=%.3g median=%.3g max=%.3g (per EUR cost)\n",
            mean(e_bar), median(e_bar), max(e_bar)))
cat(sprintf("gamma (resid cost share): mean=%.3f min=%.3f\n",
            mean(gamma, na.rm = TRUE), min(gamma, na.rm = TRUE)))
cat(sprintf("Total emissions in subset: %.3g tCO2 | ETS share of it: %.1f%%\n",
            sum(z), 100 * sum(z[tau == 1]) / sum(z)))

out_fn <- file.path(out_data, sprintf("model_inputs_%d_subset.RData", YEAR))
save(model_inputs, file = out_fn)
cat(sprintf("\nSaved: %s\n", out_fn))
cat("Done.\n")
