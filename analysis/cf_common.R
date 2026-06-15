###############################################################################
# cf_common.R  —  shared setup for the counterfactual runners.
#
# Sourced by run_T0.R / run_T1.R / run_T2.R (single scheme) and
# run_counterfactuals_rmd.R (all schemes). Provides:
#   - paths, solver, assembled bundle (CACHED), base demand shares
#   - the parameter cells (sigma swept; alpha, rho external -- Option B)
#   - get_scheme_tau(scheme), run_scheme(scheme)
# Edit the CONFIG block here once; every runner picks it up.
###############################################################################
suppressMessages({ library(dplyr); library(stringr); library(Matrix) })

.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1]); CODE_DIR <- file.path(project_root, "analysis")
source(file.path(CODE_DIR, "model_assembly.R"))
source(file.path(CODE_DIR, "phase5_model_solver.R"))

# ================================ CONFIG ===================================
YEAR  <- 2019
SCOPE <- "ets_neighbors"               # "ets_neighbors" (clean+tractable) | "full"
# Nested-CES baseline calibration (quantitative.tex 5.2): sigma_B between sectors
# (Atalay, near-Leontief across-sector), sigma_W within sector across suppliers
# (Peter-Ruane, long-run point est), alpha (Martinsson all-firm), rho (Ganapati
# energy-cost pass-through). Headline is the SINGLE baseline cell; robustness
# grids (FULL_GRID) hold sigma_B fixed and sweep sigma_W / rho / alpha.
SIGMA_B <- 0.1                          # between-sector elasticity (fixed)
SIGMA_W <- 2.5                          # within-sector elasticity (baseline)
DEF_RHO <- 0.7                          # pass-through (baseline)
DEF_ALPHA <- 2                          # abatement elasticity (baseline)
SIGMA_W_GRID <- c(0.55, 1, 2.5, 3.4)   # Fujiy 0.55 (SR), CD 1, Peter 2.5 (LR), Huneeus 3.4 (LR)
RHO_GRID   <- c(0.5, 0.7, 1)           # Amiti large-firm; Ganapati energy-cost; full (PASS_THROUGH_LIT.md §6)
ALPHA_GRID <- c(2, 4)                  # Martinsson all-firm / top-emitter
PRICE_GRID <- c(80, 100, 150, 200, 250)            # benchmark 80; rest counterfactual
PATH_STEP  <- 10                       # EUR/tCO2 path-integral step
FULL_GRID  <- FALSE                    # FALSE: single baseline cell. TRUE: sweep sigma_W, rho, alpha
# ===========================================================================

# Model outputs (cf_*.csv) and the bundle cache go to output_dir, which paths.R
# splits by machine (output_rmd vs output_local) -- so an RMD run and a local run
# never collide on the same git path. (out_data = data/processed is the SHARED
# input dir; writing results there made RMD/local outputs clobber each other.)

# ---- assembled bundle (cached; delete the file to force re-assembly) ----
bundle_file <- file.path(output_dir, sprintf("model_inputs_%d_%s.RData", YEAR, SCOPE))
if (file.exists(bundle_file)) {
  cat("Loading cached bundle:", basename(bundle_file), "\n"); load(bundle_file)
} else {
  cat("Assembling bundle (first run; cached for reuse) ...\n")
  bundle <- assemble_bundle(YEAR, SCOPE, proc_data, out_data); save(bundle, file = bundle_file)
}
bundle <- build_nest(bundle)            # nested-CES precompute (not persisted in the cache)
bshare <- base_final_shares(bundle)
cat(sprintf("  %d firms, %d ETS, max rowsum=%.4f\n",
            bundle$meta$n, bundle$meta$n_ets, bundle$meta$max_rowsum))

# ---- parameter cells (single baseline; FULL_GRID adds robustness sweeps) ----
cells <- data.frame(sigma_B = SIGMA_B, sigma_W = SIGMA_W, rho = DEF_RHO, alpha = DEF_ALPHA)
if (FULL_GRID) cells <- unique(rbind(cells,
  data.frame(sigma_B = SIGMA_B, sigma_W = SIGMA_W_GRID, rho = DEF_RHO,  alpha = DEF_ALPHA),
  data.frame(sigma_B = SIGMA_B, sigma_W = SIGMA_W,      rho = RHO_GRID, alpha = DEF_ALPHA),
  data.frame(sigma_B = SIGMA_B, sigma_W = SIGMA_W,      rho = DEF_RHO,  alpha = ALPHA_GRID)))
path_grid <- sort(unique(c(seq(0, max(PRICE_GRID), by = PATH_STEP), PRICE_GRID)))

# ---- targeting vector for a scheme ----
#   "actual_ets"           = the realized EU ETS installations (benchmark)
#   "universal_industrial" = every industrial emitter (NACE 05-09, 10-33, 35)
#   "centrality"           = most-central firms, emission coverage matched to ETS
get_scheme_tau <- function(scheme) {
  if (scheme == "centrality") {                           # needs phase6_centrality.R output
    f <- file.path(output_dir, "cf_centrality.csv")
    if (!file.exists(f)) stop("cf_centrality.csv not found - run phase6_centrality.R first (for centrality)")
    cf <- read.csv(f); ord <- order(cf$total)             # most-reducing first
    nsel <- which(cumsum(cf$z[ord]) >= sum(cf$z[cf$ets == 1]))[1]
    if (is.na(nsel)) nsel <- length(ord)
    return(as.integer(bundle$firms %in% cf$vat[ord][seq_len(nsel)]))
  }
  get_tau(scheme, bundle)                                 # actual_ets, universal_industrial
}

# ---- run one scheme across cells x prices; returns a data.frame ----
run_scheme <- function(scheme) {
  tau <- get_scheme_tau(scheme)
  cat(sprintf("\nScheme %s: %d targeted firms, %d cell(s) [path-integral decomposition]\n",
              scheme, sum(tau), nrow(cells)))
  out <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    sB <- cells$sigma_B[i]; sW <- cells$sigma_W[i]; r <- cells$rho[i]; a <- cells$alpha[i]
    cat(sprintf("  cell %d/%d: sigma_B=%.3f sigma_W=%.3f rho=%.2f alpha=%.2f ...\n",
                i, nrow(cells), sB, sW, r, a))
    bsch <- bundle; bsch$tau <- tau
    dec <- decompose_path_grid(path_grid, sB, sW, r, a, bsch, bshare)
    dec <- dec[dec$p_z %in% PRICE_GRID, ]
    out[[i]] <- data.frame(scheme = scheme, p_z = dec$p_z, sigma_B = sB, sigma_W = sW,
                           rho = r, alpha = a, dlogZ = dec$dlogZ, scale = dec$scale,
                           technique = dec$technique, composition = dec$composition,
                           realGDP = dec$realGDP, mean_price = dec$mean_price)
    gc()
  }
  bind_rows(out)
}
