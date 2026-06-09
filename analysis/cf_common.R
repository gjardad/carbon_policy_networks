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
SIGMA_GRID <- c(0.034, 0.5, 1, 4.7)    # swept (headline)
RHO_GRID   <- c(0.5, 0.75, 1)          # AIK realistic + full-passthrough
ALPHA_GRID <- c(2, 4)                  # Martinsson all-firm / top-emitter
DEF_SIGMA  <- 0.5; DEF_RHO <- 0.5; DEF_ALPHA <- 4   # external (Option B)
PRICE_GRID <- c(80, 100, 150, 200, 250)            # benchmark 80; rest counterfactual
PATH_STEP  <- 10                       # EUR/tCO2 path-integral step
FULL_GRID  <- FALSE                    # FALSE: sweep sigma. TRUE: also sweep rho, alpha
# ===========================================================================

# ---- assembled bundle (cached; delete the file to force re-assembly) ----
bundle_file <- file.path(out_data, sprintf("model_inputs_%d_%s.RData", YEAR, SCOPE))
if (file.exists(bundle_file)) {
  cat("Loading cached bundle:", basename(bundle_file), "\n"); load(bundle_file)
} else {
  cat("Assembling bundle (first run; cached for reuse) ...\n")
  bundle <- assemble_bundle(YEAR, SCOPE, proc_data, out_data); save(bundle, file = bundle_file)
}
bshare <- base_final_shares(bundle)
cat(sprintf("  %d firms, %d ETS, max rowsum=%.4f\n",
            bundle$meta$n, bundle$meta$n_ets, bundle$meta$max_rowsum))

# ---- parameter cells (sweep sigma; alpha, rho external) ----
cells <- data.frame(sigma = SIGMA_GRID, rho = DEF_RHO, alpha = DEF_ALPHA)
if (FULL_GRID) cells <- unique(rbind(cells,
  data.frame(sigma = DEF_SIGMA, rho = RHO_GRID, alpha = DEF_ALPHA),
  data.frame(sigma = DEF_SIGMA, rho = DEF_RHO,  alpha = ALPHA_GRID)))
path_grid <- sort(unique(c(seq(0, max(PRICE_GRID), by = PATH_STEP), PRICE_GRID)))

# ---- targeting vector for a scheme ----
get_scheme_tau <- function(scheme) {
  if (scheme == "T2") {                                   # centrality (needs phase6 output)
    f <- file.path(out_data, "cf_centrality.csv")
    if (!file.exists(f)) stop("cf_centrality.csv not found - run phase6_centrality.R first (for T2)")
    cf <- read.csv(f); ord <- order(cf$total)             # most-reducing first
    nsel <- which(cumsum(cf$z[ord]) >= sum(cf$z[cf$ets == 1]))[1]
    if (is.na(nsel)) nsel <- length(ord)
    return(as.integer(bundle$firms %in% cf$vat[ord][seq_len(nsel)]))
  }
  get_tau(scheme, bundle)                                 # T0 (ETS), T1 (industrial)
}

# ---- run one scheme across cells x prices; returns a data.frame ----
run_scheme <- function(scheme) {
  tau <- get_scheme_tau(scheme)
  cat(sprintf("\nScheme %s: %d targeted firms, %d cell(s) [path-integral decomposition]\n",
              scheme, sum(tau), nrow(cells)))
  out <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    s <- cells$sigma[i]; r <- cells$rho[i]; a <- cells$alpha[i]
    cat(sprintf("  cell %d/%d: sigma=%.3f rho=%.2f alpha=%.2f ...\n", i, nrow(cells), s, r, a))
    bsch <- bundle; bsch$tau <- tau
    dec <- decompose_path_grid(path_grid, s, r, a, bsch, bshare)
    dec <- dec[dec$p_z %in% PRICE_GRID, ]
    out[[i]] <- data.frame(scheme = scheme, p_z = dec$p_z, sigma = s, rho = r, alpha = a,
                           dlogZ = dec$dlogZ, scale = dec$scale, technique = dec$technique,
                           composition = dec$composition, realGDP = dec$realGDP,
                           mean_price = dec$mean_price)
    gc()
  }
  bind_rows(out)
}
