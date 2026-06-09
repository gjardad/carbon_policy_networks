###############################################################################
# phase5_model_solver.R  —  Global CES solver, Piece 1: abatement + prices
#
# Solves the firm side of the model on an assembled bundle (phase4):
#   (1) Abatement block: closed-form kappa_i*, e_i* given (p_z, tau, alpha, ebar).
#   (2) Price fixed point: p_i = c_i^input(p) + kappa_i* + rho * tau_i * p_z * e_i*.
#
# Model (sections/model.tex):
#   - Production: Cobb-Douglas(labor share gamma_i, materials share 1-gamma_i),
#     CES(elasticity sigma) across materials. Labor is numeraire (w=1).
#   - Calibrated so base prices = 1: c_i^input = P_{m,i}^{1-gamma_i},
#     P_{m,i} = (sum_j theta_ij p_j^{1-sigma})^{1/(1-sigma)}, theta = row-normalized Omega.
#   - Abatement: e_i = (1-kappa_i)^{alpha} ebar_i, linear cost kappa_i, ALPHA>1
#     (convex problem). FOC interior when tau_i p_z ebar_i alpha >= 1, else kappa=0.
#   - Pass-through rho on the carbon wedge (COUNTERFACTUALS.md): only rho of the
#     carbon bill enters the price.
#
# NOTE: quantities / aggregate emissions need the demand block (Piece 2), which
# requires the household-demand spec — not yet built here.
###############################################################################

suppressMessages({ library(Matrix); library(dplyr) })

proj <- "c:/Users/jota_/Documents/carbon_policy_networks"

load_bundle <- function(year = 2019) {
  f <- file.path(proj, "data/processed", sprintf("model_inputs_%d_subset.RData", year))
  load(f); model_inputs
}

# ---- Abatement block: closed form (alpha > 1) ------------------------------
# minimize over kappa>=0 of:  kappa + tau p_z ebar (1-kappa)^alpha
# FOC (interior): tau p_z ebar alpha (1-kappa)^{alpha-1} = 1
#   => 1-kappa = (tau p_z ebar alpha)^{1/(1-alpha)}   [<=1 iff tau p_z ebar alpha >= 1]
# Corner kappa=0 when tau p_z ebar alpha < 1 (carbon bill too small to abate).
abatement_block <- function(p_z, tau, alpha, ebar) {
  stopifnot(alpha > 1)
  drive <- tau * p_z * ebar * alpha          # >=1 => interior
  one_minus_k <- ifelse(drive >= 1, drive^(1 / (1 - alpha)), 1)  # in (0,1]
  kappa <- 1 - one_minus_k
  e <- ebar * one_minus_k^alpha              # = ebar at corner; falls when interior
  list(kappa = kappa, e = e)
}

# ---- Anderson-accelerated fixed point: solve x = G(x) -----------------------
# Depth-m Anderson acceleration. ~5-10x fewer iterations than plain Picard with
# negligible overhead (a tiny n x m least squares). Falls back to a Picard step
# if the least squares is rank-deficient. Used for BOTH the price and the
# quantity (Leontief) systems, since each is a contraction.
anderson_fp <- function(G, x0, tol = 1e-8, maxit = 200, m = 5, floor = NULL) {
  n <- length(x0)
  x <- x0; g <- G(x); f <- g - x
  delta <- max(abs(f))
  if (delta < tol) return(list(x = x, iters = 0L, delta = delta, converged = TRUE))
  x_prev <- x; f_prev <- f; x <- g                 # first step = plain Picard
  # Preallocated ring-buffer history (avoid cbind reallocation each iter).
  Xp <- matrix(0, n, m); Ff <- matrix(0, n, m); ns <- 0L; col <- 0L
  for (it in seq_len(maxit)) {
    g <- G(x); f <- g - x
    delta <- max(abs(f))
    if (delta < tol) break
    col <- (col %% m) + 1L
    Xp[, col] <- x - x_prev; Ff[, col] <- f - f_prev
    ns <- min(ns + 1L, m); x_prev <- x; f_prev <- f
    if (ns == m) { Xs <- Xp; Fs <- Ff }            # no copy in steady state
    else { Xs <- Xp[, seq_len(ns), drop = FALSE]; Fs <- Ff[, seq_len(ns), drop = FALSE] }
    # least squares argmin||f - Fs gma|| via the m x m normal equations (tiny solve),
    # not a QR of the n x ns matrix; regularize for stability near convergence.
    gma <- tryCatch({
      A <- crossprod(Fs)
      as.numeric(solve(A + (1e-10 * mean(diag(A))) * diag(ns), crossprod(Fs, f)))
    }, error = function(e) rep(0, ns))
    x <- as.numeric(x + f - Xs %*% gma - Fs %*% gma)   # no (Xs+Fs) temp array
    if (!is.null(floor)) x[x < floor] <- floor
  }
  list(x = x, iters = it, delta = delta, converged = delta < tol)
}

# ---- Price fixed point ------------------------------------------------------
# Profiled as ~70% of solve time, dominated by the iteration COUNT, so we
# accelerate it (big-O > constant factors):
#   - Anderson acceleration (depth m): ~30-50 iters vs ~360 for plain Picard,
#     negligible per-iter overhead (a tiny n x m least squares). Falls back to a
#     Picard step if the least squares is rank-deficient.
#   - Precompute the invariant exponent vector / additive term (avoid redundant work).
solve_prices <- function(p_z, sigma, rho, alpha, bundle,
                         maxit = 200, tol = 1e-8, m = 5, verbose = FALSE) {
  Omega <- bundle$Omega; gamma <- bundle$gamma; tau <- bundle$tau; ebar <- bundle$e_bar
  n <- length(gamma)
  mat_share <- rowSums(Omega); pos <- mat_share > 0
  theta <- Omega; theta@x <- theta@x / mat_share[theta@i + 1L]   # row-normalized (invariant)
  cd <- abs(sigma - 1) < 1e-8
  expo <- (1 - gamma) / (1 - sigma)                              # invariant per call
  ab <- abatement_block(p_z, tau, alpha, ebar)
  a_term <- ab$kappa + rho * tau * p_z * ab$e                    # invariant additive term

  Gmap <- function(p) {
    ci <- if (cd) exp((1 - gamma) * as.numeric(theta %*% log(p)))
          else    as.numeric(theta %*% (p^(1 - sigma)))^expo
    ci[!pos] <- 1
    ci + a_term
  }

  r <- anderson_fp(Gmap, rep(1, n), tol = tol, maxit = maxit, m = m, floor = 1e-12)
  if (verbose) cat(sprintf("  price solve: it=%d delta=%.2e\n", r$iters, r$delta))
  list(p = r$x, kappa = ab$kappa, e = ab$e, iters = r$iters, delta = r$delta, converged = r$converged)
}

# ---- Final-demand shares b_i (Cobb-Douglas), calibrated from base sales -----
# base sales s = revenue (bundle$x); intermediate sales of i = sum_j Omega_ji s_j
# = (t(Omega) %*% s)_i; final demand_i = s_i - intermediate_i.
base_final_shares <- function(bundle) {
  s <- bundle$x; s[is.na(s)] <- 0
  inter <- as.numeric(Matrix::crossprod(bundle$Omega, s))   # t(Omega) %*% s
  final <- pmax(s - inter, 0)
  final / sum(final)
}

# ---- Quantity / market-clearing block (Piece 2) ----------------------------
# Given solved prices, compute nominal sales s, real output x, emissions Z.
#   s = (I - M')^{-1} b E,  M_{ji} = phi_ji * c_input_j / p_j  (j's spend on i / revenue_j)
#   phi_ji = equilibrium expenditure share of j on input i (CES, CD-labor nest)
#   E pinned by labor market: L = sum_j gamma_j (c_input_j/p_j) s_j = 1
solve_quantities <- function(sol, p_z, sigma, rho, bundle, b) {
  Omega <- bundle$Omega; gamma <- bundle$gamma; tau <- bundle$tau
  n <- length(gamma); p <- sol$p; e <- sol$e
  c_input <- p - sol$kappa - rho * tau * p_z * e        # input+labor unit cost

  mat_share <- rowSums(Omega)
  theta <- Omega; theta@x <- theta@x / mat_share[theta@i + 1L]

  if (abs(sigma - 1) < 1e-8) {
    theta_norm <- theta                                  # CD: shares constant
  } else {
    tw <- theta %*% Diagonal(x = p^(1 - sigma))          # scale col i by p_i^{1-sigma}
    rs <- rowSums(tw); rs[rs <= 0] <- 1
    theta_norm <- Diagonal(x = 1 / rs) %*% tw            # rows sum to 1
  }
  phi <- Diagonal(x = mat_share) %*% theta_norm          # row j sums to (1-gamma_j)
  M   <- Diagonal(x = c_input / p) %*% phi               # M_ji = phi_ji c_input_j/p_j

  # s1 solves s = b + M' s (E=1). M' is sub-stochastic (contraction) -> iterate
  # with Anderson instead of a direct sparse LU (matvecs scale better with n).
  Mt <- as(t(M), "CsparseMatrix")
  s1 <- anderson_fp(function(s) b + as.numeric(Mt %*% s), b, tol = 1e-9)$x
  E  <- 1 / sum(gamma * (c_input / p) * s1)               # labor market, L=1
  s  <- E * s1
  x  <- s / p
  z  <- e * x
  yfin <- b * E / p; ok <- b > 0                          # physical final demand
  realY <- exp(sum(b[ok] * log(pmax(yfin[ok], 1e-300))))  # CD real consumption = welfare (real GDP)
  list(s = s, x = x, z = z, Z = sum(z), E = E, realY = realY)
}

full_solve <- function(p_z, sigma, rho, alpha, bundle, b) {
  sol <- solve_prices(p_z, sigma, rho, alpha, bundle)
  q   <- solve_quantities(sol, p_z, sigma, rho, bundle, b)
  list(p = sol$p, e = sol$e, kappa = sol$kappa, converged = sol$converged,
       x = q$x, z = q$z, Z = q$Z, realY = q$realY)
}

# ---- Path-integral decomposition (Baqaee-Farhi differential approach) --------
# Z = sum_i e_i x_i. We split d log Z into technique (intensity) and reallocation
# (physical output, = Corollary 3's d log y_i) by INTEGRATING the two channels
# along the shock path p_z: 0 -> target, with weights at the CURRENT equilibrium
# at each step (not base-year weights). This is exact and residual-free, unlike a
# Laspeyres (base-weight) split. We re-solve the exact equilibrium at each node
# (robust; no Euler drift) and finite-difference between nodes; with multiplicative
# log-mean (LMDI) weights per step the two terms sum to d log Z for ANY K, and as
# K grows the split converges to the structural Divisia integral. K=1 reduces to a
# two-point (endpoint) LMDI; the K=1-vs-large gap measures path dependence (whether
# the covariance genuinely evolves = real global nonlinearity vs. a base-weight artifact).
logmean <- function(a, b) { out <- ifelse(a == b, a, (a - b) / (log(a) - log(b))); out[!is.finite(out)] <- 0; out }

decompose_path <- function(p_target, sigma, rho, alpha, bundle, b, K = 24) {
  em   <- which(bundle$e_bar > 0)                       # emitting firms (fixed set)
  grid <- seq(0, p_target, length.out = K + 1L)
  sols <- lapply(grid, function(pz) full_solve(pz, sigma, rho, alpha, bundle, b))
  Zof  <- function(s) sum(s$z[em])
  technique <- 0; reallocation <- 0
  for (k in seq_len(K)) {
    s0 <- sols[[k]]; s1 <- sols[[k + 1L]]
    w  <- logmean(s1$z[em], s0$z[em]) / logmean(Zof(s1), Zof(s0))   # multiplicative LMDI
    technique    <- technique    + sum(w * (log(s1$e[em]) - log(s0$e[em])))
    reallocation <- reallocation + sum(w * (log(s1$x[em]) - log(s0$x[em])))
  }
  dlogZ <- log(Zof(sols[[K + 1L]])) - log(Zof(sols[[1L]]))
  list(dlogZ = dlogZ, technique = technique, reallocation = reallocation,
       residual = dlogZ - technique - reallocation, K = K)
}

# Efficient variant: solve ONCE along a fine grid (starting at 0) and report the
# CUMULATIVE path decomposition at every grid node, so all price columns of a
# counterfactual come from a single path of solves rather than re-integrating
# 0->p for each column. Returns a data.frame (one row per grid price > 0).
decompose_path_grid <- function(grid, sigma, rho, alpha, bundle, b) {
  stopifnot(grid[1] == 0, all(diff(grid) > 0))
  em   <- which(bundle$e_bar > 0)
  sols <- lapply(grid, function(pz) full_solve(pz, sigma, rho, alpha, bundle, b))
  Zof <- function(s) sum(s$z[em]); Yof <- function(s) sum(s$x)   # emissions; gross output (all firms)
  Z0 <- Zof(sols[[1L]]); Y0 <- Yof(sols[[1L]]); RY0 <- sols[[1L]]$realY
  # Three-way Grossman-Krueger split: d log Z = scale + technique + composition.
  #   scale       = d log Y, Y = GROSS output sum_i x_i  (the emissions-relevant pie;
  #                 NOT zero -- it de-roundabouts as dirty intermediate production shrinks).
  #   technique   = sum_i w_i d log e_i              (within-firm intensity)
  #   composition = sum_i w_i d log(x_i / Y)         (between-firm reallocation)
  # We ALSO report realGDP = d log(real consumption/welfare), which IS ~0 -- to show the
  # gross-output decline is reallocation/de-roundabouting, not a welfare/efficiency loss.
  tech <- 0; quant <- 0; rows <- vector("list", length(grid) - 1L)
  for (k in seq_len(length(grid) - 1L)) {
    s0 <- sols[[k]]; s1 <- sols[[k + 1L]]
    w  <- logmean(s1$z[em], s0$z[em]) / logmean(Zof(s1), Zof(s0))
    tech  <- tech  + sum(w * (log(s1$e[em]) - log(s0$e[em])))   # technique (integral)
    quant <- quant + sum(w * (log(s1$x[em]) - log(s0$x[em])))   # emission-weighted output change
    scale <- log(Yof(s1)) - log(Y0)                            # GROSS output change (de-roundabouts)
    rows[[k]] <- data.frame(p_z = grid[k + 1L], dlogZ = log(Zof(s1)) - log(Z0),
                            scale = scale, technique = tech, composition = quant - scale,
                            realGDP = log(s1$realY) - log(RY0),  # welfare diagnostic (~0)
                            mean_price = mean(s1$p))
  }
  do.call(rbind, rows)
}

# ---- alpha-inversion: calibrate alpha(sigma,rho) to a target emission change --
# Finds alpha s.t.  log Z(p_hi) - log Z(p_lo) = target_dlogZ.
# Framing is exposed via (p_lo, p_hi): e.g. 2005->2022 endpoints (18 -> 80), or
# no-policy -> representative price (0 -> 80 or 0 -> 19). target_dlogZ < 0.
# Returns alpha=NA with the achievable dlogZ range when the target isn't bracketed.
calibrate_alpha <- function(sigma, rho, bundle, b, p_lo, p_hi, target_dlogZ,
                            alpha_range = c(1.05, 50)) {
  dlogZ <- function(a) {
    Zlo <- full_solve(p_lo, sigma, rho, a, bundle, b)$Z
    Zhi <- full_solve(p_hi, sigma, rho, a, bundle, b)$Z
    log(Zhi) - log(Zlo)
  }
  f   <- function(a) dlogZ(a) - target_dlogZ
  flo <- f(alpha_range[1]); fhi <- f(alpha_range[2])
  if (is.na(flo) || is.na(fhi) || sign(flo) == sign(fhi)) {
    return(list(alpha = NA_real_, bracketed = FALSE, target = target_dlogZ,
                dlogZ_lo = dlogZ(alpha_range[1]), dlogZ_hi = dlogZ(alpha_range[2])))
  }
  r <- uniroot(f, alpha_range, tol = 1e-4)
  list(alpha = r$root, bracketed = TRUE, target = target_dlogZ, dlogZ = dlogZ(r$root))
}

# ============================================================================
# Validation
# ============================================================================
if (sys.nframe() == 0) {
  b <- load_bundle(2019)
  cat(sprintf("Bundle: %d firms, %d ETS\n", b$meta$n_firms, b$meta$n_ets))

  # --- Test 1: base (p_z = 0) must return prices=1, e=ebar ---
  s0 <- solve_prices(p_z = 0, sigma = 0.5, rho = 1, alpha = 2, bundle = b)
  cat(sprintf("\n[Test 1: p_z=0]  max|p-1|=%.2e  max|e-ebar|=%.2e  (both should be ~0)\n",
              max(abs(s0$p - 1)), max(abs(s0$e - b$e_bar))))

  # --- Test 2: realized 2022 price, check abatement + price response ---
  for (a in c(1.5, 2, 4)) {
    s <- solve_prices(p_z = 80, sigma = 0.5, rho = 1, alpha = a, bundle = b)
    ets <- b$tau == 1
    cat(sprintf("[p_z=80, alpha=%.1f] conv=%s it=%d | ETS: mean kappa=%.3f, mean e/ebar=%.3f | price: mean=%.3f p95=%.3f\n",
                a, s$converged, s$iters,
                mean(s$kappa[ets]), mean((s$e[ets] + 1e-12) / (b$e_bar[ets] + 1e-12)),
                mean(s$p), quantile(s$p, 0.95)))
  }

  # --- Test 3: pass-through and sigma knobs move prices in the right direction ---
  hi <- solve_prices(80, sigma = 0.5, rho = 1.0, alpha = 2, bundle = b)
  lo <- solve_prices(80, sigma = 0.5, rho = 0.25, alpha = 2, bundle = b)
  cat(sprintf("\n[Test 3: pass-through] mean price rho=1.0 -> %.4f vs rho=0.25 -> %.4f (lower rho => smaller price rise)\n",
              mean(hi$p), mean(lo$p)))

  # ---- Piece 2: quantities + aggregate emissions ----
  bshare <- base_final_shares(b)
  cat(sprintf("\n[final-demand shares] %d goods, sum=%.4f, top share=%.3f\n",
              length(bshare), sum(bshare), max(bshare)))

  base <- full_solve(p_z = 0,  sigma = 0.5, rho = 1, alpha = 2, bundle = b, b = bshare)
  # base check: real output should be proportional to observed revenue
  ok <- !is.na(b$x)
  corr <- cor(base$x[ok], b$x[ok])
  cat(sprintf("[Test 4: base p_z=0] cor(x_model, revenue)=%.4f (should be ~1); Z0=%.4g\n", corr, base$Z))

  for (a in c(2, 4)) {
    cf <- full_solve(p_z = 80, sigma = 0.5, rho = 1, alpha = a, bundle = b, b = bshare)
    wz <- base$z / sum(base$z)
    technique <- sum(wz * (log(cf$e + 1e-300) - log(base$e + 1e-300)))   # sum w_z dlog e
    dlogZ <- log(cf$Z) - log(base$Z)
    cat(sprintf("[Test 5: p_z=80 alpha=%.0f] dlogZ=%.4f | technique=%.4f | quantity+scale=%.4f\n",
                a, dlogZ, technique, dlogZ - technique))
  }

  # ---- alpha-inversion wrapper ----
  # (A) Self-recovery: pick alpha*=3, compute its dlogZ (0->80), recover it.
  astar <- 3
  d_star <- log(full_solve(80, 0.5, 1, astar, b, bshare)$Z) -
            log(full_solve(0,  0.5, 1, astar, b, bshare)$Z)
  rec <- calibrate_alpha(sigma = 0.5, rho = 1, bundle = b, b = bshare,
                         p_lo = 0, p_hi = 80, target_dlogZ = d_star)
  cat(sprintf("\n[Test 6: alpha self-recovery] target dlogZ=%.4f from alpha*=%.1f -> recovered alpha=%.3f (should be ~3)\n",
              d_star, astar, rec$alpha))

  # (B) Real target on subset: -24pp (2005->2022, p 18->80). Expect NOT bracketed.
  cal <- calibrate_alpha(sigma = 0.5, rho = 1, bundle = b, b = bshare,
                         p_lo = 18, p_hi = 80, target_dlogZ = log(0.76))
  if (cal$bracketed) {
    cat(sprintf("[Test 7: real target] calibrated alpha=%.3f\n", cal$alpha))
  } else {
    cat(sprintf("[Test 7: real target -24pp] NOT reachable on subset (dlogZ in [%.3f, %.3f], target=%.3f) -- expected; needs clean RMD data\n",
                cal$dlogZ_hi, cal$dlogZ_lo, log(0.76)))
  }
}
