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

# ---- Price fixed point ------------------------------------------------------
solve_prices <- function(p_z, sigma, rho, alpha, bundle,
                         maxit = 2000, tol = 1e-10, damp = 0.5, verbose = FALSE) {
  Omega <- bundle$Omega
  gamma <- bundle$gamma                       # labor+outside (residual) share
  tau   <- bundle$tau
  ebar  <- bundle$e_bar
  n     <- length(gamma)

  mat_share <- rowSums(Omega)                 # materials share = 1 - gamma
  theta <- Omega
  pos <- mat_share > 0
  # row-normalize Omega -> theta (materials CES weights, rows sum to 1)
  theta@x <- theta@x / mat_share[theta@i + 1L]

  ab <- abatement_block(p_z, tau, alpha, ebar)
  carbon_term <- rho * tau * p_z * ab$e       # level added to price

  p <- rep(1, n)
  for (it in seq_len(maxit)) {
    if (abs(sigma - 1) < 1e-8) {              # Cobb-Douglas materials nest
      logPm <- as.numeric(theta %*% log(p))
      cinput <- exp((1 - gamma) * logPm)
    } else {
      agg <- as.numeric(theta %*% (p^(1 - sigma)))   # sum_j theta_ij p_j^{1-sigma}
      cinput <- agg^((1 - gamma) / (1 - sigma))      # = Pm^{1-gamma}
    }
    cinput[!pos] <- 1                          # all-labor firms: input cost = 1
    p_new <- cinput + ab$kappa + carbon_term
    delta <- max(abs(p_new - p))
    p <- damp * p_new + (1 - damp) * p
    if (delta < tol) { if (verbose) cat(sprintf("  converged it=%d delta=%.2e\n", it, delta)); break }
  }
  list(p = p, kappa = ab$kappa, e = ab$e, iters = it, delta = delta,
       converged = delta < tol)
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

  A  <- Diagonal(n) - t(M)
  s1 <- as.numeric(Matrix::solve(A, b))                  # sales up to scale (E=1)
  E  <- 1 / sum(gamma * (c_input / p) * s1)               # labor market, L=1
  s  <- E * s1
  x  <- s / p
  z  <- e * x
  list(s = s, x = x, z = z, Z = sum(z), E = E)
}

full_solve <- function(p_z, sigma, rho, alpha, bundle, b) {
  sol <- solve_prices(p_z, sigma, rho, alpha, bundle)
  q   <- solve_quantities(sol, p_z, sigma, rho, bundle, b)
  list(p = sol$p, e = sol$e, kappa = sol$kappa, converged = sol$converged,
       x = q$x, z = q$z, Z = q$Z)
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
