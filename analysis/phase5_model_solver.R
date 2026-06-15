###############################################################################
# phase5_model_solver.R  —  Global nested-CES solver with incomplete pass-through.
#
# Solves the firm side of the model on an assembled bundle (phase4/model_assembly):
#   (1) Abatement block: closed-form kappa_i*, e_i* given (p_z, tau, alpha, ebar).
#   (2) Price fixed point under a constant-pass-through-elasticity pricing rule.
#   (3) Quantity / market-clearing block: nominal sales s, real output x, emissions Z.
#
# Model (sections/model.tex, quantitative.tex 5.1):
#   - NESTED CES production. Inner nest: suppliers WITHIN a sector g substitute at
#     sigma_W (eq:nested_bottom); outer nest: sector bundles M_ig + labor substitute
#     at sigma_B (eq:nested_top). Sectors = supplier NACE-2d (missing -> one bucket).
#       inner sector price index  P_ig^{1-sW} = sum_{j in g} (Omega_ij/Lambda_ig) p_j^{1-sW}
#       outer cost (w=1 numeraire)  c_i^{1-sB} = sum_g Lambda_ig P_ig^{1-sB} + gamma_i
#       Lambda_ig = sum_{j in g} Omega_ij  (base sector expenditure share; precomputed)
#     sigma_W=sigma_B collapses to a single CES over {suppliers, labor}; calibrated so
#     base prices = 1 (at p=1: P_ig=1, c_i=1 since weights sum to 1).
#   - INCOMPLETE PASS-THROUGH (eq:passthrough_global): firm passes a fraction rho of
#     marginal-cost changes into price via a constant-elasticity rule
#         p_i = (mc_i^0)^{1-rho} mc_i^{rho} = mc_i^{rho}   (since base mc_i^0 = 1),
#     with mc_i = c_i^input(p) + kappa_i + tau_i p_z e_i  (FULL carbon bill).
#     rho=1 => p_i = mc_i (full pass-through, the frictionless baseline).
#   - Abatement: e_i = (1-kappa_i)^{alpha} ebar_i, linear cost kappa_i, ALPHA>1
#     (convex). FOC interior when tau_i p_z ebar_i alpha >= 1, else kappa=0.  (UNCHANGED)
#   - Final demand: nested CES(sigma_B outer / sigma_W inner) over firm outputs with
#     base-share weights b_i (so the reallocation channel mirrors the firm nests).
###############################################################################

suppressMessages({ library(Matrix); library(dplyr) })

proj <- "c:/Users/jota_/Documents/carbon_policy_networks"

load_bundle <- function(year = 2019) {
  f <- file.path(proj, "data/processed", sprintf("model_inputs_%d_subset.RData", year))
  load(f); model_inputs
}

# ---- nested-CES precompute (one-time per bundle) ----------------------------
# Sector grouping from supplier NACE (default NACE-4d = "narrowly-defined sector",
# the level at which sigma_W is estimated; falls back to nace2d if 4d absent).
# Suppliers with a missing code become their OWN singleton sector, so they carry no
# within-sector substitution and sit directly in the outer (sigma_B) nest.
#
# FULLY SPARSE: builds the membership matrix Sg (firm-as-supplier x sector) and the
# base sector-expenditure shares Lambda = Om %*% Sg as a SPARSE dgCMatrix (NOT a
# dense n x G, which blows up at 4d granularity). Lambda's nonzero pattern is the
# set of (buyer i, supplier-sector g) pairs that actually trade; every per-call
# sector quantity (Gmat, the sector price index, shares) lives on this SAME pattern,
# so we just swap @x and use sparse rowSums. `pos` maps each nonzero of Om to its
# (i, g(j)) slot in Lambda@x, so within-sector supplier shares need no dense lookup.
# Attached as bundle$nest; run `bundle <- build_nest(bundle)` ONCE after assembly.
build_nest <- function(bundle, level = c("nace4d", "nace2d")) {
  Om <- as(bundle$Omega, "CsparseMatrix"); n <- nrow(Om)
  level <- match.arg(level)
  nace  <- bundle[[level]]; if (is.null(nace)) nace <- bundle$nace2d
  isna  <- is.na(nace)
  sec   <- as.character(nace); sec[isna] <- paste0("__NA__", which(isna))   # NA -> singletons
  lev   <- unique(sec); grp <- match(sec, lev); G <- length(lev)
  Sg     <- sparseMatrix(i = seq_len(n), j = grp, x = 1, dims = c(n, G))    # supplier -> sector
  Lambda <- as(Om %*% Sg, "CsparseMatrix")                                  # SPARSE Lambda_ig
  cidx   <- rep(seq_len(n), diff(Om@p))                                     # supplier col per Om nnz
  ridxOm <- Om@i + 1L                                                       # buyer row per Om nnz
  Lrow   <- Lambda@i + 1L; Lcol <- rep(seq_len(G), diff(Lambda@p))          # Lambda triplets (@x order)
  keyL   <- Lrow + (Lcol - 1) * as.double(n)                               # double keys (avoid int overflow)
  keyOm  <- ridxOm + (grp[cidx] - 1) * as.double(n)
  pos    <- match(keyOm, keyL)                          # Om nnz -> index into Lambda@x / Gx / Fx
  bundle$nest <- list(Omega = Om, Sg = Sg, grp = grp, Lambda = Lambda, Lx = Lambda@x,
                      cidx = cidx, ridxOm = ridxOm, pos = pos, gamma = bundle$gamma,
                      n = n, G = G, level = level, levels = lev)
  bundle
}

# ---- nested-CES input cost c_i(p) (and, optionally, expenditure-share pieces) -
# All sector objects live on Lambda's sparse pattern (vectors aligned with
# Lambda@x); c_input via sparse rowSums. Handles the Cobb-Douglas limits
# (|sigma-1|<eps) of each nest via logs. When want_shares, returns Fx (the
# secshare-numerator/Gmat ratio on Lambda's pattern), q, c_pow and labshare; the
# caller multiplies in 1/c_pow per buyer row (non-CD outer) when building phi.
.nest_cost <- function(p, nest, sB, sW, want_shares = FALSE) {
  Om <- nest$Omega; Sg <- nest$Sg; Lambda <- nest$Lambda; Lx <- nest$Lx
  gamma <- nest$gamma; cidx <- nest$cidx
  cdW <- abs(sW - 1) < 1e-8; cdB <- abs(sB - 1) < 1e-8
  posL <- Lx > 0
  nz <- length(Lx)

  # inner: sector log price index logPx and Gx (= Gmat@x), on Lambda's pattern
  if (cdW) {
    OmL <- Om; OmL@x <- Om@x * log(p)[cidx]
    LGx <- (OmL %*% Sg)@x                               # aligned with Lambda@x
    Gx  <- Lx                                           # q = p^0 = 1 => Gmat = Lambda
    logPx <- numeric(nz); logPx[posL] <- LGx[posL] / Lx[posL]
    q <- rep(1, length(p))
  } else {
    q <- p^(1 - sW); Omq <- Om; Omq@x <- Om@x * q[cidx]
    Gx <- (Omq %*% Sg)@x
    logPx <- numeric(nz); logPx[posL] <- log(Gx[posL] / Lx[posL]) / (1 - sW)
  }

  # outer: combine sector bundles + labor (w=1); c_input via sparse rowSums
  if (cdB) {
    Term <- Lambda; Term@x <- Lx * logPx                 # gamma_i * log(w=1) = 0
    c_input <- exp(as.numeric(Matrix::rowSums(Term)))
    c_pow <- NULL; numerx <- Lx                          # CD: sector cost share = Lambda_ig
  } else {
    termx <- Lx * exp((1 - sB) * logPx)                  # Lambda_ig P_ig^{1-sB}
    Term <- Lambda; Term@x <- termx
    c_pow <- as.numeric(Matrix::rowSums(Term)) + gamma   # c_input^{1-sB}
    c_input <- c_pow^(1 / (1 - sB)); numerx <- termx
  }

  out <- list(c_input = c_input, q = q, c_pow = c_pow, cdB = cdB)
  if (want_shares) {
    Fx <- numerx / Gx; Fx[!is.finite(Fx)] <- 0          # secshare-numer / Gmat (per Lambda nnz)
    out$Fx <- Fx
    out$labshare <- if (cdB) gamma else gamma / c_pow
  }
  out
}

# ---- Abatement block: closed form (alpha > 1) ------------------------------ (UNCHANGED)
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

# ---- Anderson-accelerated fixed point: solve x = G(x) ----------------------- (UNCHANGED)
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
  Xp <- matrix(0, n, m); Ff <- matrix(0, n, m); ns <- 0L; col <- 0L
  for (it in seq_len(maxit)) {
    g <- G(x); f <- g - x
    delta <- max(abs(f))
    if (delta < tol) break
    col <- (col %% m) + 1L
    Xp[, col] <- x - x_prev; Ff[, col] <- f - f_prev
    ns <- min(ns + 1L, m); x_prev <- x; f_prev <- f
    if (ns == m) { Xs <- Xp; Fs <- Ff }
    else { Xs <- Xp[, seq_len(ns), drop = FALSE]; Fs <- Ff[, seq_len(ns), drop = FALSE] }
    gma <- tryCatch({
      A <- crossprod(Fs)
      as.numeric(solve(A + (1e-10 * mean(diag(A))) * diag(ns), crossprod(Fs, f)))
    }, error = function(e) rep(0, ns))
    x <- as.numeric(x + f - Xs %*% gma - Fs %*% gma)
    if (!is.null(floor)) x[x < floor] <- floor
  }
  list(x = x, iters = it, delta = delta, converged = delta < tol)
}

# ---- Price fixed point ------------------------------------------------------
# p_i = mc_i^{rho},  mc_i = c_i^input(p) + kappa_i + tau_i p_z e_i  (base mc=1).
# The abatement block and carbon bill are invariant in p (a_term), so each
# Anderson step is one nested-cost evaluation. rho may be scalar or firm vector.
solve_prices <- function(p_z, sigma_B, sigma_W, rho, alpha, bundle,
                         maxit = 200, tol = 1e-8, m = 5, verbose = FALSE) {
  if (is.null(bundle$nest)) bundle <- build_nest(bundle)
  nest <- bundle$nest
  tau <- bundle$tau; ebar <- bundle$e_bar; n <- length(bundle$gamma)
  ab <- abatement_block(p_z, tau, alpha, ebar)
  a_term <- ab$kappa + tau * p_z * ab$e             # FULL carbon bill (no rho discount here)

  Gmap <- function(p) (.nest_cost(p, nest, sigma_B, sigma_W)$c_input + a_term)^rho

  r <- anderson_fp(Gmap, rep(1, n), tol = tol, maxit = maxit, m = m, floor = 1e-12)
  if (verbose) cat(sprintf("  price solve: it=%d delta=%.2e\n", r$iters, r$delta))
  list(p = r$x, kappa = ab$kappa, e = ab$e, iters = r$iters, delta = r$delta, converged = r$converged)
}

# ---- Final-demand CES weights b_i, calibrated from base sales --------------- (UNCHANGED)
# At base prices (p=1) the CES expenditure share on i equals b_i, so b_i is the
# base final-demand expenditure share. final demand_i = s_i - sum_j Omega_ji s_j.
base_final_shares <- function(bundle) {
  s <- bundle$x; s[is.na(s)] <- 0
  inter <- as.numeric(Matrix::crossprod(bundle$Omega, s))   # t(Omega) %*% s
  final <- pmax(s - inter, 0)
  final / sum(final)
}

# ---- Nested final-demand shares + welfare price index ----------------------
# Final demand is a nested CES (sigma_B across sectors, sigma_W within) over firm
# outputs with base weights b. Returns the equilibrium expenditure shares fshare
# (CES demand) and the log consumption price index logP (welfare deflator).
.final_demand <- function(p, b, nest, sB, sW) {
  Sg <- nest$Sg; grp <- nest$grp; G <- nest$G
  cdW <- abs(sW - 1) < 1e-8; cdB <- abs(sB - 1) < 1e-8
  B    <- as.numeric(Matrix::crossprod(Sg, b)); Bpos <- B > 0     # sector base weights B_g
  if (cdW) {
    LGb <- as.numeric(Matrix::crossprod(Sg, b * log(p)))
    logPC <- rep(0, G); logPC[Bpos] <- LGb[Bpos] / B[Bpos]
    nb <- b; Gb <- B
  } else {
    nb <- b * p^(1 - sW)
    Gb <- as.numeric(Matrix::crossprod(Sg, nb)); Gbpos <- Gb > 0
    logPC <- rep(0, G); logPC[Gbpos] <- log(Gb[Gbpos] / B[Gbpos]) / (1 - sW)
  }
  if (cdB) { wG <- B;                       logP <- sum(B * logPC) }
  else     { wG <- B * exp((1 - sB) * logPC); logP <- log(sum(wG)) / (1 - sB) }
  outer_g <- wG / sum(wG)
  within  <- nb / Gb[grp]; within[!is.finite(within)] <- 0
  fshare  <- outer_g[grp] * within
  list(fshare = fshare, logP = logP)
}

# ---- Quantity / market-clearing block --------------------------------------
# Given solved prices, compute nominal sales s, real output x, emissions Z.
#   phi_ij = i's equilibrium expenditure share on supplier j (nested CES, Shephard):
#            phi_ij = secshare_{i,g(j)} * Omega_ij p_j^{1-sW} / Gmat_{i,g(j)}
#   M_ji   = phi_ji * c_input_j / p_j   (j's spend on i / revenue_j)
#   s = (I - M')^{-1} f E,  f_i = nested final-demand share
#   E pinned by labor market: L = sum_j labshare_j (c_input_j/p_j) s_j = 1
# The labor/materials split (labshare) is now ENDOGENOUS in sigma_B (outer nest),
# not the fixed base gamma. rho is unused here (c_input is recovered from the nest).
solve_quantities <- function(sol, p_z, sigma_B, sigma_W, rho, bundle, b) {
  if (is.null(bundle$nest)) bundle <- build_nest(bundle)
  nest <- bundle$nest; Om <- nest$Omega; n <- length(bundle$gamma)
  p <- sol$p; e <- sol$e

  nc <- .nest_cost(p, nest, sigma_B, sigma_W, want_shares = TRUE)
  c_input <- nc$c_input

  # equilibrium expenditure-share matrix phi (row i = buyer, col j = supplier):
  #   phi_ij = secshare_{i,g(j)} * (Omega_ij p_j^{1-sW} / Gmat_{i,g(j)})
  # built per nonzero of Om: Fx[pos] picks the (i,g(j)) sector factor, q the price
  # factor; 1/c_pow per buyer row turns the secshare-numerator into a share (non-CD).
  phix <- Om@x * nc$Fx[nest$pos] * nc$q[nest$cidx]
  if (!nc$cdB) phix <- phix / nc$c_pow[nest$ridxOm]
  phi <- sparseMatrix(i = nest$ridxOm, j = nest$cidx, x = phix, dims = c(n, n))

  M  <- Diagonal(x = c_input / p) %*% phi                  # M_ji = phi_ji c_input_j/p_j
  fd <- .final_demand(p, b, nest, sigma_B, sigma_W)
  fshare <- fd$fshare

  Mt <- as(t(M), "CsparseMatrix")
  s1 <- anderson_fp(function(s) fshare + as.numeric(Mt %*% s), fshare, tol = 1e-9)$x
  E  <- 1 / sum(nc$labshare * (c_input / p) * s1)           # labor market, L=1
  s  <- E * s1
  x  <- s / p
  z  <- e * x
  realY <- E / exp(fd$logP)                                 # CES real consumption (welfare)
  list(s = s, x = x, z = z, Z = sum(z), E = E, realY = realY)
}

full_solve <- function(p_z, sigma_B, sigma_W, rho, alpha, bundle, b) {
  if (is.null(bundle$nest)) bundle <- build_nest(bundle)
  sol <- solve_prices(p_z, sigma_B, sigma_W, rho, alpha, bundle)
  q   <- solve_quantities(sol, p_z, sigma_B, sigma_W, rho, bundle, b)
  list(p = sol$p, e = sol$e, kappa = sol$kappa, converged = sol$converged,
       x = q$x, z = q$z, Z = q$Z, realY = q$realY)
}

# ---- Path-integral decomposition (Baqaee-Farhi differential approach) -------- (logic UNCHANGED)
# Z = sum_i e_i x_i. We split d log Z into technique (intensity), scale (gross
# output) and composition (reallocation) by INTEGRATING along the shock path
# p_z: 0 -> target with multiplicative log-mean (LMDI) weights at the current
# equilibrium at each step. Exact and residual-free for any K.
logmean <- function(a, b) { out <- ifelse(a == b, a, (a - b) / (log(a) - log(b))); out[!is.finite(out)] <- 0; out }

# Two-point (K=1) LMDI decomposition of d log Z, base solve s0 -> shocked sj, over
# emitters `em`. Returns c(total, technique, scale, composition). Reused by
# phase6_centrality.R (per-firm marginals) and proxy_horserace.R.
dec1 <- function(sj, s0, em) {
  Z1 <- sum(sj$z[em]); Z0 <- sum(s0$z[em])
  w  <- logmean(sj$z[em], s0$z[em]) / logmean(Z1, Z0)
  tech  <- sum(w * (log(sj$e[em]) - log(s0$e[em])))
  quant <- sum(w * (log(sj$x[em]) - log(s0$x[em])))
  scale <- log(sum(sj$x)) - log(sum(s0$x))
  c(total = log(Z1) - log(Z0), technique = tech, scale = scale, composition = quant - scale)
}

decompose_path <- function(p_target, sigma_B, sigma_W, rho, alpha, bundle, b, K = 24) {
  if (is.null(bundle$nest)) bundle <- build_nest(bundle)
  em   <- which(bundle$e_bar > 0)
  grid <- seq(0, p_target, length.out = K + 1L)
  sols <- lapply(grid, function(pz) full_solve(pz, sigma_B, sigma_W, rho, alpha, bundle, b))
  Zof  <- function(s) sum(s$z[em])
  technique <- 0; reallocation <- 0
  for (k in seq_len(K)) {
    s0 <- sols[[k]]; s1 <- sols[[k + 1L]]
    w  <- logmean(s1$z[em], s0$z[em]) / logmean(Zof(s1), Zof(s0))
    technique    <- technique    + sum(w * (log(s1$e[em]) - log(s0$e[em])))
    reallocation <- reallocation + sum(w * (log(s1$x[em]) - log(s0$x[em])))
  }
  dlogZ <- log(Zof(sols[[K + 1L]])) - log(Zof(sols[[1L]]))
  list(dlogZ = dlogZ, technique = technique, reallocation = reallocation,
       residual = dlogZ - technique - reallocation, K = K)
}

# Solve ONCE along a fine grid (starting at 0) and report the CUMULATIVE path
# decomposition at every node, so all price columns come from a single path of
# solves. Three-way Grossman-Krueger split d log Z = scale + technique +
# composition; realGDP (welfare) reported as a diagnostic (~0).
decompose_path_grid <- function(grid, sigma_B, sigma_W, rho, alpha, bundle, b) {
  stopifnot(grid[1] == 0, all(diff(grid) > 0))
  if (is.null(bundle$nest)) bundle <- build_nest(bundle)
  em   <- which(bundle$e_bar > 0)
  sols <- lapply(grid, function(pz) full_solve(pz, sigma_B, sigma_W, rho, alpha, bundle, b))
  Zof <- function(s) sum(s$z[em]); Yof <- function(s) sum(s$x)
  Z0 <- Zof(sols[[1L]]); Y0 <- Yof(sols[[1L]]); RY0 <- sols[[1L]]$realY
  tech <- 0; quant <- 0; rows <- vector("list", length(grid) - 1L)
  for (k in seq_len(length(grid) - 1L)) {
    s0 <- sols[[k]]; s1 <- sols[[k + 1L]]
    w  <- logmean(s1$z[em], s0$z[em]) / logmean(Zof(s1), Zof(s0))
    tech  <- tech  + sum(w * (log(s1$e[em]) - log(s0$e[em])))
    quant <- quant + sum(w * (log(s1$x[em]) - log(s0$x[em])))
    scale <- log(Yof(s1)) - log(Y0)
    rows[[k]] <- data.frame(p_z = grid[k + 1L], dlogZ = log(Zof(s1)) - log(Z0),
                            scale = scale, technique = tech, composition = quant - scale,
                            realGDP = log(s1$realY) - log(RY0),
                            mean_price = mean(s1$p))
  }
  do.call(rbind, rows)
}

# ---- alpha-inversion: calibrate alpha to a target emission change ----------- (params threaded)
# Finds alpha s.t.  log Z(p_hi) - log Z(p_lo) = target_dlogZ.
calibrate_alpha <- function(sigma_B, sigma_W, rho, bundle, b, p_lo, p_hi, target_dlogZ,
                            alpha_range = c(1.05, 50)) {
  if (is.null(bundle$nest)) bundle <- build_nest(bundle)
  dlogZ <- function(a) {
    Zlo <- full_solve(p_lo, sigma_B, sigma_W, rho, a, bundle, b)$Z
    Zhi <- full_solve(p_hi, sigma_B, sigma_W, rho, a, bundle, b)$Z
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
# Validation (run: Rscript analysis/phase5_model_solver.R, needs a nace2d bundle)
# ============================================================================
if (sys.nframe() == 0) {
  # baseline calibration (quantitative.tex 5.2)
  SB <- 0.1; SW <- 2.5; RHO <- 0.7; ALPHA <- 2

  src <- c("C:/Users/jardang/Documents/carbon_policy_networks",
           "c:/Users/jota_/Documents/carbon_policy_networks")
  root <- src[dir.exists(src)][1]
  bf <- file.path(root, "output_local", "model_inputs_2019_ets_neighbors.RData")
  if (!file.exists(bf)) bf <- file.path(root, "output_rmd", "model_inputs_2019_ets_neighbors.RData")
  load(bf); bundle <- build_nest(bundle)
  b <- base_final_shares(bundle)
  cat(sprintf("Bundle: %d firms, %d ETS, %d emitters, %d sectors\n",
              length(bundle$gamma), sum(bundle$tau), sum(bundle$e_bar > 0), bundle$nest$G))

  # Test 1: base (p_z=0) => prices=1, e=ebar
  s0 <- solve_prices(0, SB, SW, RHO, ALPHA, bundle)
  cat(sprintf("\n[1 base p_z=0] max|p-1|=%.2e  max|e-ebar|=%.2e  (both ~0)\n",
              max(abs(s0$p - 1)), max(abs(s0$e - bundle$e_bar))))

  # Test 2: baseline solve converges; abatement + price response sensible
  s <- solve_prices(80, SB, SW, RHO, ALPHA, bundle, verbose = TRUE)
  ets <- bundle$tau == 1
  cat(sprintf("[2 p_z=80 baseline] conv=%s it=%d | ETS mean kappa=%.3f mean e/ebar=%.3f | price mean=%.4f p95=%.4f\n",
              s$converged, s$iters, mean(s$kappa[ets]),
              mean((s$e[ets] + 1e-12) / (bundle$e_bar[ets] + 1e-12)), mean(s$p), quantile(s$p, 0.95)))

  # Test 3: pass-through monotone (lower rho => smaller price rise)
  hi <- solve_prices(80, SB, SW, 1.0, ALPHA, bundle)
  lo <- solve_prices(80, SB, SW, 0.5, ALPHA, bundle)
  cat(sprintf("[3 pass-through] mean price rho=1.0 -> %.4f  vs  rho=0.5 -> %.4f  (lower rho => smaller)\n",
              mean(hi$p), mean(lo$p)))

  # Test 4: base quantities ~ revenue; baseline counterfactual decomposition
  base <- full_solve(0, SB, SW, RHO, ALPHA, bundle, b)
  ok <- !is.na(bundle$x)
  cat(sprintf("\n[4 base] cor(x_model, revenue)=%.4f (~1); Z0=%.4g\n", cor(base$x[ok], bundle$x[ok]), base$Z))
  cf <- full_solve(80, SB, SW, RHO, ALPHA, bundle, b)
  em <- which(bundle$e_bar > 0)
  d  <- dec1(cf, base, em)
  cat(sprintf("[5 ETS @ p_z=80 baseline] dlogZ=%.4f  technique=%.4f  scale=%.4f  composition=%.4f\n",
              d["total"], d["technique"], d["scale"], d["composition"]))

  # Test 6: sigma_W raises |composition| (more within-sector reallocation)
  dlo <- dec1(full_solve(80, SB, 0.55, RHO, ALPHA, bundle, b), full_solve(0, SB, 0.55, RHO, ALPHA, bundle, b), em)
  cat(sprintf("[6 sigma_W up] composition sigma_W=0.55 -> %.4f  vs  sigma_W=2.5 -> %.4f  (|.| grows in sigma_W)\n",
              dlo["composition"], d["composition"]))
}
