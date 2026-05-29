###############################################################################
# utils/sector_conventions.R
#
# Sector recoding conventions and within-sector dispersion helpers used by the
# phase3_* scripts (firm-level dispersion in (network-adjusted) emission
# intensity).
#
# Adapted from facts-emissions-across-network/utils/sector_conventions.R, with
# make_nace4d() added.
#
# NACE convention: 17 (paper) and 18 (printing) are merged to "17/18" at the
# 2-digit level (the imputation/NIR "paper" cell). At 4-digit the codes stay
# distinct.
###############################################################################

#' 2-digit NACE with the 17/18 merge applied.
make_nace2d <- function(nace5d) {
  n2 <- substr(nace5d, 1, 2)
  n2[n2 %in% c("17", "18")] <- "17/18"
  n2
}

#' 4-digit NACE (no merge; 17xx and 18xx stay distinct).
make_nace4d <- function(nace5d) {
  substr(nace5d, 1, 4)
}

# Minimum firms per sector cell to report a dispersion statistic / correlation.
MIN_N_STATS <- 3L
MIN_N_CORR  <- 10L

#' Gini coefficient on positive values.
gini <- function(x) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  if (n < 2L) return(NA_real_)
  x <- sort(x)
  2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1L) / n
}

#' Ratio of the p_hi to p_lo quantiles, on positive values.
pct_ratio <- function(x, p_hi, p_lo) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) < 2L) return(NA_real_)
  q <- quantile(x, c(p_lo, p_hi), names = FALSE)
  if (q[1L] == 0) return(NA_real_)
  q[2L] / q[1L]
}

#' Within-cell dispersion statistics for an intensity vector `ei` (e.g.
#' emissions / revenue, or network-adjusted emission intensity).
#'
#' Returns a one-row data.frame. Uses only finite, strictly-positive values.
#' Log-based stats (`p9010_log`, `var_log`) are symmetric: dispersion of
#' log(emissions/revenue) equals dispersion of log(revenue/emissions) up to
#' sign, so these are directly comparable to the "carbon productivity"
#' framing used in the literature.
disp_stats <- function(ei) {
  ei <- ei[is.finite(ei) & ei > 0]
  n  <- length(ei)
  if (n < 2L) {
    return(data.frame(n_firms = n,
                      p90p10 = NA_real_, p9010_log = NA_real_,
                      var_log = NA_real_, gini = NA_real_))
  }
  le <- log(ei)
  data.frame(
    n_firms   = n,
    p90p10    = pct_ratio(ei, 0.9, 0.1),
    p9010_log = as.numeric(diff(quantile(le, c(0.1, 0.9), names = FALSE))),
    var_log   = var(le),
    gini      = gini(ei)
  )
}
