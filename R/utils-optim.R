# Simple central-difference Jacobian of a gradient function.
# Does 2m evaluations vs numDeriv::jacobian's ~4m (Richardson extrapolation).
fast_jacobian <- function(gr_fn, x, h = 1e-5, cols = seq_along(x)) {
  m <- length(x)
  H <- matrix(0, m, m)
  for (j in cols) {
    xp <- xm <- x
    xp[j] <- x[j] + h
    xm[j] <- x[j] - h
    H[, j] <- (gr_fn(xp) - gr_fn(xm)) / (2 * h)
  }
  H
}

# Central-difference Hessian of a scalar function (no analytic gradient needed).
# Does 4m(m-1)/2 + 2m = 2m^2 evaluations of fn.
fast_hessian <- function(fn, x, h = 1e-5) {
  m <- length(x)
  f0 <- fn(x)
  H <- matrix(0, m, m)

  # Diagonal: (f(x+h*ei) - 2*f(x) + f(x-h*ei)) / h^2
  for (i in seq_len(m)) {
    xp <- xm <- x
    xp[i] <- x[i] + h
    xm[i] <- x[i] - h
    H[i, i] <- (fn(xp) - 2 * f0 + fn(xm)) / (h^2)
  }

  # Off-diagonal (symmetric): use the cross-partial formula
  # (f(x+h*ei+h*ej) - f(x+h*ei-h*ej) - f(x-h*ei+h*ej) + f(x-h*ei-h*ej)) / (4h^2)
  if (m > 1) {
    for (i in seq_len(m - 1)) {
      for (j in (i + 1):m) {
        xpp <- xpm <- xmp <- xmm <- x
        xpp[c(i, j)] <- x[c(i, j)] + h
        xmm[c(i, j)] <- x[c(i, j)] - h
        xpm[i] <- x[i] + h; xpm[j] <- x[j] - h
        xmp[i] <- x[i] - h; xmp[j] <- x[j] + h
        H[i, j] <- H[j, i] <-
          (fn(xpp) - fn(xpm) - fn(xmp) + fn(xmm)) / (4 * h^2)
      }
    }
  }

  H
}

# Central-difference gradient of a scalar function.
# Does 2m evaluations of fn.
fast_grad <- function(fn, x, h = 1e-5) {
  m <- length(x)
  g <- numeric(m)
  for (j in seq_len(m)) {
    xp <- xm <- x
    xp[j] <- x[j] + h
    xm[j] <- x[j] - h
    g[j] <- (fn(xp) - fn(xm)) / (2 * h)
  }
  g
}


# Saturated-means fast path: detect a mean block for which the posterior is
# exactly block-diagonal at the mode. With Gaussian likelihood the cross
# information between means and covariance parameters is n dSigma^{-1}/dtheta
# (ybar - mu_hat), which vanishes when the mean structure is saturated
# (mu_hat = ybar): all intercepts free and unconstrained with normal priors,
# no free latent means, fixed intercepts only for frozen exogenous
# covariates. Along those axes the posterior is exactly Gaussian, so the
# Hessian block is analytic (n Sigma^{-1}[free, free] + prior precision) and
# the skew-normal scans are redundant. Returns NULL when the fast path does
# not apply, else the packed indices, prior precisions, and Sigma row
# positions of the free intercepts.
saturated_mean_idx <- function(pt, lavmodel, lavsamplestats, lavdata,
                               ceq.simple) {
  if (!isTRUE(lavmodel@meanstructure)) return(NULL)
  if (lavmodel@ngroups > 1L || lavdata@nlevels > 1L) return(NULL)
  if (isTRUE(ceq.simple)) return(NULL)
  if (!is.null(attr(pt, "gcp_blocks"))) return(NULL)
  if (any(pt$mat == "alpha" & pt$free > 0)) return(NULL)
  # mu must equal nu EXACTLY: any nonzero alpha (frozen exogenous dummies
  # under fixed.x, or latent means fixed at nonzero values) makes the
  # implied mean depend on free loadings/regressions, so the mean/covariance
  # cross-information no longer vanishes at the mode
  a_vals <- unlist(lavmodel@GLIST[names(lavmodel@GLIST) == "alpha"])
  if (length(a_vals) && any(abs(a_vals) > 1e-12)) return(NULL)
  if (length(unlist(lavsamplestats@x.idx)) > 0L) return(NULL)
  nu_rows <- which(pt$mat == "nu")
  if (!length(nu_rows)) return(NULL)
  free_nu <- nu_rows[pt$free[nu_rows] > 0]
  if (!length(free_nu)) return(NULL)
  fixed_nu <- setdiff(nu_rows, free_nu)
  ovn <- lavdata@ov.names[[1L]]
  xn <- ovn[unlist(lavsamplestats@x.idx[[1L]])]
  if (length(fixed_nu) && !all(pt$lhs[fixed_nu] %in% xn)) return(NULL)
  if (!setequal(pt$lhs[free_nu], setdiff(ovn, xn))) return(NULL)
  if (!all(grepl("^normal\\(", pt$prior[free_nu]))) return(NULL)
  idx <- pt$free[free_nu]
  if (any(duplicated(idx)) || any(idx %in% pt$free[-nu_rows])) return(NULL)
  sds <- vapply(pt$prior[free_nu], function(s) {
    as.numeric(strsplit(gsub("normal\\(|\\)", "", s), ",")[[1]][2])
  }, numeric(1))
  ord <- order(idx)
  list(
    idx = idx[ord],
    prec = (1 / sds^2)[ord],
    sigma_pos = match(pt$lhs[free_nu], ovn)[ord]
  )
}
