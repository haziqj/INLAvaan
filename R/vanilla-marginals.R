# Stage 2: skew-normal marginals, one per parameter, ported from INLAvaan.
#
# For parameter j the joint log-posterior is scanned along the direction of
# the conditional mode path (CMP) of the Laplace Gaussian,
#   theta(t) = mode + t v_j,  v_j = Sigma[, j] / sqrt(Sigma[j, j]),
# so theta_j(t) = mode_j + t sd_j and the other parameters sit at their
# Gaussian conditional means (Jamil & Rue, 2026, Lemma 3.1). The log-heights
# are corrected for the change in the volume of the other parameters along
# the path by adding t gamma1_j (Jamil & Rue, 2026, Corollary 3.3), and a
# skew-normal is fitted to the corrected log-heights on a grid of t.
#
# The correction is the linear term of the width gamma_j(t) =
# -1/2 log|H_{-j}(theta(t))|, the log-volume of the other parameters. The
# `marginal_scan` argument chooses it ("linear", the default), or one of three
# other scans, each one error away from the next: "plain" leaves the width term
# out, "exact" takes the width exactly along the CMP, removing the
# linearisation, and "laplace" moves the other parameters to their
# conditional mode at each point and takes height and width there, the
# Laplace marginal of Tierney and Kadane (1986), removing the error of the
# straight path as well. The skew-normal fit is the same for all four.
#
# Jamil, H., & Rue, H. (2026). Approximate Bayesian inference for structural
# equation models using integrated nested Laplace approximations
# (arXiv:2603.25690 [stat.ME]). arXiv.
# https://doi.org/10.48550/arXiv.2603.25690
#
# Tierney, L., & Kadane, J. B. (1986). Accurate approximations for posterior
# moments and marginal densities. Journal of the American Statistical
# Association, 81(393), 82-86. https://doi.org/10.1080/01621459.1986.10478240

# The volume correction gamma1_j: the rate of change, along v_j, of minus half
# the log-determinant of the conditional precision of the other parameters. In
# whitened coordinates (along the columns of A, from cov_root()) the
# precision at the mode is the identity, so its trace is m and v_j' H v_j = 1
# there.

# The correction for every parameter at once, from the gradient. With T the
# third derivative of the log-posterior at the mode,
#   gamma1_j = 1/2 (sum_k T(A_k, A_k, v_j) - T(v_j, v_j, v_j)),
# and T(a, a, .) is the second difference of the gradient along a. So one
# vector, the sum over k, holds the trace part of every parameter, and each
# v_j adds one more second difference: 4m + 1 gradient calls for all m
# parameters. The step is `h` posterior SDs. Truncation grows as h^2, and noise
# in the gradient, such as that of an inner optimisation, as 1 / h^2. Any A
# with A A' = Sigma gives the same sum in the limit. fit_marginals() passes
# lap$A from cov_root(), so that the truncation does not depend on the order
# of the parameters. Only the parameters in `which` get their own second
# difference; the others are NA.
gamma1_grad <- function(gr, mode, Vscan, A, h = 0.1, which = seq_along(mode)) {
  m <- length(mode)
  g0 <- gr(mode)
  second_diff <- function(a) {
    (gr(mode + h * a) + gr(mode - h * a) - 2 * g0) / h^2
  }
  u <- numeric(m)
  for (k in seq_len(m)) {
    u <- u + second_diff(A[, k])
  }
  vapply(
    seq_len(m),
    function(j) {
      if (j %in% which) {
        0.5 * sum(Vscan[, j] * (u - second_diff(Vscan[, j])))
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
}

# The correction from the log-posterior alone, by a forward difference of
# `d_outer` posterior SDs from the mode. Each whitened curvature is a second
# difference of the log-posterior, 2m + 5 calls per parameter. Second
# differences carry a truncation error of their own, so the baselines at the
# mode (`at_mode`, and the curvature along v_j computed here) are taken by the
# same rule rather than set to m and 1, and the error cancels in the forward
# difference.
#
# The same calls, differenced the other way, give the whitened gradient at
# mode + d_outer v_j, and from it the direction in which the ridge bends, for
# scan_bend(). In whitened coordinates the scan direction is a = A[j, ] / s_j,
# and with T the third derivative at the mode the gradient along the scan is
#   g(t) = g(0) - t a + t^2 / 2 T(a, a, .) + ...
# So g(t) - g(0) + t a, without its part along a, is the second-order term
# that points across the ridge. Returns `gamma1` and `bend_dir`, that
# direction as a unit vector in whitened coordinates, or NULL if it cannot be
# found.
gamma1_lp <- function(
  j,
  lp,
  mode,
  lp_max,
  Vscan,
  A,
  at_mode,
  d_outer = 0.01,
  h = 1e-2
) {
  vj <- Vscan[, j]
  th <- mode + vj * d_outer
  f_th <- lp(th)
  at_th <- whitened_lp(lp, th, f_th, A, h)
  vHv <- -fd_curvature(lp, th, vj, h, f0 = f_th)
  vHv0 <- -fd_curvature(lp, mode, vj, h, f0 = lp_max)
  a <- A[j, ] / sqrt(sum(A[j, ]^2))
  across <- at_th$grad - at_mode$grad + d_outer * a
  across <- across - a * sum(a * across)
  size <- sqrt(sum(across^2))
  list(
    gamma1 = -0.5 *
      (at_th$trace - at_mode$trace) /
      d_outer +
      0.5 * (vHv - vHv0) / d_outer,
    bend_dir = if (is.finite(size) && size > 0) across / size
  )
}

# The whitened trace of the precision at `x`, whose log-posterior is `f_x`, and
# the whitened gradient there, from the same 2m calls along the columns of A.
whitened_lp <- function(lp, x, f_x, A, h = 1e-2) {
  tr <- 0
  grad <- numeric(ncol(A))
  for (k in seq_len(ncol(A))) {
    up <- lp(x + h * A[, k])
    down <- lp(x - h * A[, k])
    tr <- tr - (up - 2 * f_x + down) / h^2
    grad[k] <- (up - down) / (2 * h)
  }
  list(trace = tr, grad = grad)
}

# The width term without the linearisation: the volume of the other
# parameters, gamma_j(x) = -1/2 log|H_{-j}(x)|, from the negative Hessian
# H_{-j} of the log-posterior in the other parameters at the point `x`. It
# comes from one call of the user's Hessian, from central differences of the
# gradient in the other coordinates (2(m - 1) gradient calls), or from second
# differences of the log-posterior there ((m - 1)m calls, with `f0` the value
# at `x`). The steps are those of the Hessian at the mode in laplace_at(), so
# the volume at the mode, taken from lap$H, is on the same footing. NA where
# H_{-j} is not positive definite or cannot be evaluated.
cond_volume <- function(x, j, lp, gr, he, lap, f0 = NULL) {
  others <- seq_along(x)[-j]
  h <- lap$scales[others]
  Hb <- if (!is.null(he)) {
    -he(x)[others, others, drop = FALSE]
  } else if (!is.null(gr)) {
    gr_others <- function(y) {
      x[others] <- y
      gr(x)[others]
    }
    -fd_jacobian(gr_others, x[others], h = 1e-4 * h)
  } else {
    lp_others <- function(y) {
      x[others] <- y
      lp(x)
    }
    if (is.null(f0)) {
      f0 <- lp(x)
    }
    -fd_hessian(lp_others, x[others], h = 0.01 * h, f0 = f0)
  }
  half_log_det(Hb)
}

# Minus half the log-determinant of a negative Hessian, symmetrised, or NA
# when it is not positive definite.
half_log_det <- function(H) {
  R <- chol_or_null(0.5 * (H + t(H)))
  if (is.null(R)) NA_real_ else -sum(log(diag(R)))
}

# "exact": the exact width term along the CMP, gamma_j(t) - gamma_j(0)
# at each scan point, with the heights `raw` already known. The volume at
# the mode is that of lap$H. Points where the height is not finite are left
# NA, with no calls.
width_exact <- function(j, lp, gr, he, lap, Vscan, z, raw) {
  g0 <- half_log_det(lap$H[-j, -j, drop = FALSE])
  vapply(
    seq_along(z),
    function(k) {
      if (z[k] == 0) {
        return(0)
      }
      if (!is.finite(raw[k])) {
        return(NA_real_)
      }
      x <- lap$mode + Vscan[, j] * z[k]
      cond_volume(x, j, lp, gr, he, lap, f0 = raw[k]) - g0
    },
    numeric(1)
  )
}

# "laplace": the Laplace marginal of Tierney and Kadane (1986). At each
# scan point, theta_j = mode_j + t sd_j, the other parameters are moved to
# their conditional mode, found by stats::nlminb() from the CMP point, in the
# whitened coordinates of their conditional Laplace covariance C_j (where the
# conditional posterior has unit curvature at the mode). The height is the
# log-posterior there and the width the volume there, relative to the mode.
# Returns both, with NA where the conditional mode or its volume could not be
# found. Needs m > 1.
scan_laplace <- function(j, lp, gr, he, lap, Vscan, z) {
  others <- seq_along(lap$mode)[-j]
  height <- width <- rep(NA_real_, length(z))
  g0 <- half_log_det(lap$H[-j, -j, drop = FALSE])
  # u -> theta_{-j}: x_{-j} = start + Lc u, with Lc Lc' = H_{-j}^{-1}
  Rc <- chol_or_null(lap$H[others, others, drop = FALSE])
  Lc <- if (is.null(Rc)) {
    diag(lap$scales[others], length(others))
  } else {
    backsolve(Rc, diag(length(others)))
  }
  for (k in seq_along(z)) {
    if (z[k] == 0) {
      height[k] <- lap$lp_max
      width[k] <- 0
      next
    }
    x0 <- lap$mode + Vscan[, j] * z[k]
    to_x <- function(u) {
      x <- x0
      x[others] <- x0[others] + drop(Lc %*% u)
      x
    }
    ob <- function(u) {
      v <- lp(to_x(u))
      if (is.finite(v)) -v else Inf
    }
    gro <- if (is.null(gr)) {
      NULL
    } else {
      function(u) -drop(crossprod(Lc, gr(to_x(u))[others]))
    }
    heo <- if (is.null(he)) {
      NULL
    } else {
      function(u) {
        Hb <- -he(to_x(u))[others, others, drop = FALSE]
        crossprod(Lc, Hb %*% Lc)
      }
    }
    opt <- tryCatch(
      stats::nlminb(
        numeric(length(others)),
        ob,
        gradient = gro,
        hessian = heo,
        control = list(iter.max = 200L, eval.max = 400L)
      ),
      error = function(e) NULL
    )
    if (is.null(opt) || !is.finite(opt$objective)) {
      next
    }
    x <- to_x(opt$par)
    height[k] <- -opt$objective
    width[k] <- cond_volume(x, j, lp, gr, he, lap, f0 = height[k]) - g0
  }
  list(height = height, width = width)
}

# The slope of a width curve at the mode, per posterior SD: the central
# difference across t = 0 between the nearest scan points either side. For
# the linear trace it is gamma1 itself.
width_slope <- function(z, w) {
  l <- which(z < 0)
  r <- which(z > 0)
  if (length(l) == 0L || length(r) == 0L) {
    return(NA_real_)
  }
  l <- l[which.max(z[l])]
  r <- r[which.min(z[r])]
  (w[r] - w[l]) / (z[r] - z[l])
}

# Scan each parameter over +-4 Laplace SDs, as in INLAvaan, and fit its
# skew-normal. `tails` is FALSE when the corrected log-height at an end of the
# scan is still within 3 of its maximum (over 5% of the peak density): the
# Laplace SD has badly understated a heavy tail, and the skew-normal
# extrapolates it. Moderately skewed marginals, which the skew-normal fits
# well, end around -4 to -6 on their heavy side. Extending the scan was tried
# and rejected: it improved some heavy tails but pulled the fit away from the
# bulk of others (a Fisher-z correlation lost an order of magnitude in SD
# accuracy), so the problem is reported instead.
#
# The width term (the volume of the other parameters) comes from
# `marginal_scan`: "linear" (the default) adds the linear term gamma1_j t,
# "plain" adds nothing, "exact" the exact volume along the CMP
# (width_exact()), and "laplace" replaces the scan by the Laplace marginal of
# Tierney and Kadane (scan_laplace()), heights included. With `tk_fallback`, a
# parameter whose ridge bend is flagged (above summary_levels$ridge_bend) is
# scanned by "laplace" whatever `marginal_scan` says. The linear volume
# correction takes gamma1_grad() with a gradient, whether or not there is a
# Hessian, because a Hessian per parameter costs far more than the gradients
# it would save, and gamma1_lp() otherwise. With one parameter there are no
# other parameters: "exact" and "laplace" are then the linear scan, and the two
# routes of the linear term agree.
#
# Returns `sn`, a matrix with a row per parameter (the skew-normal on the
# theta scale, the diagnostics, and `logC`, the log scale factor that sets
# the fitted density's height against the scan normalised to a peak of 1),
# the scan grid `z`, `logpost`, the log-heights along each scan (a column per
# parameter: along the CMP, or at the conditional modes for "laplace"),
# `width`, the width term added to them (zero at the mode), and `method`, the
# scan each parameter used. plot(type = "sn_fit") draws them as INLAvaan's
# visual_debug() does.
#
# The parameters in `gaussian` (the `gaussian_marginals` of vanilla_core())
# are not scanned: their marginal is the Gaussian N(mode_j, Sigma_jj), a
# skew-normal with shape 0, with NA for the scan's diagnostics, its heights
# and its width term, and NA for its scan in `method`.
fit_marginals <- function(
  lp,
  gr,
  lap,
  ctrl,
  tick = function() NULL,
  start = function(step) NULL,
  he = NULL,
  marginal_scan = "linear",
  tk_fallback = FALSE,
  par = NULL,
  gaussian = integer()
) {
  mode <- lap$mode
  Sigma <- lap$Sigma
  A <- lap$A
  m <- length(mode)
  sds <- sqrt(diag(Sigma))
  # The parameters to scan
  todo <- setdiff(seq_len(m), gaussian)
  Vscan <- sweep(Sigma, 2, sds, "/")
  z <- seq(-4, 4, length.out = ctrl$n_grid)

  # Whether the scans take the linear width term: with "linear", and with one
  # parameter unless the scan is "plain"
  linear <- marginal_scan == "linear" ||
    (m == 1L && marginal_scan != "plain")
  # The baselines at the mode for gamma1_lp(), only when some parameter uses it
  at_mode <- if (is.null(gr) && linear && length(todo) > 0L) {
    whitened_lp(lp, mode, lap$lp_max, A)
  } else {
    NULL
  }
  # The linear width term from the gradient, for all the parameters at once.
  # It has a progress line of its own, ahead of the per-parameter one, whose
  # estimate of the time left would otherwise charge it to the first parameter.
  g1_grad <- if (!is.null(gr) && linear && length(todo) > 0L) {
    start("shared")
    gamma1_grad(gr, mode, Vscan, A, which = todo)
  } else {
    NULL
  }

  ctx <- list(
    lap = lap,
    Vscan = Vscan,
    z = z,
    # Scan points for the ridge bend (see scan_bend())
    i_bend = unique(c(which.min(abs(z + 2)), which.min(abs(z - 2)))),
    marginal_scan = marginal_scan,
    tk_fallback = tk_fallback,
    log_threshold = ctrl$log_threshold,
    at_mode = at_mode,
    g1_grad = g1_grad
  )

  # On workers, if the fit has `cores` or the user a future plan, and the scans
  # are long enough. Before the per-parameter line starts, because it can print
  # a message
  setup <- list(jobs = 1L)
  if (!is.null(par) && length(todo) > 1L) {
    data <- c(par[c("logpost", "grad", "hess", "init")], list(ctx = ctx))
    setup <- par_setup(
      par[c("logpost", "grad", "hess")],
      scan_probe,
      data,
      est_s = par$call_s *
        length(todo) *
        scan_calls(m, ctrl$n_grid, marginal_scan, !is.null(gr), !is.null(he)),
      n = length(todo),
      note = par$note,
      label = "The skew-normal scans",
      what = "logpost",
      cores = par$cores
    )
    on.exit(setup$restore(), add = TRUE)
  }
  start("scans")

  # A parameter ticks once, however many times its job reports it
  ticked <- logical(m)
  tick_once <- function(j) {
    if (!ticked[[j]]) {
      ticked[[j]] <<- TRUE
      tick()
    }
  }
  scans <- vector("list", m)
  for (j in gaussian) {
    na <- rep(NA_real_, length(z))
    scans[[j]] <- list(
      row = c(mode[[j]], sds[[j]], 0, NA, NA, NA, NA, NA),
      raw = na,
      w = na,
      method = NA_character_
    )
  }
  if (setup$jobs > 1L) {
    groups <- lapply(
      par_groups(length(todo), setup$jobs),
      function(i) todo[i]
    )
    done <- par_run(groups, scan_job, data, setup, tick_once)
    for (k in seq_along(groups)) {
      if (!is.null(done[[k]])) {
        scans[groups[[k]]] <- done[[k]]$scans
        par$add_calls(done[[k]]$calls)
      }
    }
  }
  # Every parameter a worker did not return, and with no workers every one,
  # is scanned here, so an error reads as it does in a serial fit
  for (j in seq_len(m)) {
    if (is.null(scans[[j]])) {
      scans[[j]] <- scan_parameter(j, lp, gr, he, ctx)
    }
    tick_once(j)
  }

  out <- t(vapply(scans, function(s) s$row, numeric(8)))
  dimnames(out) <- list(
    names(mode),
    c("xi", "omega", "alpha", "gamma1", "nmad", "tails", "ridge_bend", "logC")
  )
  heights <- vapply(scans, function(s) s$raw, numeric(length(z)))
  widths <- vapply(scans, function(s) s$w, numeric(length(z)))
  dimnames(heights) <- dimnames(widths) <- list(NULL, names(mode))
  list(
    sn = out,
    z = z,
    logpost = heights,
    width = widths,
    method = stats::setNames(
      vapply(scans, function(s) s$method, character(1)),
      names(mode)
    ),
    workers = setup$jobs
  )
}

# The parameters `ids` on a worker, with wrappers of its own: the fit's are tied
# to the fit's progress line and frame. Returns the scans and the calls made.
scan_job <- function(ids, data) {
  fns <- wrap_fns(data$logpost, data$grad, data$hess, data$init)
  scans <- lapply(ids, function(j) {
    scan <- scan_parameter(j, fns$lp, fns$gr, fns$he, data$ctx)
    par_tick(j)
    scan
  })
  list(
    scans = scans,
    calls = c(fns$state$n_lp, fns$state$n_gr, fns$state$n_he)
  )
}

# The user's functions at the mode, to compare a worker with the parent. It has
# wrappers of its own so that the check does not show in the fit's counts.
scan_probe <- function(ids, data) {
  fns <- wrap_fns(data$logpost, data$grad, data$hess, data$init)
  x <- data$ctx$lap$mode
  list(
    lp = fns$lp(x),
    gr = if (!is.null(fns$gr)) fns$gr(x),
    he = if (!is.null(fns$he)) fns$he(x)
  )
}

# A rough number of calls per parameter of the user's functions in the scan,
# from the counts in ?inla_vanilla, to judge whether the scans are long. The
# conditional optimisations of "laplace" took about 12 calls per scan point
# with a gradient and 7m without one on the fits tried.
scan_calls <- function(m, n_grid, marginal_scan, has_grad, has_hess) {
  n <- n_grid - 1
  volume <- if (has_hess) {
    1
  } else if (has_grad) {
    2 * (m - 1)
  } else {
    m * (m - 1)
  }
  switch(
    marginal_scan,
    linear = n + 2 + if (has_grad) 0 else 2 * m + 5,
    plain = n + 2,
    exact = n + 2 + n * volume,
    laplace = 2 + n * (volume + if (has_grad) 12 else 7 * m)
  )
}

# The scan and skew-normal fit of parameter j. `lp`, `gr` and `he` are wrappers
# from wrap_fns(), and `ctx` holds what the parameters share, set up by
# fit_marginals(). Nothing here depends on another parameter's scan. Returns
# `row`, the parameter's row of `sn`, `raw` and `w`, its log-heights and width
# term, and `method`, the scan it used.
scan_parameter <- function(j, lp, gr, he, ctx) {
  lap <- ctx$lap
  mode <- lap$mode
  m <- length(mode)
  sd_j <- sqrt(lap$Sigma[j, j])
  Vscan <- ctx$Vscan
  z <- ctx$z
  i_bend <- ctx$i_bend
  method <- ctx$marginal_scan
  # Log-posterior along the CMP; the mode's height is known
  cmp_height <- function(t) {
    vapply(
      t,
      function(t) if (t == 0) lap$lp_max else lp(mode + Vscan[, j] * t),
      numeric(1)
    )
  }
  above <- function(h, y) {
    is.finite(h) && h - max(y[is.finite(y)]) > -3
  }

  # The heights along the CMP, which "laplace" does not need beyond the ridge
  # bend, and that only without a gradient
  raw <- if (method != "laplace" || m == 1L) {
    cmp_height(z)
  } else {
    replace(
      rep(NA_real_, length(z)),
      i_bend,
      if (is.null(gr)) cmp_height(z[i_bend]) else NA_real_
    )
  }
  # Without a gradient the volume correction comes first, because its calls
  # also say which way the ridge bends
  g1_lp <- if (!is.null(ctx$at_mode)) {
    gamma1_lp(j, lp, mode, lap$lp_max, Vscan, lap$A, ctx$at_mode)
  } else {
    NULL
  }
  bend <- scan_bend(
    j,
    lp,
    gr,
    lap,
    Vscan,
    z[i_bend],
    raw[i_bend],
    g1_lp$bend_dir
  )
  if (ctx$tk_fallback && off_level(bend, summary_levels$ridge_bend)) {
    method <- "laplace"
  }

  # The width term, and the heights it is added to. With one parameter
  # there are no others, so "exact" and "laplace" are the linear scan, taken
  # by the linear route.
  use <- if (m == 1L && method != "plain") "linear" else method
  if (use == "linear") {
    g1 <- if (is.null(gr)) g1_lp$gamma1 else ctx$g1_grad[[j]]
    w <- g1 * z
  } else if (use == "plain") {
    g1 <- NA_real_
    w <- numeric(length(z))
  } else if (use == "exact") {
    w <- width_exact(j, lp, gr, he, lap, Vscan, z, raw)
    g1 <- width_slope(z, w)
  } else {
    tk <- scan_laplace(j, lp, gr, he, lap, Vscan, z)
    raw <- tk$height
    w <- tk$width
    g1 <- width_slope(z, w)
  }

  # Corrected log-heights
  y <- raw + w
  tails <- !above(y[1], y) && !above(y[length(y)], y)

  ok <- is.finite(y)
  fit <- fit_skew_normal(
    x = z[ok],
    y = y[ok] - max(y[ok]),
    threshold_log_drop = ctx$log_threshold
  )

  # Back from the scan coordinate t to theta_j = mode_j + t sd_j
  list(
    row = c(
      mode[[j]] + fit$xi * sd_j,
      fit$omega * sd_j,
      fit$alpha,
      g1,
      fit$nmad,
      tails,
      bend,
      fit$logC
    ),
    raw = raw,
    w = w,
    method = method
  )
}

# The mass each fitted marginal puts outside the window that was scanned to
# fit it, mode +- 4 Laplace SDs: there the skew-normal is extrapolated, so
# when this is large the outer quantiles are the ones to distrust. This is
# INLAvaan's `scan_end_mass` (in its diagnostics()), renamed. A Gaussian
# marginal gives 2 pnorm(-4) = 6.3e-5; larger values mean more of the
# marginal is extrapolated. summary() flags it, and the fit warns, above
# 0.05. INLAvaan's docs give 1e-3 to 1e-2 as the healthy range; its lower
# end fails for healthy fits here (see lab/DESIGN.md), so no range is given.
unscanned_mass <- function(sn, mode, sds) {
  lo <- psnorm(mode - 4 * sds, sn[, "xi"], sn[, "omega"], sn[, "alpha"])
  hi <- psnorm(
    mode + 4 * sds,
    sn[, "xi"],
    sn[, "omega"],
    sn[, "alpha"],
    lower_tail = FALSE
  )
  unname(lo + hi)
}

# How far the straight scan path misses the conditional mode of the other
# parameters: the linear CMP is a first-order approximation to a possibly
# curved ridge (Jamil & Rue, 2026, Section 3.2.1), and this measures the
# curvature it ignores, a check of inlavanilla's own. At scan points t (near
# +-2 posterior SDs), one Newton step in the other parameters, with the
# Laplace curvature, would raise the log-posterior by 0.5 g' C g, where g is
# the gradient in the other parameters and C their conditional covariance
# given theta_j,
#   C = Sigma[-j, -j] - Sigma[-j, j] Sigma[j, -j] / Sigma[j, j].
# That is the height the scan misses there, in log-density units. When it is
# large, the other parameters' conditional mode bends away from the line (as
# a noise scale does for a regression coefficient at small n), and the
# marginal comes out too narrow. Returns the larger of the gains. The
# gradient is the user's, or one-sided differences in the other coordinates
# only (m - 1 calls per point, reusing the scan's height), with their bias,
# half a step times the curvature, removed with the Laplace curvature.
#
# Without a gradient those differences would be about as many calls as the
# volume correction, for a check that most parameters pass easily. So `dir`,
# the direction of the bend from gamma1_lp(), is tried first, at one call per
# point (bend_along()). That gain is a lower bound on the full one, and usually
# close to it. The differences are taken only when it is above `bend_screen`,
# so that every value that can reach the flag is the full one.
scan_bend <- function(j, lp, gr, lap, Vscan, t, f_t, dir = NULL) {
  m <- length(lap$mode)
  if (m < 2L) {
    return(0)
  }
  if (is.null(gr) && !is.null(dir)) {
    along <- bend_along(j, lp, lap, Vscan, t, f_t, dir)
    if (along <= bend_screen) {
      return(along)
    }
  }
  Sigma <- lap$Sigma
  sds <- sqrt(diag(Sigma))
  C <- Sigma[-j, -j, drop = FALSE] -
    tcrossprod(Sigma[-j, j]) / Sigma[j, j]
  others <- seq_len(m)[-j]
  h <- 1e-3 * sds
  gain <- numeric(length(t))
  for (k in seq_along(t)) {
    x <- lap$mode + Vscan[, j] * t[k]
    g <- if (is.null(gr)) {
      vapply(
        others,
        function(i) {
          xp <- x
          xp[i] <- x[i] + h[i]
          (lp(xp) - f_t[k]) / h[i] + 0.5 * h[i] * lap$H[i, i]
        },
        numeric(1)
      )
    } else {
      gr(x)[others]
    }
    gain[k] <- if (all(is.finite(g))) 0.5 * sum(g * (C %*% g)) else Inf
  }
  max(gain)
}

# Below this ridge bend, the gain along one direction is reported without
# the full gradient (scan_bend()). It was the flag level until October 2026:
# in the normal mean with an unknown SD a bend of 0.1 is a marginal SD 4% too
# small (lab/bend-threshold.md). A bend at the flag level escapes the screen
# only if the direction holds less than 40% of its gain, and over 351
# parameters of 28 models one flagged bend did (lab/bend-screen.R).
bend_screen <- 0.1

# The gain of scan_bend() along one direction `dir`, a unit vector in whitened
# coordinates, where the Laplace curvature is 1. One call per point.
bend_along <- function(j, lp, lap, Vscan, t, f_t, dir, h = 1e-3) {
  step <- h * drop(lap$A %*% dir)
  gain <- vapply(
    seq_along(t),
    function(k) {
      d <- (lp(lap$mode + Vscan[, j] * t[k] + step) - f_t[k]) / h + 0.5 * h
      if (is.finite(d)) 0.5 * d^2 else Inf
    },
    numeric(1)
  )
  max(gain)
}
