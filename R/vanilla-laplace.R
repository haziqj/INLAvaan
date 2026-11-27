# Stage 1a: the joint Gaussian (Laplace) approximation N(mode, Sigma) with
# Sigma = H(mode)^-1, and the Laplace estimate of the log marginal likelihood.
#
# `lp`, `gr` and `he` are the wrapped log-posterior, gradient and Hessian
# (`gr` and `he` are NULL when the user gave none). With `he`, nlminb() takes
# Newton steps with it, and the Hessian at the mode is the user's. `he_mode`
# is a Hessian for the mode alone, such as one that is cheap or valid only
# there: it replaces the differences at the mode, once or, after a restart,
# twice, and nlminb() never calls it. Returns
# the mode, the log-posterior there, the upper Cholesky factor R of the
# precision (R'R = H), the covariance Sigma, its square root A = cov_root()
# (A A' = Sigma), the optimiser output, the gradient of the log-posterior at
# the mode with the checks built on it, the condition number of the Hessian
# in posterior-SD units, and the Laplace log marginal likelihood. `gap_calls`
# counts the logpost calls, summed over a restart, taken with a supplied
# Hessian only to check the gradient (grad_gap). For verbose
# output it also says how the Hessian was taken (`hessian`) and whether
# nlminb() was restarted (`restart`, NULL when it was not). After a restart
# the iterations and evaluations of `opt` are the totals over both runs.
laplace_fit <- function(lp, gr, init, ctrl, he = NULL, he_mode = NULL) {
  ob <- function(x) {
    v <- lp(x)
    if (is.finite(v)) -v else Inf
  }
  gro <- if (is.null(gr)) NULL else function(x) -gr(x)
  heo <- if (is.null(he)) NULL else function(x) -he(x)
  opt <- stats::nlminb(
    start = unname(init),
    objective = ob,
    gradient = gro,
    hessian = heo,
    control = mode_nlminb(ctrl)
  )
  # The Hessian at the mode, and how it was taken
  at_mode <- if (is.null(he_mode)) he else he_mode
  by <- if (!is.null(he_mode)) {
    "hess_mode"
  } else if (!is.null(he)) {
    "hess"
  } else if (is.null(gr)) {
    "logpost"
  } else {
    "grad"
  }
  cur <- laplace_at(lp, gr, opt$par, -opt$objective, names(init), at_mode, by)
  gap_calls <- cur$hessian$gap_calls

  # nlminb() works on the raw parameters, and its convergence tests assume
  # they are on a unit scale. Far from that (posterior SDs of 1e4, or 1e-6)
  # it can stop well short of the mode, reporting success or false
  # convergence. When the whitened gradient says the mode is off, restart
  # from there with each parameter scaled by its curvature.
  restart <- NULL
  if (is.null(cur$R) || cur$grad_mode > 1e-3) {
    scale <- if (is.null(cur$R)) 1 / cur$scales else sqrt(diag(cur$H))
    opt2 <- stats::nlminb(
      start = opt$par,
      objective = ob,
      gradient = gro,
      hessian = heo,
      scale = scale,
      control = mode_nlminb(ctrl)
    )
    restart <- list(
      grad_mode = cur$grad_mode,
      iterations = opt$iterations,
      restart_iterations = opt2$iterations,
      kept = opt2$objective <= opt$objective
    )
    # The iterations and evaluations reported are the work of both runs,
    # whichever is kept. The return code and the message are those of the run
    # that is kept, and `restart` holds each run's own iterations.
    iterations <- opt$iterations + opt2$iterations
    evaluations <- opt$evaluations + opt2$evaluations
    if (restart$kept) {
      opt <- opt2
      cur <- laplace_at(
        lp,
        gr,
        opt$par,
        -opt$objective,
        names(init),
        at_mode,
        by
      )
      gap_calls <- gap_calls + cur$hessian$gap_calls
    }
    opt$iterations <- iterations
    opt$evaluations <- evaluations
  }

  if (is.null(cur$R)) {
    H <- cur$H
    problem <- if (all(is.finite(H))) {
      "is not positive definite"
    } else {
      "has values that are not finite"
    }
    abort(
      c(
        "The Hessian at the posterior mode {problem}.",
        if (!is.null(gr) && is.null(he)) grad_sign_hint(H, cur$scales),
        "i" = "The optimiser may have stopped short of the mode, or the
               posterior may be improper or not identified.",
        "i" = "Optimiser message: {.val {opt$message}}."
      )
    )
  }
  m <- length(init)
  # Two condition numbers of the Hessian at the mode, the eigenvalue ratio of
  # its inverse. `hess_cond` is INLAvaan's, on the raw scale: that of Sigma,
  # which a mere change of units can make large (posterior SDs of 1e-6 and 1e4
  # give 1e20). `hess_cond_cor` has each parameter in its own posterior-SD
  # units, D^1/2 H D^1/2 with D = diag(Sigma): that of the correlation matrix
  # cov2cor(Sigma), free of units, which the fit-time warning checks.
  cond <- function(S) {
    ev <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
    if (min(ev) > 0) max(ev) / min(ev) else Inf
  }
  c(
    cur,
    list(
      opt = opt,
      restart = restart,
      hess_cond = cond(cur$Sigma),
      hess_cond_cor = cond(stats::cov2cor(cur$Sigma)),
      gap_calls = gap_calls,
      # log pi(y) ~ log pi(y, mode) - log N(mode | mode, Sigma), with
      # log det(Sigma) = -2 sum(log(diag(R)))
      logml = cur$lp_max + (m / 2) * log(2 * pi) - sum(log(diag(cur$R)))
    )
  )
}

# The bullet for a Hessian error caused by a gradient of the wrong sign, the
# easiest mistake with a function that returns minus the log-posterior: the
# curvature from `grad` is minus that from `logpost`. NULL otherwise.
grad_sign_hint <- function(H, scales) {
  ratio <- (diag(H) * as.numeric(scales)^2)[attr(scales, "ok")]
  if (length(ratio) == 0L || !all(is.finite(ratio) & ratio < 0)) {
    return(NULL)
  }
  c(
    "x" = "{.arg grad} looks like it has the wrong sign: it must be the
           gradient of {.arg logpost}, so if you negate one, negate the other."
  )
}

# Hessian, its Cholesky factor, the covariance and the whitened gradient at
# `mode`. With `he` the Hessian is the user's (`by` says from which of its
# functions), checked against the gradient by hess_discrepancy(). Otherwise
# finite-difference steps are set per coordinate from fd_scales(), so they suit
# each parameter's own scale: central differences of the gradient at 1e-4 of
# that scale, or, without a gradient, second differences of the log-posterior at
# a hundredth of it. R is NULL when the Hessian is not positive definite.
# fd_scales() runs with `he` too, since its differences of `logpost` give
# `grad_gap`, which compares a user gradient, and the diagonal of the Hessian,
# with `logpost` there: NA without a gradient. With `he` its search starts from
# the Hessian's diagonal, so it usually takes 2 calls per parameter, against 6
# to 8 from its default start. `hessian` records the method and
# its calls, those of fd_scales() counted apart. `gap_calls` counts the calls of
# fd_scales() taken only for `grad_gap`, those with a supplied Hessian whose
# diagonal gives the scales.
laplace_at <- function(
  lp,
  gr,
  mode,
  lp_max,
  parnames,
  he = NULL,
  by = if (is.null(he)) {
    if (is.null(gr)) "logpost" else "grad"
  } else {
    "hess"
  }
) {
  m <- length(mode)
  mode <- stats::setNames(mode, parnames)
  if (is.null(he)) {
    fds <- fd_scales(lp, mode, lp_max)
    scales <- fds
    H <- if (is.null(gr)) {
      -fd_hessian(lp, mode, h = 0.01 * scales, f0 = lp_max)
    } else {
      -fd_jacobian(gr, mode, h = 1e-4 * scales)
    }
  } else {
    H <- -he(mode)
    # The curvature scales, which only rescale a restart of nlminb(), from
    # the Hessian's own diagonal where it allows
    d <- diag(H)
    from_diag <- all(is.finite(d) & d > 0)
    # The step search then serves grad_gap alone. The diagonal gives each
    # conditional SD 1 / sqrt(H_jj), so the search starts at a tenth of it,
    # where the drop is about 0.005, and a right diagonal ends the search at
    # the first try: 2 calls per parameter. That is about where the search
    # from its default start ends, so grad_gap reads the same. A coordinate
    # whose diagonal is more than 5 times too large, or far too small, leaves
    # the window and searches on.
    fds <- fd_scales(lp, mode, lp_max, h0 = if (from_diag) 0.1 / sqrt(d))
    scales <- if (from_diag) {
      structure(1 / sqrt(d), ok = rep(TRUE, m))
    } else {
      fds
    }
  }
  # The logpost calls of fd_scales() taken only for grad_gap, which the VB
  # budget leaves out: all of them when the scales come from the Hessian
  gap_calls <- if (!is.null(he) && from_diag) attr(fds, "calls") else 0L
  # A Hessian is symmetric, so an asymmetry relative to the curvatures, the
  # largest |H_ij - H_ji| / sqrt(H_ii H_jj), means an error: in the gradient
  # whose Jacobian it is, or in a supplied Hessian, which is also checked
  # against the gradient by hess_discrepancy(). It is measured on every route
  # (second differences of logpost are symmetric by construction), and the
  # Hessian is then symmetrised.
  d <- sqrt(abs(diag(H)))
  asymmetry <- if (all(is.finite(H)) && all(d > 0)) {
    max(abs(H - t(H)) / outer(d, d))
  } else {
    0
  }
  H <- 0.5 * (H + t(H))
  dimnames(H) <- list(parnames, parnames)
  R <- chol_or_null(H)
  out <- list(
    mode = mode,
    lp_max = lp_max,
    H = H,
    R = R,
    scales = scales,
    asymmetry = asymmetry,
    hess_error = NA_real_,
    grad_gap = stats::setNames(rep(NA_real_, m), parnames),
    grad_mode = Inf,
    hessian = list(
      by = by,
      calls = if (!is.null(he)) {
        1L
      } else if (is.null(gr)) {
        m^2 + m
      } else {
        2 * m
      },
      step_calls = attr(fds, "calls"),
      check_calls = 0L,
      gap_calls = gap_calls
    )
  )
  if (is.null(R)) {
    return(out)
  }
  out$Sigma <- chol2inv(R)
  dimnames(out$Sigma) <- list(parnames, parnames)
  # The one whitening factor of every stage, which does not depend on the
  # order of the parameters (cov_root())
  out$A <- cov_root(out$Sigma)
  if (!is.null(he)) {
    out$hess_error <- hess_discrepancy(gr, mode, out$A)
    out$hessian$check_calls <- attr(out$hess_error, "calls")
  }
  # The gradient of the log-posterior at the mode, and two measures of how
  # far a Newton step would still move the mode. `grad_mode`, the largest
  # whitened component, decides the restart in laplace_fit().
  # `mode_shift` is INLAvaan's per-parameter Newton step in posterior SDs,
  # |(Sigma g)_j| / sd_j, which decides the fit-time check; it does not
  # depend on the order of the parameters.
  sds <- sqrt(diag(out$Sigma))
  g <- if (is.null(gr)) {
    fd_grad(lp, mode, h = 1e-3 * sds)
  } else {
    gr(mode)
  }
  out$grad <- stats::setNames(as.numeric(g), parnames)
  if (!is.null(gr)) {
    out$grad_gap[] <- grad_gap(as.numeric(g), H, fds)
  }
  out$grad_mode <- max(abs(crossprod(out$A, g)))
  out$mode_shift <- stats::setNames(
    abs(as.numeric(out$Sigma %*% g)) / sds,
    parnames
  )
  out
}

# How far a user Hessian is from central differences of the gradient at the
# mode, in whitened coordinates, where the Hessian is the identity: the
# largest error in A'(-J)A w, for J the Jacobian of the gradient along A w,
# over two fixed unit directions w (all ones, and alternating signs). It
# costs 4 gradient calls whatever m, against 2m for a check of each column,
# and a wrong Hessian passes only if its error happens to vanish along both
# directions. Not finite when the gradient is not finite there.
hess_discrepancy <- function(gr, mode, A, h = 1e-4) {
  m <- length(mode)
  W <- unique(cbind(rep(1, m), rep_len(c(1, -1), m)), MARGIN = 2) / sqrt(m)
  err <- 0
  for (k in seq_len(ncol(W))) {
    u <- drop(A %*% W[, k])
    jl <- (gr(mode + h * u) - gr(mode - h * u)) / (2 * h)
    err <- max(err, abs(drop(crossprod(A, -jl)) - W[, k]))
  }
  structure(err, calls = 2L * ncol(W))
}

# A curvature scale for each coordinate: roughly the conditional posterior SD
# 1 / sqrt(H_ii), found by scaling a step by tens until the symmetric drop
# f(x) - (f(x + h e_i) + f(x - h e_i)) / 2 = H_ii h^2 / 2 lies between 1e-3
# and 1, a few calls per coordinate. A fixed step fails at the extremes: for
# a posterior SD of 1e4 the second difference at h = 1e-4 drowns in rounding
# error, and for an SD of 1e-6 it measures far beyond the quadratic region.
# The attribute `ok` marks the coordinates where the search succeeded, `slope`
# holds the central difference of `fn` on the last step tried, which
# grad_gap() compares with a user gradient, and `calls` counts the calls of
# `fn`. `h0` holds the first step of each coordinate, by default 1e-4 of the
# parameter's size (at least 1e-4). A start near the scale itself, such as one
# from a supplied Hessian's diagonal, usually ends the search at the first try.
fd_scales <- function(fn, x, f0, h0 = NULL) {
  m <- length(x)
  s <- numeric(m)
  slope <- numeric(m)
  ok <- logical(m)
  calls <- 0L
  for (i in seq_len(m)) {
    h <- if (is.null(h0)) 1e-4 * max(1, abs(x[[i]])) else h0[[i]]
    drop <- NA_real_
    for (k in 1:30) {
      xp <- xm <- x
      xp[i] <- x[i] + h
      xm[i] <- x[i] - h
      fp <- fn(xp)
      fm <- fn(xm)
      drop <- f0 - 0.5 * (fp + fm)
      slope[i] <- (fp - fm) / (2 * h)
      calls <- calls + 2L
      if (!is.finite(drop) || drop > 1) {
        h <- h / 10 # too far: out of the quadratic region or the support
      } else if (drop < 1e-3) {
        h <- h * 10 # too close: lost in rounding error
      } else {
        break
      }
    }
    ok[i] <- is.finite(drop) && drop >= 1e-3 && drop <= 1
    s[i] <- if (is.finite(drop) && drop > 0) h / sqrt(2 * drop) else h
  }
  # `ok` is FALSE where no step gave a drop in the window, such as where the
  # log-posterior is not locally concave
  structure(s, ok = ok, slope = slope, calls = calls)
}

# How far a user gradient is from `logpost` at the mode, for each parameter. It
# costs no calls of its own: fd_scales() has taken symmetric differences of
# `logpost` there, for the steps of the Hessian, or with a supplied Hessian for
# this check alone. They give the slope along each coordinate and the curvature
# 1 / s_j^2, against the gradient `g` and the Hessian `H` taken from the user's
# gradient. The gap is the larger of the difference in slope per conditional
# posterior SD, about how far it would move the mode in SDs, and half the
# relative difference in curvature, about the relative error it makes in an SD.
# NA where the search for the step failed.
grad_gap <- function(g, H, scales) {
  gap <- pmax(
    abs(g - attr(scales, "slope")) * as.numeric(scales),
    0.5 * abs(diag(H) * as.numeric(scales)^2 - 1)
  )
  gap[!attr(scales, "ok")] <- NA_real_
  gap
}

chol_or_null <- function(H) {
  if (!all(is.finite(H))) {
    return(NULL)
  }
  tryCatch(chol(H), error = function(e) NULL)
}
