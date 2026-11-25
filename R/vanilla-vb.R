# Stage 1b: the variational Bayes mean correction. The Laplace Gaussian is
# recentred at mode + shift, keeping its covariance, where the shift maximises
# the Gaussian expectation of the log-posterior. At the optimum the expected
# gradient vanishes, E[grad log pi(mode + shift + z)] = 0 for z ~ N(0, Sigma).
#
# There are two ways to solve it. vb_correction = "full" iterates to the
# solution: a fixed-point iteration with Anderson(1) acceleration and an
# nlminb() fallback, on a degree-5 cubature rule built on a regular simplex
# (vb_nodes()). With a gradient it uses the expected gradient over the nodes.
# Without one it uses Stein's identity on the same nodes, from log-posterior
# values alone.
#
# vb_correction = "onestep", the default, takes the first step of that iteration
# and stops. The fixed-point map is nearly flat (its slope is O(1/n)), so the
# first step holds most of the movement, and exact VB is itself only O(n^-3/2)
# posterior SDs from the posterior mean. A second sweep at the step then checks
# that the fixed-point map contracts there, which is what the one step assumes
# (vb_checked_onestep()). Where it does not, a quasi-Newton solve on the same
# sweeps takes over, capped by the calls of the mode and Hessian stage. With a
# gradient a sweep is 2m + 1 gradients on the axes of cov_root()
# (axis_nodes()), about one numerical Hessian. Without one it is one Stein
# pass on vb_nodes(), since Stein's identity needs that rule's degree. The
# check, the solve and the warning are the same code on both routes. See
# lab/ideas/2026-11-one-step-vb.

# Node set for the Gaussian expectation: a degree-5 rule built on a regular
# simplex, in the whitened coordinates of cov_root(). There its nodes are the
# centre and
#   +/- r a_k    at the m + 1 vertices a_k of a regular simplex (unit
#                vectors with a_k' a_l = -1/m),
#   +/- s b_kl   along the m (m + 1) / 2 edge directions
#                b_kl = (a_k + a_l) / |a_k + a_l|,
# (m + 1)(m + 2) nodes besides the centre, about half the 2m^2 of the fully
# symmetric degree-5 rule on the principal axes, which the package used
# before. The node set is invariant under the symmetries of the simplex and
# under -I, so by Sobolev's theorem the rule integrates every polynomial of
# degree five or less exactly once it integrates the invariant ones: 1,
# |z|^2, |z|^4 and p4(z) = sum_k (a_k' z)^4, whose Gaussian moments are 1, m,
# m(m + 2) and 3(m + 1). Given the radii, these fix the total weight of each
# shell. While the vertex shell's weight is not negative (m <= 7) both shells
# sit on the sphere of radius sqrt(m + 2), and every weight is positive.
# Beyond that the vertex weights are negative whatever the radii; r^2 = 3
# then keeps them small, and s^2 falls from 8.4 (m = 8) towards 6. At m = 7
# the vertex weight is exactly zero and those nodes are dropped. No node is
# more than 3 posterior SDs from the centre. Some weights are negative, so
# sums over the nodes are taken as differences, which keeps large terms from
# cancelling.
#
# For m <= 3 some directions coincide (a_2 = -a_1 at m = 1, a_k + a_l = -a_j
# at m = 2, a_1 + a_2 = -(a_3 + a_4) at m = 3), and each node is kept once.
# This gives the classical rules: the three-point Gauss-Hermite rule (m = 1),
# Radon's seven-point rule on a hexagon (m = 2), and the corners of a cube
# with the vertices of an octahedron (m = 3).
#
# The nodes come in antipodal pairs: the centre, then one node of each pair,
# then their negatives in the same order. The simplex is placed so that a
# reordering of the parameters only permutes its vertices (simplex_rule()), and
# a reordering permutes the rows and the columns of cov_root() alike, so the
# nodes of a reordered posterior are the reordered nodes.
vb_nodes <- function(Sigma, A = cov_root(Sigma)) {
  rule <- simplex_rule(ncol(Sigma))
  half <- rule$nodes[1 + seq_len(rule$n_pairs), , drop = FALSE] %*% t(A)
  list(nodes = rbind(0, half, -half), weights = rule$weights)
}

# The node set of the one-step correction with a gradient: the centre and
# +/- sqrt(3) along each axis of cov_root(Sigma), 2m + 1 nodes, as many
# gradients as a numerical Hessian. Written as differences from the centre,
# the expected gradient is g0 + sum_k (g(+a_k) + g(-a_k) - 2 g0) / 6. In one
# dimension this is the three-point Gauss-Hermite rule, as vb_nodes() is. The
# rule is exact for polynomials of degree three, so the gradient of a quartic
# log-posterior is averaged exactly, and the leading shift, half the trace of
# the third derivatives, is exact on a cubic. A wider rule did not help: it
# picks up the tails, and the first step then moves further from the
# posterior mean (lab/ideas/2026-11-one-step-vb).
axis_nodes <- function(Sigma, A = cov_root(Sigma)) {
  m <- ncol(Sigma)
  half <- sqrt(3) * t(A)
  list(
    nodes = rbind(0, half, -half),
    weights = c(1 - m / 3, rep(1 / 6, 2 * m))
  )
}

# The whitening factor of every stage, A with A A' = Sigma: the symmetric
# square root of the correlation matrix C, scaled by the posterior SDs,
# A = D C^1/2. A reordering of the parameters permutes its rows and columns
# alike, and a change of units scales its rows, so no stage depends on the
# order or the units. Stages whitened by a Cholesky factor depended on the
# order. It is a smooth function of Sigma, unlike the principal axes, whose
# directions are arbitrary where eigenvalues are equal, as with exchangeable
# random effects: there rounding error alone moved the VB mean by 0.02 SDs. It
# is also accurate when the SDs span many orders of magnitude: with SDs from
# 1e-6 to 1e4 it reproduces Sigma to 1e-15, the principal axes of Sigma to
# 1.6% only (lab/whitening-order.R and lab/DESIGN.md).
cov_root <- function(Sigma) {
  sds <- sqrt(diag(Sigma))
  e <- eigen(Sigma / outer(sds, sds), symmetric = TRUE)
  root <- e$vectors %*% (sqrt(pmax(e$values, 0)) * t(e$vectors))
  unname(sds * root)
}

# The rule of vb_nodes() in whitened coordinates
simplex_rule <- function(m) {
  # Unit vertices, a_k' a_l = -1/m, placed so that a reordering of the
  # coordinates only permutes them: a e_k + b 1 for k = 1, ..., m, and the
  # last on the diagonal
  a <- sqrt((m + 1) / m)
  b <- (1 / sqrt(m) - a) / m
  vertices <- rbind(a * diag(m) + b, rep(-1 / sqrt(m), m))
  p4 <- function(x) sum(as.numeric(vertices %*% x)^4)
  # One vertex and one edge direction per antipodal pair
  V <- if (m == 1) vertices[1, , drop = FALSE] else vertices
  edges <- if (m >= 4) {
    utils::combn(m + 1, 2)
  } else if (m == 3) {
    rbind(1, 2:4)
  } else {
    matrix(0L, 2, 0)
  }
  B <- vertices[edges[1, ], , drop = FALSE] +
    vertices[edges[2, ], , drop = FALSE]
  B <- B / sqrt(rowSums(B^2))

  # The weights of the two shells, W1 = a4 / r^4 and W2 = b4 / s^4
  if (nrow(B) == 0) {
    r2 <- s2 <- m + 2
    a4 <- m * (m + 2)
    b4 <- 0
  } else {
    alpha <- p4(V[1, ])
    beta <- p4(B[1, ])
    a4 <- (3 * (m + 1) - beta * m * (m + 2)) / (alpha - beta)
    b4 <- m * (m + 2) - a4
    if (a4 > -1e-8) {
      r2 <- s2 <- m + 2
    } else {
      r2 <- 3
      s2 <- b4 / (m - a4 / r2)
    }
  }
  w1 <- a4 / r2^2 / (2 * nrow(V))
  w2 <- if (nrow(B) > 0) b4 / s2^2 / (2 * nrow(B)) else numeric()
  half <- rbind(sqrt(r2) * V, sqrt(s2) * B)
  w <- c(rep(w1, nrow(V)), rep(w2, nrow(B)))
  keep <- abs(w) > 1e-12
  half <- half[keep, , drop = FALSE]
  w <- w[keep]
  list(
    nodes = rbind(0, half, -half),
    weights = c(1 - 2 * sum(w), w, w),
    n_pairs = nrow(half)
  )
}

# The largest fixed-point step, in posterior SDs (see vb_correct())
vb_maxstep <- 1.5

# The step of the directional differences of vb_slope_along(), in the
# whitened coordinates of cov_root()
vb_slope_step <- 1e-3

# The cap on the sweeps of the checked one step, the two sweeps of its check
# among them: the calls of the mode and Hessian stage (`budget`) over 2m + 1,
# the calls of one sweep with a gradient. With a gradient the VB stage then
# stays within the calls of that stage. The same count of sweeps holds without
# a gradient (the owner's, 2026-11-25), where a sweep is a Stein pass of
# (m + 1)(m + 2) calls: a flagged fit can then spend up to
# (m + 1)(m + 2) / (2m + 1) times the calls of that stage, about m / 2, which
# is the price of a fit without a gradient. The two sweeps of the check are
# always taken.
vb_max_sweeps <- function(budget, m) {
  floor(budget / (2 * m + 1))
}

# One sweep with a gradient, on the axis rule at a shift: the gradient at
# mode + shift and at +/- sqrt(3) along each axis of A, 2m + 1 calls. `step`
# is the fixed-point step there, Sigma times the expected gradient with the
# step of the fixed parameters at zero. `resid` is that step in the whitened
# coordinates of A, where the Laplace Gaussian is N(0, I): the residual F(d)
# of the VB equation F(d) = 0. `curv` estimates the expected Hessian there
# from the same gradients. By Stein's identity
# E[U_k grad(d + U)] = E[Hessian(d + U)] e_k, and the three-point rule along
# axis k gives the left side as (g(+k) - g(-k)) / (2 sqrt(3)). It holds the
# part of the expected Hessian along the axes only, with no mixed fourth
# derivatives.
vb_sweep_axis <- function(gr, mode, shift, rule, Sigma, A, A_inv, fixed) {
  Z <- rule$nodes
  w <- rule$weights
  nn <- nrow(Z)
  m <- length(mode)
  mu <- mode + shift
  g0 <- gr(mu)
  s <- g0
  G <- matrix(0, nn, m)
  for (b in 2:nn) {
    G[b, ] <- gr(mu + Z[b, ])
    s <- s + w[b] * (G[b, ] - g0)
  }
  step <- as.numeric(Sigma %*% s)
  step[fixed] <- 0
  plus <- 1L + seq_len(m)
  across <- G[plus, , drop = FALSE] - G[m + plus, , drop = FALSE]
  curv <- crossprod(A, t(across)) / (2 * sqrt(3))
  list(
    step = step,
    resid = as.numeric(A_inv %*% step),
    curv = (curv + t(curv)) / 2,
    finite = all(is.finite(step))
  )
}

# One sweep without a gradient: a pass of Stein's identity on vb_nodes() at a
# shift, E[z f(mu + z)] = Sigma E[grad f(mu + z)] for z ~ N(0, Sigma), summed
# over antipodal pairs as w_b z_b (f(mu + z_b) - f(mu - z_b)), which cancels
# the size of the log-posterior. At most (m + 1)(m + 2) values of the
# log-posterior. `step` and `resid` are as in vb_sweep_axis(). There is no
# `curv`: the slope along the step comes from vb_slope_along().
vb_sweep_stein <- function(lp, mode, shift, rule, A_inv, fixed) {
  Z <- rule$nodes
  nn <- nrow(Z)
  n_pairs <- (nn - 1L) %/% 2L
  plus <- seq_len(n_pairs)
  mu <- mode + shift
  v <- numeric(nn - 1L)
  for (b in 2:nn) {
    v[b - 1L] <- lp(mu + Z[b, ])
  }
  Zp <- Z[1L + plus, , drop = FALSE]
  wp <- rule$weights[1L + plus]
  step <- as.numeric(crossprod(Zp, wp * (v[plus] - v[n_pairs + plus])))
  step[fixed] <- 0
  list(
    step = step,
    resid = as.numeric(A_inv %*% step),
    finite = all(is.finite(v)) && all(is.finite(step))
  )
}

# The slope of the fixed-point map along a whitened unit direction u at a
# shift, without a gradient: u'(I + curv) u with `curv` the estimate of
# vb_sweep_axis(), which needs only the gradient along u at each axis node.
# That is a central difference of the log-posterior, `h` apart in whitened
# units: 4 values per axis, 4m in all. The second-order Stein identity on the
# values of vb_sweep_stein() would cost nothing, but it is not exact at
# leading order under a degree-5 rule.
vb_slope_along <- function(lp, mode, shift, u, A, h = vb_slope_step) {
  mu <- mode + shift
  hu <- h * as.numeric(A %*% u)
  along <- function(x) (lp(x + hu) - lp(x - hu)) / (2 * h)
  total <- 0
  for (k in which(u != 0)) {
    axis <- sqrt(3) * A[, k]
    total <- total +
      u[k] * (along(mu + axis) - along(mu - axis)) / (2 * sqrt(3))
  }
  1 + total
}

# The check of the one step d1 = F(0), from the sweep at the mode and the
# sweep at d1. The one step is the first iterate of the fixed-point map
# phi(d) = d + F(d), and its theory needs phi to be a contraction near the
# mode. By the contraction mapping theorem the iteration then has one root
# near the step, within L / (1 - L) |d1| of it, for a contraction constant
# L < 1. The two sweeps measure this, by the same rule over the same width as
# the step itself:
#   lhat = |F(d1)| / |d1|, the contraction ratio of phi over the step, since
#          phi(d1) - phi(0) = F(d1). Below 1 the step reduces the residual of
#          the VB equation.
#   j0   = the slope of phi along u = d1 / |d1| at the mode, u'(I + curv) u
#   j1   = the same slope at the step
# `slope(sw, u)` gives the slope at a sweep: from its `curv` with a gradient,
# and from vb_slope_along() without one. lhat compares two points only, and
# misses a map that is steep near the mode and flat beyond, so the slope is
# checked at both ends. The check passes when all three are finite, lhat is
# below 1, and j0 and j1 lie in (-1, 1). A value that is not finite is kept as
# NaN or Inf, never NA, which means a check that did not run. The slope at a
# step whose sweep is not finite is not taken, and is NaN.
vb_onestep_check <- function(sw0, sw1, slope) {
  d1 <- sw0$resid
  len <- sqrt(sum(d1^2))
  u <- d1 / len
  vals <- c(
    lhat = sqrt(sum(sw1$resid^2)) / len,
    j0 = slope(sw0, u),
    j1 = if (sw1$finite) slope(sw1, u) else NaN
  )
  vals[is.na(vals)] <- NaN
  list(
    vals = vals,
    pass = all(is.finite(vals)) && all(abs(vals) < 1),
    u = u
  )
}

# Broyden's quasi-Newton method for the VB equation F(d) = 0, in whitened
# coordinates, on the sweeps of either route. F is the expected gradient of
# the log-posterior under the shifted Gaussian, so the solve is a maximisation
# of the expected log-posterior, and its line search needs only the
# directional derivative F(d + t p)'p, which the sweeps give. `sweep_at(d)` is
# a sweep at d with `d` added to it, `start` the sweep at the mode, and `B`
# the first estimate of the Jacobian of F.
# Every finite sweep then gives a secant pair (s, y) and the rank-one update
# B + (y - B s) s' / s's, so each curvature the solve uses is measured by the
# same rule over the same width as the gradient.
#
# Each iteration takes p = -B^-1 F, or the plain fixed-point step F when that
# is not an ascent direction, and searches t along it for the strong Wolfe
# curvature condition |F(d + t p)'p| <= 0.9 F(d)'p: by regula falsi on the
# directional derivative once a change of sign brackets the maximum along p,
# and by doubling t before that. A trial that is not finite halves t. The
# solve stops when the step still to go is below `tol` posterior SDs
# (`step_sd()`), after `maxit` iterations, or when a line search ends with no
# point accepted, because `room()` says that the cap on the sweeps is reached
# or `maxit` trials have gone. It returns the last accepted point.
vb_broyden <- function(sweep_at, start, B, room, project, step_sd, tol, maxit) {
  m <- length(start$d)
  cur <- start
  steps <- step_sd(cur)
  it <- 0L
  while (it < maxit && step_sd(cur) >= tol) {
    p <- tryCatch(
      project(-solve(B, cur$resid)),
      error = function(e) rep(NaN, m)
    )
    slope0 <- sum(cur$resid * p)
    if (!all(is.finite(p)) || slope0 <= 0) {
      p <- cur$resid
      slope0 <- sum(p^2)
    }
    accepted <- FALSE
    lo <- 0
    slope_lo <- slope0
    hi <- Inf
    slope_hi <- NA_real_
    t <- 1
    trials <- 0L
    while (trials < maxit && room()) {
      trials <- trials + 1L
      new <- sweep_at(cur$d + t * p)
      if (!new$finite) {
        hi <- t
        slope_hi <- NA_real_
        t <- (lo + t) / 2
        next
      }
      s <- new$d - cur$d
      y <- new$resid - cur$resid
      B <- B + outer(y - as.numeric(B %*% s), s) / sum(s^2)
      slope_t <- sum(new$resid * p)
      if (abs(slope_t) <= 0.9 * slope0) {
        cur <- new
        accepted <- TRUE
        break
      }
      if (slope_t < 0) {
        hi <- t
        slope_hi <- slope_t
      } else {
        lo <- t
        slope_lo <- slope_t
      }
      t <- if (is.finite(hi) && is.finite(slope_hi)) {
        lo + (hi - lo) * slope_lo / (slope_lo - slope_hi)
      } else if (is.finite(hi)) {
        (lo + hi) / 2
      } else {
        2 * t
      }
    }
    if (!accepted) {
      break
    }
    it <- it + 1L
    steps <- c(steps, step_sd(cur))
  }
  list(
    point = cur,
    iterations = it,
    steps = steps,
    converged = step_sd(cur) < tol
  )
}

# The checked one step: vb_correction = "onestep", with or without a gradient.
# It takes the one step d1 from a sweep at the mode, and a second sweep at d1
# for the check (vb_onestep_check()). With a gradient (`gr`) a sweep is
# vb_sweep_axis(), and the check costs 4m + 2 gradient calls. Without one
# (`gr = NULL`) a sweep is vb_sweep_stein() and the two slopes come from
# vb_slope_along(): two Stein passes and about 8m values of the log-posterior.
# Everything else is the same code. When the check passes, the shift is the
# one step, unchanged. When it fails, the posterior is far from Gaussian along
# the step and the one step is not an approximation of the VB mean, so the VB
# equation is solved by vb_broyden(), and `solve` is TRUE. The caller warns
# about every such fit, also when the solve converged, since the flag is about
# the posterior and not about the solver.
#
# The solve starts from the mode, not from d1, which lies outside the region
# where the check found a contraction and can be far out when a node touched
# the edge of the support. Its first Jacobian is -I + j0 u u': the Laplace
# curvature, with the slope measured along the step in place of the Laplace
# one. The sweeps of the stage are capped at vb_max_sweeps(), the two of the
# check among them. At the cap the last accepted point stands, the mode itself
# when no sweep was left for the solve. `step_sigma` is then the step still to
# go there, from the sweep that accepted it. After a passed check it is NA, as
# the step is not iterated, and it is not a flag after a solve either.
#
# A first step below `vb_tol` in every parameter is taken with no second
# sweep: the solve would stop at the mode, less than `vb_tol` away, so there
# is nothing to check, and the ratio of two residuals that small is rounding
# error. `solve` and the three numbers of the check are then NA. Returns NULL
# when the sweep at the mode is not finite, which leaves no step to check:
# vb_correct() then goes on to its fallback.
vb_checked_onestep <- function(lp, gr, mode, Sigma, R, A, ctrl, budget, fixed) {
  m <- length(mode)
  sds <- sqrt(diag(Sigma))
  stein <- is.null(gr)
  rule <- if (stein) vb_nodes(Sigma, A) else axis_nodes(Sigma, A)
  A_inv <- crossprod(A, crossprod(R))
  max_sweeps <- vb_max_sweeps(budget, m)
  sweeps <- 0L
  to_shift <- function(d) {
    shift <- as.numeric(A %*% d)
    shift[fixed] <- 0
    shift
  }
  sweep_at <- function(d, shift = to_shift(d)) {
    sweeps <<- sweeps + 1L
    sw <- if (stein) {
      vb_sweep_stein(lp, mode, shift, rule, A_inv, fixed)
    } else {
      vb_sweep_axis(gr, mode, shift, rule, Sigma, A, A_inv, fixed)
    }
    c(list(d = d, shift = shift), sw)
  }
  slope <- function(sw, u) {
    if (stein) {
      vb_slope_along(lp, mode, sw$shift, u, A)
    } else {
      1 + sum(u * as.numeric(sw$curv %*% u))
    }
  }
  step_sd <- function(sw) {
    move <- max(abs(sw$step) / sds)
    if (is.finite(move)) move else Inf
  }

  sw0 <- sweep_at(numeric(m), numeric(m))
  if (!sw0$finite) {
    return(NULL)
  }
  shift <- numeric(m) + sw0$step
  steps <- step_sd(sw0)
  iterations <- 1L
  check <- c(lhat = NA_real_, j0 = NA_real_, j1 = NA_real_)
  solve <- NA
  settled <- NA
  step_sigma <- rep(NA_real_, m)
  if (steps >= ctrl$vb_tol) {
    chk <- vb_onestep_check(sw0, sweep_at(sw0$resid, shift), slope)
    check <- chk$vals
    solve <- !chk$pass
  }
  if (isTRUE(solve)) {
    B <- -diag(m)
    if (is.finite(check[["j0"]]) && check[["j0"]] < 1) {
      B <- B + check[["j0"]] * tcrossprod(chk$u)
    }
    br <- vb_broyden(
      sweep_at,
      start = sw0,
      B = B,
      room = function() sweeps < max_sweeps,
      project = function(d) {
        if (length(fixed) == 0L) d else as.numeric(A_inv %*% to_shift(d))
      },
      step_sd = step_sd,
      tol = ctrl$vb_tol,
      maxit = ctrl$vb_maxit
    )
    shift <- to_shift(br$point$d)
    steps <- br$steps
    iterations <- br$iterations
    settled <- br$converged
    step_sigma <- abs(br$point$step) / sds
    step_sigma[!is.finite(step_sigma)] <- Inf
  }

  names(shift) <- names(mode)
  list(
    shift = shift,
    iterations = iterations,
    fallback = FALSE,
    one_pass = !isTRUE(solve),
    solve = solve,
    check = check,
    # The parameter that the one step moves furthest, in posterior SDs, which
    # the caller's warning names
    check_worst = names(mode)[which.max(abs(sw0$step) / sds)],
    step_sigma = stats::setNames(step_sigma, names(mode)),
    history = list(
      steps = steps,
      cut = logical(length(steps)),
      accelerated = logical(length(steps)),
      reason = NULL,
      nlminb = NULL,
      rerun = FALSE,
      # The sweeps of the stage, the check's among them, and their cap
      sweeps = sweeps,
      max_sweeps = max_sweeps,
      settled = settled
    ),
    kld_global = 0.5 * sum((R %*% shift)^2)
  )
}

# `method` is "onestep" or "full". "onestep" is vb_checked_onestep(), whose
# sweeps `budget`, the calls of the mode and Hessian stage, caps
# (vb_max_sweeps()). Of the code below it reaches only the fallback, and only
# when its sweep at the mode is not finite.
# `fixed` holds the parameters whose shift is known to be zero, those with
# Gaussian marginals (the `gaussian_marginals` of vanilla_core()): their step
# is set to zero at every iteration, and their shift after the fallback, with
# the nodes still on all m axes. That is exact when the log-posterior is
# quadratic along those axes and they are nearly separate from the others, as
# INLAvaan's saturated means are.
vb_correct <- function(
  lp,
  gr,
  mode,
  Sigma,
  R,
  A,
  ctrl,
  method = "full",
  budget = Inf,
  fixed = integer()
) {
  m <- length(mode)
  stein <- is.null(gr)
  onestep <- method == "onestep"
  rule <- if (onestep && !stein) axis_nodes(Sigma, A) else vb_nodes(Sigma, A)
  Z <- rule$nodes
  w <- rule$weights
  nn <- nrow(Z)
  sds <- sqrt(diag(Sigma))

  # The log-posterior at every node but the centre. Without a gradient the
  # values of the last call are kept, so that the fallback's objective and
  # gradient at one point share them.
  at_shift <- NULL
  at_values <- NULL
  node_lp <- function(shift) {
    if (!stein || !identical(shift, at_shift)) {
      mu <- mode + shift
      v <- numeric(nn - 1)
      for (b in 2:nn) {
        v[b - 1] <- lp(mu + Z[b, ])
      }
      at_shift <<- shift
      at_values <<- v
    }
    at_values
  }

  # The fixed-point step Sigma E[grad log pi(mode + shift + z)] at a shift.
  # With a gradient, the expected gradient is the weighted mean over the
  # nodes, written as differences from the centre gradient: at most
  # (m + 1)(m + 2) + 1 gradient calls per sweep, and exact when the
  # log-posterior is a polynomial of degree six or less in whitened
  # coordinates. Without one it comes from Stein's identity,
  # E[z f(mu + z)] = Sigma E[grad f(mu + z)] for z ~ N(0, Sigma), on the same
  # nodes: the step is the weighted sum of the nodes times the log-posterior
  # there, taken over antipodal pairs as w_b z_b (f(mu + z_b) - f(mu - z_b)),
  # which cancels the size of the log-posterior. That takes at most
  # (m + 1)(m + 2) log-posterior calls per sweep, against m + 1 times as many
  # for the gradient form on finite differences, and z f(mu + z) has degree
  # five when the log-posterior has degree four, so the shift is then
  # exact.
  if (stein) {
    n_pairs <- (nn - 1) %/% 2
    plus <- seq_len(n_pairs)
    Zp <- Z[1 + plus, , drop = FALSE]
    wp <- w[1 + plus]
    raw_step <- function(shift) {
      v <- node_lp(shift)
      as.numeric(crossprod(Zp, wp * (v[plus] - v[n_pairs + plus])))
    }
  } else {
    score <- function(shift) {
      mu <- mode + shift
      g0 <- gr(mu)
      s <- g0
      for (b in 2:nn) {
        s <- s + w[b] * (gr(mu + Z[b, ]) - g0)
      }
      s
    }
    raw_step <- function(shift) as.numeric(Sigma %*% score(shift))
  }
  # The step with the shift of the fixed parameters held at zero
  step_at <- function(shift) {
    step <- raw_step(shift)
    step[fixed] <- 0
    step
  }

  # Negative expected log-posterior, for the fallback optimiser. A failed node
  # fails the objective, since the negative centre weight would otherwise
  # reward a -Inf there.
  failed <- 1e40
  objective <- function(shift) {
    lps <- c(lp(mode + shift), node_lp(shift))
    if (!all(is.finite(lps))) {
      return(failed)
    }
    -(lps[1] + sum(w[-1] * (lps[-1] - lps[1])))
  }

  # The fallback can end with no mean at which logpost is finite at every
  # node, or at the edge of the region where it is, with the optimum beyond.
  # The rule does not define the correction there, and a mean on that edge
  # would only show where the nodes are, so the fit stops. The message names
  # a node around `mode + shift` where logpost is -Inf.
  stop_failed_nodes <- function(shift, pinned) {
    mu <- mode + shift
    lps <- vapply(seq_len(nn), function(b) lp(mu + Z[b, ]), 0)
    x <- mu + Z[which.max(!is.finite(lps)), ]
    at <- paste(names(mode), "=", signif(x, 4), collapse = ", ")
    reach <- with_sd(max(sweep(abs(Z), 2, sds, "/")))
    abort(
      c(
        "The VB mean correction cannot be computed.",
        "x" = if (pinned) {
          "It stopped at the edge of the region where {.arg logpost} is
           finite at all of its nodes, and its next step puts a node at
           {at}, where {.arg logpost} is {.code -Inf}."
        } else {
          "{.arg logpost} is {.code -Inf} at some of its nodes around every
           mean tried, for example at {at}."
        },
        "i" = "The nodes reach {reach} from the mean, and the correction
               needs {.arg logpost} finite at all of them.",
        "i" = "Reparameterise any bounded parameter so that every real value
               is allowed (for example its log or logit, with the
               log-Jacobian), or set {.code vb_correction = \"none\"}."
      ),
      call = quote(inla_vanilla())
    )
  }

  # Fixed-point iteration shift <- shift + step_at(shift): with the curvature
  # of the Laplace approximation, the Newton step for the stationarity
  # condition is one multiplication by Sigma. Convergence is judged on the
  # step in posterior SDs. On a skewed posterior the first step can overshoot
  # the root, by over a posterior SD, and the iteration then comes back, so a
  # step over `maxstep` SDs is cut to `maxstep` SDs. Two such steps in a row,
  # or a non-finite step, mean the quadratic premise has failed, and the solve
  # falls back to nlminb(). INLAvaan falls back at the first step over 1 SD,
  # which on the ECPE DINA model (m = 63) cost 54,000 calls to reach the
  # point that six fixed-point steps reach.
  # The history (the step at each iteration, in posterior SDs, whether it was
  # cut and whether the Anderson secant replaced it) and the reason for a
  # fallback are kept for verbose output.
  maxstep <- vb_maxstep
  maxit <- ctrl$vb_maxit
  shift <- numeric(m)
  iter <- 0L
  move <- Inf
  fallback <- FALSE
  rerun <- FALSE
  reason <- NULL
  if (onestep) {
    checked <- vb_checked_onestep(
      lp,
      gr,
      mode,
      Sigma,
      R,
      A,
      ctrl,
      budget,
      fixed
    )
    if (!is.null(checked)) {
      return(checked)
    }
    fallback <- TRUE
    reason <- if (stein) {
      "the log-posterior was not finite at a node"
    } else {
      "the expected gradient was not finite"
    }
    maxit <- 0L
  }
  steps <- numeric()
  cut <- logical()
  accelerated <- logical()
  step_prev <- shift_prev <- NULL
  for (it in seq_len(maxit)) {
    step <- step_at(shift)
    if (!all(is.finite(step))) {
      fallback <- TRUE
      reason <- if (stein) {
        "the log-posterior was not finite at a node"
      } else {
        "the expected gradient was not finite"
      }
      break
    }
    move <- max(abs(step) / sds)
    steps[it] <- move
    raw <- step
    cut[it] <- move > maxstep
    if (cut[it]) {
      if (it > 1L && cut[it - 1L]) {
        fallback <- TRUE
        reason <- paste0("two steps in a row were over ", with_sd(maxstep))
        break
      }
      step <- step * maxstep / move
    }
    if (move < ctrl$vb_tol) {
      shift <- shift + step
      iter <- it
      break
    }
    # Anderson(1) acceleration: a secant estimate from consecutive steps
    # removes the dominant error mode. The plain step is kept whenever the
    # secant is degenerate or would move too far, and after a cut step. The
    # secant uses the steps before any cut, the fixed-point map's values.
    update <- step
    accelerated[it] <- FALSE
    if (!is.null(step_prev) && !cut[it]) {
      df <- step - step_prev
      dx <- shift - shift_prev
      denom <- sum(df^2)
      if (denom > 0) {
        gam <- sum(step * df) / denom
        cand <- step - gam * (dx + df)
        cand[fixed] <- 0
        if (
          all(is.finite(cand)) &&
            max(abs(cand) / sds) <= min(maxstep, 2 * move)
        ) {
          update <- cand
          accelerated[it] <- TRUE
        }
      }
    }
    step_prev <- raw
    shift_prev <- shift
    shift <- shift + update
    iter <- it
  }
  if (!onestep && move >= ctrl$vb_tol) {
    fallback <- TRUE
    if (is.null(reason)) {
      reason <- "the iteration did not converge in `vb_maxit` steps"
    }
  }

  if (fallback) {
    # Optimise in whitened coordinates d, with shift = A d for A = cov_root(),
    # where the problem is well conditioned. The gradient there is
    # -A' E[grad], which without a gradient is -A' Sigma^-1 step = -A^-1 step,
    # with A^-1 = A' H = A' R'R. That is the expected gradient by Stein's
    # identity, which agrees with the gradient of the objective to the error of
    # the rule, and costs no calls beyond the objective's at the same point.
    #
    # The gradient is not finite where logpost is -Inf at a node, as the
    # objective is failed there, or where grad is not finite at one.
    # nlminb() asks for it only at its start and at the points it accepts,
    # which have a finite objective, but the start, the mode, has failed
    # nodes when logpost is -Inf within their reach. There the gradient is
    # the mean of the failed nodes in whitened coordinates, so that nlminb()
    # moves the mean away from them, back towards the region where every node
    # is finite. Finding the failed nodes takes no calls without a gradient,
    # and one more sweep with one.
    A_inv <- crossprod(A, crossprod(R))
    U <- Z %*% t(A_inv) # whitened nodes
    away <- function(shift) {
      bad <- if (stein) {
        c(FALSE, !is.finite(node_lp(shift)))
      } else {
        mu <- mode + shift
        vapply(seq_len(nn), function(b) !all(is.finite(gr(mu + Z[b, ]))), NA)
      }
      if (any(bad)) colMeans(U[bad, , drop = FALSE]) else numeric(m)
    }
    fall_back <- function(start) {
      stats::nlminb(
        start = start,
        objective = function(d) objective(as.numeric(A %*% d)),
        gradient = function(d) {
          shift <- as.numeric(A %*% d)
          g <- if (stein) {
            -as.numeric(A_inv %*% raw_step(shift))
          } else {
            as.numeric(crossprod(A, -score(shift)))
          }
          if (all(is.finite(g))) g else away(shift)
        },
        control = list(rel.tol = 1e-8)
      )
    }
    nl <- fall_back(numeric(m))
    shift <- as.numeric(A %*% nl$par)
    shift[fixed] <- 0
    iter <- nl$iterations
    if (nl$objective >= failed) {
      stop_failed_nodes(shift, pinned = FALSE)
    }
    # Where the fallback landed: the fixed-point step there, the Newton step
    # for the stationarity condition. One more sweep.
    step <- step_at(shift)
    # With a gradient the objective and its gradient agree, so a landing that
    # did not settle means nlminb() stopped short. It can report relative
    # convergence well short of the optimum after starting where a node
    # fails (the objective's 1e40 there). Rerun it once from where it landed,
    # as the mode search reruns nlminb() (R/laplace.R). Without a gradient a
    # rerun cannot help: its gradient, Stein's estimate, disagrees with the
    # objective by the rule's error (lab/DESIGN.md). Only an unsettled
    # landing with a gradient pays for it.
    rerun <- !stein &&
      all(is.finite(step)) &&
      max(abs(step) / sds) >= warn_levels$vb_step
    if (rerun) {
      nl <- fall_back(nl$par)
      shift <- as.numeric(A %*% nl$par)
      shift[fixed] <- 0
      iter <- iter + nl$iterations
      step <- step_at(shift)
    }
    # A landing that did not settle may be pinned to the edge of the region
    # where every node is finite, with the optimum beyond it. nlminb() then
    # stops as close to the edge as its tolerance allows, so a thousandth of
    # the step still to go leads out of the region. A landing short of an
    # interior optimum is not that close to the edge. One more sweep, only
    # when the landing did not settle.
    if (
      all(is.finite(step)) &&
        max(abs(step) / sds) >= warn_levels$vb_step &&
        objective(shift + step / 1000) >= failed
    ) {
      stop_failed_nodes(shift + step / 1000, pinned = TRUE)
    }
  } else if (max(abs(step) / sds) >= warn_levels$vb_step) {
    # The iteration met a vb_tol above the settling level, so the step just
    # taken says little about the one still to go: take it. One more sweep,
    # which the default vb_tol never needs.
    step <- step_at(shift)
  }
  # The step still to go at the VB mean, per parameter in posterior SDs, as
  # the Newton step at the mode is for the mode: the step where the fallback
  # landed, or on the fixed-point path the last step taken, which was below
  # vb_tol and bounds the next (taken afresh when it was above the settling
  # level). A step that is not finite is Inf.
  step_sigma <- abs(step) / sds
  step_sigma[!is.finite(step_sigma)] <- Inf

  names(shift) <- names(mode)
  list(
    shift = shift,
    iterations = iter,
    fallback = fallback,
    one_pass = FALSE,
    # The check of the one step, which did not run here
    solve = NA,
    check = c(lhat = NA_real_, j0 = NA_real_, j1 = NA_real_),
    check_worst = NA_character_,
    step_sigma = stats::setNames(step_sigma, names(mode)),
    history = list(
      steps = steps,
      cut = cut,
      accelerated = accelerated,
      reason = reason,
      nlminb = if (fallback) nl$message,
      rerun = fallback && rerun
    ),
    # Joint KL divergence between the Laplace Gaussian and its shifted copy,
    # 0.5 shift' H shift. It is the location term the Laplace estimate of the
    # marginal likelihood misses, and is added to it.
    kld_global = 0.5 * sum((R %*% shift)^2)
  )
}
