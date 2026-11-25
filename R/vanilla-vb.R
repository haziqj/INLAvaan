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
# posterior SDs from the posterior mean. With a gradient the step uses 2m + 1
# nodes on the axes of cov_root() (axis_nodes()), about one numerical Hessian.
# Without one it is one Stein pass on vb_nodes(), since Stein's identity needs
# that rule's degree. See lab/ideas/2026-11-one-step-vb.

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

# How much of the promised gain one step delivered. The step takes the expected
# log-posterior to be a parabola with the Laplace curvature, which promises a
# rise of 0.5 shift' H shift. The ratio is the rise that came about, over that
# promise, so it is near 1 when the premise holds, and small or negative when
# the step went too far or the posterior is far from Gaussian
# (lab/ideas/2026-11-one-step-vb). The expectations use axis_nodes(), with or
# without a gradient: 4m + 2 calls of logpost. A step too small to check is NA.
vb_gain_ratio <- function(lp, mode, Sigma, R, shift, A = cov_root(Sigma)) {
  promised <- 0.5 * sum((R %*% shift)^2)
  if (promised < 1e-6) {
    return(NA_real_)
  }
  rule <- axis_nodes(Sigma, A)
  expected <- function(mu) {
    lps <- vapply(
      seq_len(nrow(rule$nodes)),
      function(b) lp(mu + rule$nodes[b, ]),
      0
    )
    lps[1] + sum(rule$weights[-1] * (lps[-1] - lps[1]))
  }
  (expected(mode + shift) - expected(mode)) / promised
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

# The largest first step that vb_correction = "onestep" accepts, in posterior
# SDs. Beyond it the premise of a small correction has failed, and the iteration
# carries on with capped steps. The level is a heuristic: at 2 SDs the guard
# fired on large logistic regressions where the plain step was mostly better,
# and at 3 SDs it let through a strongly skewed posterior whose first step was
# 2.9 SDs (lab/ideas/2026-11-one-step-vb).
vb_onestep_guard <- 2.5

# `method` is "onestep" or "full". `budget` is the calls of the mode and
# Hessian stage: when one-step VB has to iterate, it stops within that many
# calls (at least two passes), and never calls nlminb() on account of its
# length, so the VB stage stays within what the fit has already spent.
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
  #
  # With vb_correction = "onestep" the first step is taken whole and the
  # iteration stops there, unless it is not finite or larger than the guard.
  # Then the iteration goes on as above, within the budget: on running out, the
  # last point stands, and the settling check reports how far it still had to
  # go.
  maxstep <- vb_maxstep
  maxit <- ctrl$vb_maxit
  if (onestep) {
    calls_per_pass <- if (stein) nn - 1L else nn
    # One pass of the budget is kept for the step still to go, if it runs out
    maxit <- min(maxit, max(2L, floor(budget / calls_per_pass) - 1L))
  }
  one_pass <- FALSE
  shift <- numeric(m)
  iter <- 0L
  move <- Inf
  fallback <- FALSE
  rerun <- FALSE
  reason <- NULL
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
    if (onestep && it == 1L && move <= vb_onestep_guard) {
      shift <- shift + step
      iter <- 1L
      cut[it] <- FALSE
      accelerated[it] <- FALSE
      one_pass <- TRUE
      break
    }
    raw <- step
    cut[it] <- move > maxstep
    if (cut[it]) {
      if (!onestep && it > 1L && cut[it - 1L]) {
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
  ran_out <- onestep && !one_pass && !fallback && move >= ctrl$vb_tol
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
  } else if (!one_pass && max(abs(step) / sds) >= warn_levels$vb_step) {
    # The iteration met a vb_tol above the settling level, or one-step VB ran
    # out of budget, so the step just taken says little about the one still
    # to go: take it. One more sweep, which the default vb_tol never needs.
    step <- step_at(shift)
  }
  # The step still to go at the VB mean, per parameter in posterior SDs, as
  # the Newton step at the mode is for the mode: the step where the fallback
  # landed, or on the fixed-point path the last step taken, which was below
  # vb_tol and bounds the next (taken afresh when it was above the settling
  # level). A step that is not finite is Inf. One step has no step still to
  # go, as it would take another pass to know, so the check did not run: NA.
  step_sigma <- abs(step) / sds
  step_sigma[!is.finite(step_sigma)] <- Inf
  if (one_pass) {
    step_sigma[] <- NA_real_
  }

  names(shift) <- names(mode)
  list(
    shift = shift,
    iterations = iter,
    fallback = fallback,
    one_pass = one_pass,
    gain_ratio = if (one_pass) {
      vb_gain_ratio(lp, mode, Sigma, R, shift, A)
    } else {
      NA_real_
    },
    step_sigma = stats::setNames(step_sigma, names(mode)),
    history = list(
      steps = steps,
      cut = cut,
      accelerated = accelerated,
      ran_out = ran_out,
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
