## ---- Unit tests: fast_grad --------------------------------------------------

test_that("fast_grad matches numDeriv::grad on a quadratic", {
  set.seed(42)
  m <- 6
  Q <- crossprod(matrix(rnorm(m^2), m, m)) + diag(m)
  fn <- function(x) 0.5 * as.numeric(t(x) %*% Q %*% x)

  x0 <- rnorm(m)
  g_fast <- fast_grad(fn, x0)
  g_numD <- numDeriv::grad(fn, x0)
  g_true <- as.numeric(Q %*% x0)

  expect_equal(g_fast, g_numD, tolerance = 1e-6)
  expect_equal(g_fast, g_true, tolerance = 1e-4)
})

test_that("fast_grad works for scalar case", {
  fn <- function(x) 2 * x^3
  g <- fast_grad(fn, 2.0)
  expect_equal(as.numeric(g), 24, tolerance = 1e-4) # true: 6x^2 = 24
})

## ---- Unit tests: fast_jacobian vs numDeriv::jacobian ------------------------

# Test 1: Known quadratic function (exact Hessian is constant)
test_that("fast_jacobian matches numDeriv on a quadratic", {
  # f(x) = 0.5 * x^T A x, so grad(f) = A x and Hessian = A
  set.seed(42)
  m <- 10
  Q <- crossprod(matrix(rnorm(m^2), m, m)) + diag(m) # pos-def
  gr_fn <- function(x) as.numeric(Q %*% x)

  x0 <- rnorm(m)
  H_fast <- fast_jacobian(gr_fn, x0)
  H_numD <- numDeriv::jacobian(gr_fn, x0)

  expect_equal(H_fast, H_numD, tolerance = 1e-4)
  # Both should recover Q exactly (up to FD error)
  expect_equal(H_fast, Q, tolerance = 1e-4)
})

# Test 2: Rosenbrock-like function with non-trivial curvature
test_that("fast_jacobian matches numDeriv on a non-linear function", {
  rosenbrock_grad <- function(x) {
    n <- length(x)
    g <- numeric(n)
    for (i in 1:(n - 1)) {
      g[i] <- g[i] - 2 * (1 - x[i]) - 400 * x[i] * (x[i + 1] - x[i]^2)
      g[i + 1] <- g[i + 1] + 200 * (x[i + 1] - x[i]^2)
    }
    g
  }

  x0 <- rep(1.1, 5) # near the minimum at (1,1,...,1)
  H_fast <- fast_jacobian(rosenbrock_grad, x0)
  H_numD <- numDeriv::jacobian(rosenbrock_grad, x0)

  expect_equal(H_fast, H_numD, tolerance = 1e-3)
})

# Test 3: Scalar gradient (m = 1)
test_that("fast_jacobian works for scalar case", {
  gr_fn <- function(x) 6 * x^2 # gradient of 2x^3
  H_fast <- fast_jacobian(gr_fn, 2.0)
  H_numD <- numDeriv::jacobian(gr_fn, 2.0)

  expect_equal(H_fast, H_numD, tolerance = 1e-4)
  expect_equal(as.numeric(H_fast), 24, tolerance = 1e-4) # true Hessian = 12x = 24
})

# Test 4: Symmetry of the resulting Hessian
test_that("fast_jacobian produces a (near-)symmetric matrix", {
  set.seed(123)
  m <- 8
  Q <- crossprod(matrix(rnorm(m^2), m, m)) + diag(m)
  gr_fn <- function(x) as.numeric(Q %*% x)

  x0 <- rnorm(m)
  H_fast <- fast_jacobian(gr_fn, x0)

  expect_equal(H_fast, t(H_fast), tolerance = 1e-6)
})

## ---- Integration test: Hessian on an actual SEM model -----------------------

dat <- lavaan::PoliticalDemocracy
mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  dem60 ~ ind60
  dem65 ~ ind60 + dem60
"

test_that("fast_jacobian Hessian matches numDeriv on the PoliticalDemocracy SEM", {
  # Fit with debug = TRUE to get internal objects, but we need the gradient

  # closure. Instead, build a lightweight version using INLAvaan internals.
  fit <- asem(mod, dat, test = "none", verbose = FALSE, nsamp = 3)

  # Extract the Sigma_theta (covariance) from the fit
  int <- INLAvaan:::get_inlavaan_internal(fit)
  Sigma_fast <- int$Sigma_theta

  # Now refit with numDeriv to get the "reference" Hessian.
  # We do this by accessing the internal gradient function via the same
  # model setup and using numDeriv::jacobian directly.
  #
  # Reconstruct the joint_lp_grad closure from the fitted objects
  pt <- int$partable
  lavmodel <- int$lavmodel
  lavsamplestats <- int$lavsamplestats
  lavdata <- int$lavdata
  lavcache <- fit@Cache
  lavoptions <- fit@Options
  theta_star <- int$theta_star_novbc # mode before VB correction
  ceq.simple <- lavmodel@ceq.simple.only
  ceq.K <- lavmodel@ceq.simple.K
  prior_cache <- INLAvaan:::prepare_priors_for_optim(pt)

  neg_grad <- function(pars) {
    if (isTRUE(ceq.simple)) {
      pars_unpacked <- as.numeric(ceq.K %*% pars)
      x <- INLAvaan:::pars_to_x(pars_unpacked, pt)
      jcb <- mapply(
        function(f, x) f(x),
        pt$ginv_prime[pt$free > 0],
        pars_unpacked
      )
    } else {
      x <- INLAvaan:::pars_to_x(pars, pt)
      jcb <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
    }
    gll <- INLAvaan:::inlav_model_grad(
      x,
      lavmodel,
      lavsamplestats,
      lavdata,
      lavcache
    )
    jcb <- diag(jcb, length(jcb))
    sd1sd2 <- attr(x, "sd1sd2")
    jcb <- jcb * sd1sd2
    jcb_mat <- attr(x, "jcb_mat")
    if (!is.null(jcb_mat)) {
      for (k in seq_len(nrow(jcb_mat))) {
        i <- jcb_mat[k, 1]
        j <- jcb_mat[k, 2]
        jcb[i, j] <- jcb_mat[k, 3]
      }
    }
    gll_th <- as.numeric(jcb %*% gll)
    if (isTRUE(ceq.simple)) {
      gll_th <- as.numeric(gll_th %*% ceq.K)
    }
    glp_th <- INLAvaan:::prior_grad_vectorized(pars, prior_cache)
    -1 * as.numeric(gll_th + glp_th)
  }

  H_fast <- INLAvaan:::fast_jacobian(neg_grad, theta_star)
  H_numD <- numDeriv::jacobian(neg_grad, theta_star)

  # The two Hessians should be very close
  expect_equal(H_fast, H_numD, tolerance = 1e-3)

  # And the resulting covariance matrices should be close
  H_sym_fast <- 0.5 * (H_fast + t(H_fast))
  H_sym_numD <- 0.5 * (H_numD + t(H_numD))
  Sigma_numD <- solve(H_sym_numD)
  Sigma_fast2 <- solve(H_sym_fast)
  expect_equal(Sigma_fast2, Sigma_numD, tolerance = 1e-3)

  # The posterior SDs should agree to ~0.1%
  se_fast <- sqrt(diag(Sigma_fast2))
  se_numD <- sqrt(diag(Sigma_numD))
  expect_equal(se_fast, se_numD, tolerance = 1e-3)
})

## ---- Unit tests: fast_hessian -----------------------------------------------

test_that("fast_hessian matches numDeriv::hessian on a quadratic", {
  set.seed(42)
  m <- 6
  Q <- crossprod(matrix(rnorm(m^2), m, m)) + diag(m)
  fn <- function(x) 0.5 * as.numeric(t(x) %*% Q %*% x)

  x0 <- rnorm(m)
  H_fast <- fast_hessian(fn, x0)
  H_numD <- numDeriv::hessian(fn, x0)

  expect_equal(H_fast, H_numD, tolerance = 1e-3)
  # Both should recover the symmetric part of Q
  expect_equal(H_fast, 0.5 * (Q + t(Q)), tolerance = 1e-3)
})

test_that("fast_hessian matches numDeriv on Rosenbrock", {
  rosenbrock <- function(x) {
    sum(100 * (x[-1] - x[-length(x)]^2)^2 + (1 - x[-length(x)])^2)
  }
  x0 <- rep(1.1, 4)
  H_fast <- fast_hessian(rosenbrock, x0)
  H_numD <- numDeriv::hessian(rosenbrock, x0)

  expect_equal(H_fast, H_numD, tolerance = 1e-2)
})

test_that("fast_hessian is symmetric", {
  fn <- function(x) sin(x[1]) * cos(x[2]) + x[3]^3
  H <- fast_hessian(fn, c(1, 2, 3))
  expect_equal(H, t(H), tolerance = 1e-10)
})

test_that("fast_hessian works for scalar case", {
  fn <- function(x) 2 * x^3
  H <- fast_hessian(fn, 2.0)
  expect_equal(as.numeric(H), 24, tolerance = 1e-3) # true: 12x = 24
})