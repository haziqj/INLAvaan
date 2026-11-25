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

# Test 5: Forward differences
test_that("fast_jacobian forward differences recover the Hessian", {
  set.seed(42)
  m <- 10
  Q <- crossprod(matrix(rnorm(m^2), m, m)) + diag(m)
  gr_fn <- function(x) as.numeric(Q %*% x)
  x0 <- rnorm(m)
  expect_equal(
    fast_jacobian(gr_fn, x0, method = "forward"),
    Q,
    tolerance = 1e-6
  )

  rosenbrock_grad <- function(x) {
    n <- length(x)
    g <- numeric(n)
    for (i in 1:(n - 1)) {
      g[i] <- g[i] - 2 * (1 - x[i]) - 400 * x[i] * (x[i + 1] - x[i]^2)
      g[i + 1] <- g[i + 1] + 200 * (x[i + 1] - x[i]^2)
    }
    g
  }
  x1 <- rep(1.1, 5)
  expect_equal(
    fast_jacobian(rosenbrock_grad, x1, method = "forward"),
    numDeriv::jacobian(rosenbrock_grad, x1),
    tolerance = 1e-4
  )
})

test_that("fast_jacobian forward differences need m + 1 gradient calls", {
  calls <- 0L
  gr_fn <- function(x) {
    calls <<- calls + 1L
    2 * x
  }
  fast_jacobian(gr_fn, c(1, 2, 3, 4), method = "forward")
  expect_identical(calls, 5L)
  calls <- 0L
  fast_jacobian(gr_fn, c(1, 2, 3, 4))
  expect_identical(calls, 8L)
  # Columns outside `cols` stay zero
  H <- fast_jacobian(gr_fn, c(1, 2, 3, 4), cols = 2:3, method = "forward")
  expect_equal(H[, c(1, 4)], matrix(0, 4, 2))
  expect_equal(diag(H)[2:3], c(2, 2), tolerance = 1e-6)
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
  skip_on_cran()
  # Fit with debug = TRUE to get internal objects, but we need the gradient

  # closure. Instead, build a lightweight version using INLAvaan internals.
  fit <- asem(mod, dat, test = "none", silent = TRUE, nsamp = 3)

  # Extract the posterior covariance Omega from the fit
  int <- INLAvaan:::get_inlavaan_internal(fit)
  Sigma_fast <- int$Omega

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

test_that("hessian_method = 'forward' gives the central posterior", {
  skip_on_cran()
  fit_c <- asem(mod, dat, test = "none", silent = TRUE, nsamp = 3)
  fit_f <- asem(
    mod,
    dat,
    test = "none",
    silent = TRUE,
    nsamp = 3,
    hessian_method = "forward"
  )
  int_c <- get_inlavaan_internal(fit_c)
  int_f <- get_inlavaan_internal(fit_f)
  expect_equal(
    sqrt(diag(int_f$Omega)),
    sqrt(diag(int_c$Omega)),
    tolerance = 1e-5
  )
  expect_equal(int_f$theta_star, int_c$theta_star, tolerance = 1e-5)
  expect_error(
    asem(mod, dat, test = "none", silent = TRUE, hessian_method = "backward")
  )
})
