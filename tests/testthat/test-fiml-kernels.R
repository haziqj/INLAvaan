# The single-level FIML kernels of R/fiml-kernels.R: the log-likelihood and
# its gradient against lavaan, on the shared route and on the direct route.

fiml_test_model <- "
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9
"

# Values missing at random places: many patterns, most with one case
fiml_test_scattered <- local({
  dat <- lavaan::HolzingerSwineford1939
  set.seed(26)
  for (v in paste0("x", 1:9)) {
    dat[[v]][runif(nrow(dat)) < 0.12] <- NA
  }
  dat
})

# Planned missingness: three forms that each miss six of the nine variables,
# and one complete form. Few patterns, each with many cases.
fiml_test_planned <- local({
  dat <- lavaan::HolzingerSwineford1939
  form <- rep_len(1:4, nrow(dat))
  dat[form == 1L, paste0("x", c(2, 3, 5, 6, 8, 9))] <- NA
  dat[form == 2L, paste0("x", c(1, 3, 4, 6, 7, 9))] <- NA
  dat[form == 3L, paste0("x", c(1, 2, 4, 5, 7, 8))] <- NA
  dat
})

fiml_test_fits <- list(
  scattered = lavaan::cfa(fiml_test_model, fiml_test_scattered, missing = "ml"),
  planned = lavaan::cfa(fiml_test_model, fiml_test_planned, missing = "ml"),
  multigroup = lavaan::cfa(
    fiml_test_model,
    fiml_test_scattered,
    missing = "ml",
    group = "school",
    group.equal = "loadings",
    ceq.simple = TRUE
  )
)

fiml_test_loglik <- function(fit, x, fiml = NULL) {
  inlav_model_loglik(
    x,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options,
    fit@Cache,
    fiml = fiml
  )
}

fiml_test_grad <- function(fit, x, fiml = NULL) {
  inlav_model_grad(
    x,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Cache,
    fiml = fiml
  )
}

# The plan of a fit with every pattern on one route
fiml_test_setup <- function(fit, shared) {
  local_mocked_bindings(
    fiml_route_shared = function(k, p, many) rep(shared, length(k))
  )
  fiml_setup(fit@Model, fit@SampleStats, fit@Data)
}

test_that("the plan sends patterns to the shared and to the direct route", {
  # The complete pattern and the six patterns that miss one variable are
  # shared. A lone pattern is direct.
  expect_identical(
    fiml_route_shared(c(0L, rep(1L, 6L), 2L, 6L), p = 9L),
    c(rep(TRUE, 7L), FALSE, FALSE)
  )
  # With few patterns the shared route does not pay for its set-up
  expect_identical(fiml_route_shared(c(0L, 1L, 1L), p = 9L), rep(FALSE, 3L))
  plan <- fiml_plan(fiml_test_fits$scattered@SampleStats)[[1L]]
  expect_gt(length(plan$sets), 1L)
  expect_lt(length(plan$direct), length(plan$pats))
  plan <- fiml_plan(fiml_test_fits$planned@SampleStats)[[1L]]
  expect_length(plan$direct, 4L)
  expect_length(plan$pats, 4L)
})

test_that("the fixed rows of a pattern reproduce its sum of squares", {
  for (pat in fiml_plan(fiml_test_fits$planned@SampleStats)[[1L]]$pats) {
    rows <- fiml_fixed_rows(pat, 9L)
    expect_lt(nrow(rows), pat$freq)
    expect_equal(
      crossprod(rows)[pat$o, pat$o],
      pat$freq * unname(pat$sy),
      tolerance = 1e-12
    )
  }
})

test_that("the FIML log-likelihood equals lavaan's logLik()", {
  for (fit in fiml_test_fits) {
    x <- lavaan::lav_model_get_parameters(fit@Model)
    ref <- as.numeric(lavaan::logLik(fit))
    expect_equal(fiml_test_loglik(fit, x), ref, tolerance = 1e-12)
    for (shared in c(TRUE, FALSE)) {
      fiml <- fiml_test_setup(fit, shared)
      expect_true(fiml$loglik)
      expect_equal(fiml_test_loglik(fit, x, fiml), ref, tolerance = 1e-12)
    }
  }
})

test_that("the FIML gradient equals lavaan's and a numerical gradient", {
  for (fit in fiml_test_fits) {
    x <- lavaan::lav_model_get_parameters(fit@Model)
    x <- x * (1 + 0.05 * sin(seq_along(x)))
    h <- 1e-5
    grad_fd <- vapply(
      seq_along(x),
      function(i) {
        e <- replace(numeric(length(x)), i, h)
        (fiml_test_loglik(fit, x + e) - fiml_test_loglik(fit, x - e)) / (2 * h)
      },
      numeric(1)
    )
    grad_lavaan <- -fit@SampleStats@ntotal *
      lavaan___lav_model_grad(
        lavmodel = lavaan::lav_model_set_parameters(fit@Model, x),
        lavsamplestats = fit@SampleStats,
        lavdata = fit@Data,
        lavcache = fit@Cache,
        group_weight = FALSE
      )
    # Under equality constraints `x` is packed, with one entry per set of
    # equal parameters, and the gradient has one entry per free row.
    grad <- fiml_test_grad(fit, x)
    grad_packed <- if (isTRUE(fit@Model@ceq.simple.only)) {
      as.numeric(grad %*% fit@Model@ceq.simple.K)
    } else {
      grad
    }
    expect_equal(grad_packed, grad_fd, tolerance = 1e-6)
    expect_equal(grad, grad_lavaan, tolerance = 1e-10)
    for (shared in c(TRUE, FALSE)) {
      fiml <- fiml_test_setup(fit, shared)
      expect_true(fiml$grad)
      expect_equal(
        fiml_test_grad(fit, x, fiml),
        grad_lavaan,
        tolerance = 1e-10
      )
    }
  }
})

test_that("a plan built before the call gives the same values", {
  fit <- fiml_test_fits$scattered
  x <- lavaan::lav_model_get_parameters(fit@Model) + 0.02
  fiml <- fiml_setup(fit@Model, fit@SampleStats, fit@Data)
  expect_identical(fiml_test_loglik(fit, x, fiml), fiml_test_loglik(fit, x))
  expect_identical(fiml_test_grad(fit, x, fiml), fiml_test_grad(fit, x))
})

test_that("a group without a Cholesky factor of Sigma is scored by block", {
  # Two variables that are never observed together. The log-likelihood needs
  # only the two variances, so it exists where Sigma is not positive definite.
  pats <- list(
    list(var.idx = c(TRUE, FALSE), freq = 3L, MY = 0.2, SY = matrix(1.5)),
    list(var.idx = c(FALSE, TRUE), freq = 1L, MY = -0.4, SY = NULL)
  )
  local_mocked_bindings(fiml_route_shared = function(k, p, many) {
    rep(TRUE, length(k))
  })
  plan <- list(fiml_plan_group(pats))
  expect_length(plan[[1L]]$direct, 0L)
  implied <- list(
    mean = list(c(0.1, 0.3)),
    cov = list(matrix(c(1, 2, 2, 1), 2))
  )
  ref <- -0.5 * 3 * (log(2 * pi) + (1.5 + 0.1^2)) - 0.5 * (log(2 * pi) + 0.7^2)
  expect_equal(fiml_loglik_samp(NULL, implied, plan), ref, tolerance = 1e-14)
  # A variance below zero has no factor on either route
  implied$cov[[1L]][1L, 1L] <- -1
  expect_identical(fiml_loglik_samp(NULL, implied, plan), -1e40)
})
