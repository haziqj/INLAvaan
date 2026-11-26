# The two-level kernels of R/twolevel-kernels.R: the log-likelihood and its
# gradient against lavaan's two-level code and a numerical gradient.

tlk_data <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:40, ]

tlk_models <- list(
  # One factor at each level
  cfa = "
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
  ",
  # Covariates at both levels, one of them between-only
  covariates = "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1 + x2
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
  ",
  # A between-only outcome, and loadings held equal across the levels
  between_outcome = "
    level: 1
      fw =~ y1 + a*y2 + b*y3
    level: 2
      fb =~ y1 + a*y2 + b*y3
      w2 ~ fb + w1
  "
)

tlk_fits <- lapply(tlk_models, function(m) {
  suppressWarnings(lavaan::sem(
    m,
    data = tlk_data,
    cluster = "cluster",
    ceq.simple = TRUE,
    se = "none"
  ))
})

tlk_lavaan <- list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)

tlk_loglik <- function(fit, x, multilevel = NULL) {
  inlav_model_loglik(
    x,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options,
    fit@Cache,
    multilevel = multilevel
  )
}

tlk_grad <- function(fit, x, multilevel = NULL) {
  inlav_model_grad(
    x,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Cache,
    multilevel = multilevel
  )
}

# A point away from the estimates, in lavaan's packed parameters, with every
# variance at least 0.1
tlk_point <- function(fit, seed) {
  set.seed(seed)
  x <- lavaan::lav_model_get_parameters(fit@Model)
  x <- x + 0.05 * stats::rnorm(length(x))
  pt <- lavaan::parTable(fit)
  var_idx <- unique(pt$free[pt$op == "~~" & pt$lhs == pt$rhs & pt$free > 0])
  x[var_idx] <- pmax(abs(x[var_idx]), 0.1)
  x
}

tlk_k <- function(fit) {
  K <- fit@Model@ceq.simple.K
  if (length(K) == 0L) diag(fit@Model@nx.free) else K
}

test_that("the kernels take two-level models with complete data only", {
  for (fit in tlk_fits) {
    expect_true(twolevel_kernel_ok(fit@Model, fit@SampleStats, fit@Data))
  }
  dat_na <- tlk_data
  dat_na$y1[c(3, 40, 77)] <- NA
  fit_na <- suppressWarnings(lavaan::sem(
    tlk_models$cfa,
    data = dat_na,
    cluster = "cluster",
    missing = "ml",
    se = "none"
  ))
  expect_false(twolevel_kernel_ok(
    fit_na@Model,
    fit_na@SampleStats,
    fit_na@Data
  ))
  single <- lavaan::cfa("f =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
  expect_false(twolevel_kernel_ok(
    single@Model,
    single@SampleStats,
    single@Data
  ))
})

test_that("the log-likelihood equals lavaan's", {
  for (fit in tlk_fits) {
    x <- lavaan::lav_model_get_parameters(fit@Model)
    expect_equal(tlk_loglik(fit, x), as.numeric(lavaan::logLik(fit)))
    for (seed in 1:3) {
      x <- tlk_point(fit, seed)
      expect_equal(
        tlk_loglik(fit, x),
        tlk_loglik(fit, x, multilevel = tlk_lavaan),
        tolerance = 1e-12
      )
    }
  }
})

test_that("the gradient equals lavaan's and a numerical gradient", {
  skip_if_not_installed("numDeriv")
  for (fit in tlk_fits) {
    for (seed in 1:2) {
      x <- tlk_point(fit, seed)
      g <- tlk_grad(fit, x)
      expect_length(g, fit@Model@nx.unco)
      expect_equal(
        g,
        tlk_grad(fit, x, multilevel = tlk_lavaan),
        tolerance = 1e-10
      )
      g_num <- numDeriv::grad(function(z) tlk_loglik(fit, z), x)
      expect_equal(as.numeric(g %*% tlk_k(fit)), g_num, tolerance = 1e-6)
    }
  }
})

test_that("a matrix with no Cholesky factor falls back to lavaan", {
  fit <- tlk_fits$cfa
  implied <- lavaan::lav_model_implied(fit@Model)
  implied$cov[[1L]][] <- -1
  expect_null(twolevel_kernel_loglik(fit@SampleStats, fit@Data, implied))
  expect_null(twolevel_dmoments(fit@SampleStats, fit@Data, implied))
})

## ----- Missing data (R/twolevel-fiml-kernels.R) ------------------------------

tlk_data_na <- local({
  dat <- tlk_data
  set.seed(27)
  for (v in c("y1", "y2", "y3", "x1")) {
    dat[[v]][stats::runif(nrow(dat)) < 0.12] <- NA
  }
  # One case that misses every level-1 variable of the model
  dat[5L, c("y1", "y2", "y3")] <- NA
  dat
})

tlk_fits_na <- lapply(
  list(
    cfa = tlk_models$cfa,
    # A within-only variable and loadings held equal across the levels
    within_only = "
      level: 1
        fw =~ y1 + a*y2 + b*y3 + x1
      level: 2
        fb =~ y1 + a*y2 + b*y3
    "
  ),
  function(m) {
    suppressWarnings(lavaan::sem(
      m,
      data = tlk_data_na,
      cluster = "cluster",
      missing = "ml",
      ceq.simple = TRUE,
      se = "none"
    ))
  }
)

test_that("the FIML kernels take two-level models with missing data", {
  for (fit in tlk_fits_na) {
    expect_true(twolevel_fiml_ok(fit@Model, fit@SampleStats, fit@Data))
    expect_false(twolevel_kernel_ok(fit@Model, fit@SampleStats, fit@Data))
  }
  between <- suppressWarnings(lavaan::sem(
    tlk_models$between_outcome,
    data = tlk_data_na,
    cluster = "cluster",
    missing = "ml",
    se = "none"
  ))
  expect_false(twolevel_fiml_ok(
    between@Model,
    between@SampleStats,
    between@Data
  ))
})

test_that("the FIML log-likelihood equals lavaan's", {
  for (fit in tlk_fits_na) {
    x <- lavaan::lav_model_get_parameters(fit@Model)
    expect_equal(tlk_loglik(fit, x), as.numeric(lavaan::logLik(fit)))
    for (seed in 1:3) {
      x <- tlk_point(fit, seed)
      expect_equal(
        tlk_loglik(fit, x),
        tlk_loglik(fit, x, multilevel = tlk_lavaan),
        tolerance = 1e-12
      )
    }
  }
})

test_that("the FIML gradient equals lavaan's and a numerical gradient", {
  skip_if_not_installed("numDeriv")
  for (fit in tlk_fits_na) {
    for (seed in 1:2) {
      x <- tlk_point(fit, seed)
      g <- tlk_grad(fit, x)
      expect_length(g, fit@Model@nx.unco)
      expect_equal(
        g,
        tlk_grad(fit, x, multilevel = tlk_lavaan),
        tolerance = 1e-10
      )
      g_num <- numDeriv::grad(function(z) tlk_loglik(fit, z), x)
      expect_equal(as.numeric(g %*% tlk_k(fit)), g_num, tolerance = 1e-6)
    }
  }
})

test_that("the FIML kernels go back to lavaan or give -Inf as lavaan does", {
  fit <- tlk_fits_na$cfa
  plan <- twolevel_fiml_plan(fit@Data)
  implied <- lavaan::lav_model_implied(fit@Model)
  bad_b <- implied
  diag(bad_b$cov[[2L]]) <- -1
  expect_identical(twolevel_fiml_loglik(plan, fit@SampleStats, bad_b), -Inf)
  expect_null(twolevel_fiml_dmoments(plan, bad_b))
  bad_w <- implied
  bad_w$cov[[1L]][] <- 1
  expect_null(twolevel_fiml_loglik(plan, fit@SampleStats, bad_w))
})
