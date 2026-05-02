test_that("SN fit fixed temp", {
  x_grid <- seq(-2, 5, length.out = 21)
  y_log <- sapply(x_grid, function(x) {
    dsnorm(x, xi = 1, omega = 1, alpha = 5, log = TRUE)
  })
  y_log <- y_log - max(y_log) # normalise to have maximum at zero

  res <- fit_skew_normal(x_grid, y_log, temp = 1)
  expect_equal(res$xi, 1)
  expect_equal(res$omega, 1)
  expect_equal(res$alpha, 5)
})

test_that("SN fit variable temp", {
  x_grid <- seq(-2, 5, length.out = 31)
  y_log <- sapply(x_grid, function(x) {
    dsnorm(x, xi = 1, omega = 1, alpha = 5, log = TRUE)
  })
  y_log <- y_log - max(y_log) # normalise to have maximum at zero

  res <- fit_skew_normal(x_grid, y_log, temp = NA)
  expect_equal(res$xi, 1)
  expect_equal(res$omega, 1)
  expect_equal(res$alpha, 5, tolerance = 1e-6)
})

test_that("SN sample fit uses native optimiser and recovers parameters", {
  set.seed(1)
  x <- qsnorm_fast(runif(4000), xi = 1, omega = 1.5, alpha = 4)

  res <- fit_skew_normal_samp(x)

  expect_equal(res$xi, 1, tolerance = 0.12)
  expect_equal(res$omega, 1.5, tolerance = 0.12)
  expect_equal(res$alpha, 4, tolerance = 0.45)
})
