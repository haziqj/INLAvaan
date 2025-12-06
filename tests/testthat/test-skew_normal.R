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
  expect_equal(res$alpha, 5)
})
