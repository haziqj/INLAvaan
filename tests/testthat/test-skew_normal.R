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

test_that("Gaussian marginal moments are taken on the original scale", {
  m <- 0.5
  s <- 0.3
  res <- post_marg_marggaus(
    j = 1,
    g = log,
    g_prime = function(x) 1 / x,
    ginv = exp,
    ginv_prime = exp,
    theta_star = m,
    Sigma_theta = matrix(s^2)
  )$summary
  expect_equal(unname(res["Mean"]), exp(m + s^2 / 2))
  expect_equal(unname(res["SD"]), sqrt((exp(s^2) - 1) * exp(2 * m + s^2)))
  expect_equal(unname(res["50%"]), exp(m))
})
