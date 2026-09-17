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

test_that("psnorm() reduces to pnorm() when there is no skew", {
  q <- seq(-5, 5, by = 0.25)
  expect_equal(psnorm(q), stats::pnorm(q))
  expect_equal(
    psnorm(q, lower_tail = FALSE),
    stats::pnorm(q, lower.tail = FALSE)
  )
  expect_equal(
    psnorm(q, xi = 1.5, omega = 0.4),
    stats::pnorm(q, mean = 1.5, sd = 0.4)
  )
  expect_equal(
    psnorm(q, xi = 1.5, omega = 0.4, lower_tail = FALSE),
    stats::pnorm(q, mean = 1.5, sd = 0.4, lower.tail = FALSE)
  )
})

test_that("psnorm() upper tail is the reflected lower tail", {
  q <- seq(-5, 5, by = 0.25)
  expect_equal(psnorm(q, 0, 1, 3), 1 - psnorm(-q, 0, 1, -3))
  expect_equal(psnorm(q, 0, 1, 3, lower_tail = FALSE), psnorm(-q, 0, 1, -3))
  # A far tail must not be computed as the difference of two numbers close
  # to one, so the symmetric pair adds up to the Gaussian answer exactly.
  expect_identical(
    psnorm(-4) + psnorm(4, lower_tail = FALSE),
    2 * stats::pnorm(-4)
  )
})

test_that("owen_t() matches its closed form at h = 0", {
  a <- c(-5, -1, -0.3, 0, 0.3, 1, 5)
  expect_equal(owen_t(0, a), atan(a) / (2 * pi))
})

test_that("psnorm() matches sn::psn() to machine precision", {
  skip_if_not_installed("sn")
  q <- seq(-6, 6, by = 0.25)
  for (alpha in c(-6, -1, 0, 1, 6)) {
    ref <- sn::psn(q, xi = 0, omega = 1, alpha = alpha)
    expect_equal(psnorm(q, 0, 1, alpha), ref, tolerance = 1e-12)
    expect_equal(
      psnorm(q, 0, 1, alpha, lower_tail = FALSE),
      1 - ref,
      tolerance = 1e-12
    )
  }
})

test_that("psnorm() returns NA for a degenerate scale", {
  expect_true(is.na(psnorm(1, 0, 0, 1)))
  expect_true(is.na(psnorm(1, 0, NA, 1)))
  expect_true(is.na(psnorm(1, 0, -1, 1)))
  expect_true(is.na(psnorm(NA, 0, 1, 1)))
})
