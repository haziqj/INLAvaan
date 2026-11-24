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

test_that("fit_skew_normal() fixes the temperature at 1 by default", {
  x_grid <- seq(-2, 5, length.out = 31)
  y_log <- dsnorm(x_grid, xi = 1, omega = 1, alpha = 5, log = TRUE)
  y_log <- y_log - max(y_log)

  res <- fit_skew_normal(x_grid, y_log)
  expect_named(res, c("xi", "omega", "alpha", "logC", "nmad"))
  expect_identical(res, fit_skew_normal(x_grid, y_log, temp = 1))
  expect_equal(c(res$xi, res$omega, res$alpha), c(1, 1, 5))
  expect_lt(res$nmad, 1e-8)
})

test_that("Skew-normal sample fit is scale-equivariant", {
  set.seed(1)
  d <- 4 / sqrt(17)
  y <- d * abs(rnorm(500)) + sqrt(1 - d^2) * rnorm(500)
  ref <- fit_skew_normal_samp(y)
  expect_gt(ref$alpha, 2)
  for (s in c(1e-2, 1e-4)) {
    res <- fit_skew_normal_samp(s * y)
    expect_equal(res$alpha, ref$alpha, tolerance = 1e-5)
    expect_equal(
      c(res$xi, res$omega) / s,
      c(ref$xi, ref$omega),
      tolerance = 1e-5
    )
  }
})

test_that("Posterior modes are accurate on a small scale", {
  set.seed(1)
  d <- 4 / sqrt(17)
  y <- d * abs(rnorm(500)) + sqrt(1 - d^2) * rnorm(500)
  ref <- summarise_samples_sn(y)$summary[["Mode"]]
  for (s in c(1e-2, 1e-4)) {
    res <- summarise_samples_sn(s * y)$summary[["Mode"]]
    expect_equal(res / s, ref, tolerance = 1e-5)
  }

  # A log-normal marginal with median 1e-3 has its mode at exp(m - v)
  m <- log(1e-3)
  v <- 0.3^2
  res <- post_marg_marggaus(
    j = 1,
    g = log,
    g_prime = function(x) 1 / x,
    ginv = exp,
    ginv_prime = exp,
    theta_star = m,
    Sigma_theta = matrix(v)
  )$summary
  expect_equal(unname(res["Mode"]), exp(m - v), tolerance = 1e-6)
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

test_that("psnorm() is 0 and 1 at the infinite ends", {
  expect_identical(psnorm(c(-Inf, Inf), 0, 1, 3), c(0, 1))
  expect_identical(psnorm(c(-Inf, Inf), 0, 1, 3, lower_tail = FALSE), c(1, 0))
})

test_that("qsnorm() inverts psnorm(), also far out in the tails", {
  p <- c(1e-10, 1e-4, 0.025, 0.5, 0.975, 1 - 1e-4)
  upper <- p > 0.5
  for (alpha in c(-20, -2, -0.5, 0, 0.5, 2, 20)) {
    q <- qsnorm(p, xi = 1, omega = 2, alpha = alpha)
    # Each smaller tail probability comes back to a small relative error
    tail_p <- ifelse(upper, 1 - p, p)
    tail_q <- ifelse(
      upper,
      psnorm(q, xi = 1, omega = 2, alpha = alpha, lower_tail = FALSE),
      psnorm(q, xi = 1, omega = 2, alpha = alpha)
    )
    expect_lt(max(abs(tail_q / tail_p - 1)), 1e-6)
  }
  expect_equal(qsnorm(p), stats::qnorm(p))
})

test_that("qsnorm() matches sn::qsn()", {
  skip_if_not_installed("sn")
  p <- c(1e-6, 0.025, 0.5, 0.975, 1 - 1e-6)
  for (alpha in c(-6, -1, 1, 6)) {
    ref <- sn::qsn(p, xi = 0.5, omega = 2, alpha = alpha, tol = 1e-12)
    expect_equal(qsnorm(p, 0.5, 2, alpha), ref, tolerance = 1e-9)
  }
})

test_that("qsnorm() gives the infinite ends and NA outside [0, 1]", {
  expect_identical(qsnorm(c(0, 1), 0, 1, 2), c(-Inf, Inf))
  expect_true(all(is.na(qsnorm(c(-0.1, NA, 1.1), 0, 1, 2))))
  expect_true(is.na(qsnorm(0.5, 0, 0, 2)))
})

test_that("inlavaan() checks sn_fit_logthresh and sn_fit_temp", {
  fit_with <- function(...) {
    acfa(
      "visual =~ x1 + x2 + x3",
      lavaan::HolzingerSwineford1939,
      verbose = FALSE,
      test = "none",
      nsamp = 3,
      ...
    )
  }
  for (bad in list(0, 2, -Inf, NA, NA_real_, NULL, c(-6, -4), "-6")) {
    expect_error(fit_with(sn_fit_logthresh = bad), "sn_fit_logthresh")
  }
  for (bad in list(0, -1, Inf, NA, NA_real_, NULL, c(1, 2), "1")) {
    expect_error(fit_with(sn_fit_temp = bad), "sn_fit_temp")
  }
})

test_that("Skew-normal marginal quantiles are exact", {
  skip_if_not_installed("sn")
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  qcols <- c("2.5%", "25%", "50%", "75%", "97.5%")
  for (alpha in c(-6, -2, 0.5, 2, 6)) {
    res <- post_marg_skewnorm(
      j = 1,
      theta_star = 0.2,
      Sigma_theta = matrix(0.09),
      sn_params = cbind(xi = 0.2, omega = 0.3, alpha = alpha)
    )$summary
    expect_equal(
      unname(res[qcols]),
      sn::qsn(probs, 0.2, 0.3, alpha, tol = 1e-12),
      tolerance = 1e-8
    )
  }
  # The quantiles of a skew-normal fitted to draws
  set.seed(1)
  y <- stats::rgamma(500, shape = 3)
  res <- summarise_samples_sn(y)
  snp <- res$sn_params
  expect_equal(
    unname(res$summary[qcols]),
    sn::qsn(probs, snp[["xi"]], snp[["omega"]], snp[["alpha"]], tol = 1e-12),
    tolerance = 1e-8
  )
})
