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

test_that("Skew-normal sample fit does not stall at alpha = 0", {
  # Draws with skewness about 0.3. A single start at alpha = 0 stalled at
  # alpha of about 0.01, with a log-likelihood about 7.5 below the optimum.
  set.seed(288)
  d <- 1.5 / sqrt(1 + 1.5^2)
  y <- d * abs(rnorm(1000)) + sqrt(1 - d^2) * rnorm(1000)
  yc <- y - mean(y)
  expect_equal(mean(yc^3) / mean(yc^2)^1.5, 0.3, tolerance = 0.05)

  loglik <- function(p) sum(dsnorm(y, p$xi, p$omega, p$alpha, log = TRUE))
  res <- fit_skew_normal_samp(y)
  normal <- list(xi = mean(y), omega = sqrt(mean(yc^2)), alpha = 0)
  expect_gt(res$alpha, 1)
  expect_gt(loglik(res) - loglik(normal), 7)

  # Draws with the opposite skew give the mirror image
  neg <- fit_skew_normal_samp(-y)
  expect_equal(
    c(-neg$xi, neg$omega, -neg$alpha),
    unlist(res),
    ignore_attr = TRUE
  )
})

test_that("Skew-normal sample fit is stable under a little noise", {
  # On these draws, noise of 1e-3 SD tipped a single start at alpha = 0
  # between alpha of about 0.01 and the optimum.
  d <- 1 / sqrt(2)
  for (seed in c(18, 37, 217)) {
    set.seed(seed)
    y <- d * abs(rnorm(1000)) + sqrt(1 - d^2) * rnorm(1000)
    y_noisy <- y + 1e-3 * rnorm(1000)
    alpha <- fit_skew_normal_samp(y)$alpha
    alpha_noisy <- fit_skew_normal_samp(y_noisy)$alpha
    expect_gt(min(alpha, alpha_noisy), 0.5)
    expect_equal(alpha_noisy, alpha, tolerance = 0.01)
  }
})

test_that("Skew-normal sample fit keeps the mirrored start when it is better", {
  # Heavy-tailed draws with a sample skewness of about -0.007. The moment-based
  # start, with negative skew, reaches alpha of about -0.63. The mirrored start
  # reaches alpha of about 0.66, with a log-likelihood about 0.006 larger.
  set.seed(306061)
  y <- rt(4000, 8)
  yc <- y - mean(y)
  expect_lt(mean(yc^3) / mean(yc^2)^1.5, 0)

  # The best fit with alpha >= 0 and the best fit with alpha <= 0
  nll <- function(par) -sum(dsnorm(y, par[1], exp(par[2]), par[3], log = TRUE))
  start <- c(mean(y), log(sd(y)), 0)
  pos <- nlminb(start + c(-0.5, 0, 0.5), nll, lower = c(-Inf, -Inf, 0))
  neg <- nlminb(start + c(0.5, 0, -0.5), nll, upper = c(Inf, Inf, 0))
  expect_gt(neg$objective - pos$objective, 0.003)
  expect_lt(neg$par[3], -0.5)

  # The fit returns the larger likelihood, with positive skew
  res <- fit_skew_normal_samp(y)
  loglik <- sum(dsnorm(y, res$xi, res$omega, res$alpha, log = TRUE))
  expect_gt(loglik + neg$objective, 0.003)
  expect_equal(loglik, -pos$objective, tolerance = 1e-8)
  expect_equal(res$alpha, pos$par[3], tolerance = 1e-3)
})

test_that("Skew-normal sample fit has the right gradient and Hessian", {
  skip_if_not_installed("numDeriv")
  set.seed(1)
  z <- rt(200, 5)
  nll <- sn_samp_nll(z)
  ref <- function(par) -sum(dsnorm(z, par[1], exp(par[2]), par[3], log = TRUE))
  pars <- list(c(0.1, -0.2, 0.7), c(-0.3, 0.1, -3), c(0, 0, 0), c(0.5, 0.3, 12))
  for (par in pars) {
    expect_equal(nll$fn(par), ref(par), tolerance = 1e-12)
    expect_equal(nll$gr(par), numDeriv::grad(ref, par), tolerance = 1e-6)
    expect_equal(nll$hs(par), numDeriv::jacobian(nll$gr, par), tolerance = 1e-6)
    expect_equal(nll$hs(par), numDeriv::hessian(ref, par), tolerance = 1e-4)
  }
})

test_that("Skew-normal sample fit has a stable Hessian when alpha u is very negative", {
  skip_if_not_installed("numDeriv")
  # The gap x + r of the Mills ratio r at x = -t against its asymptotic series
  t <- 10^(2:6)
  expect_equal(
    sn_mills_gap(t),
    1 / t - 2 / t^3 + 10 / t^5 - 74 / t^7,
    tolerance = 1e-12
  )

  # Points where alpha u reaches about -5e4, -8e5 and -4e6. With the direct
  # form -r (alpha u + r) of the derivative of r, which has the wrong sign at
  # alpha u = -1e5, the Hessian is 75% and 100% off at the first two points.
  set.seed(1)
  z <- rt(200, 5)
  nll <- sn_samp_nll(z)
  pars <- list(c(0.1, -0.2, 1e4), c(0.1, -0.2, -1e5), c(0, 0, 1e6))
  for (par in pars) {
    expect_equal(nll$hs(par), numDeriv::jacobian(nll$gr, par), tolerance = 1e-8)
  }
})

# Record the starts that fit_skew_normal_samp() runs. A function in `fake`, at
# the position of a start, replaces the fit from that start.
local_record_starts <- function(fake = list(), env = parent.frame()) {
  rec <- new.env()
  rec$starts <- list()
  local_mocked_bindings(
    sn_samp_nlminb = function(start, nll) {
      i <- length(rec$starts) + 1
      rec$starts[[i]] <- start
      if (i <= length(fake) && is.function(fake[[i]])) {
        return(fake[[i]](nll))
      }
      stats::nlminb(start, nll$fn, nll$gr, nll$hs)
    },
    .env = env
  )
  rec
}

# The fits from all three starts of fit_skew_normal_samp(), and the result
# from the best of them
all_three_fits <- function(y) {
  m <- stats::median(y)
  s <- stats::sd(y)
  z <- (y - m) / s
  nll <- sn_samp_nll(z)
  zc <- z - mean(z)
  d0 <- sn_delta_from_skew(mean(zc^3) / mean(zc^2)^1.5)
  if (abs(d0) < 0.2) {
    d0 <- 0.5
  }
  starts <- lapply(c(d0, -d0), function(d) {
    om <- 1 / sqrt(1 - 2 * d^2 / pi)
    c(mean(z) - om * d * sqrt(2 / pi), log(om), d / sqrt(1 - d^2))
  })
  fits <- lapply(c(starts, list(c(0, 0, 0))), function(st) {
    stats::nlminb(st, nll$fn, nll$gr, nll$hs)
  })
  to_list <- function(f) {
    list(xi = m + s * f$par[1], omega = s * exp(f$par[2]), alpha = f$par[3])
  }
  best <- fits[[which.min(vapply(fits, `[[`, 0, "objective"))]]
  list(fits = fits, best = to_list(best), to_list = to_list)
}

test_that("Skew-normal sample fit skips the third start on opposite sides", {
  # The fits from the two mirrored starts end at alpha of about -0.63 and 0.66,
  # so the fit from alpha = 0 does not run. It would not improve the result.
  set.seed(306061)
  y <- rt(4000, 8)
  ref <- all_three_fits(y)
  alphas <- vapply(ref$fits, function(f) f$par[3], 0)
  expect_lt(alphas[1] * alphas[2], 0)

  rec <- local_record_starts()
  res <- fit_skew_normal_samp(y)
  expect_length(rec$starts, 2)
  expect_false(any(vapply(rec$starts, function(st) all(st == 0), TRUE)))
  expect_equal(res, ref$best)
})

test_that("Skew-normal sample fit runs the third start on the same side", {
  # The fits from the two mirrored starts both end at alpha of about -1.31, so
  # the fit from alpha = 0 also runs. The result is the best of the three.
  set.seed(17)
  y <- rt(50, 5) + 0.3 * abs(rnorm(50))
  ref <- all_three_fits(y)
  alphas <- vapply(ref$fits, function(f) f$par[3], 0)
  expect_gte(alphas[1] * alphas[2], 0)

  rec <- local_record_starts()
  res <- fit_skew_normal_samp(y)
  expect_length(rec$starts, 3)
  expect_identical(rec$starts[[3]], c(0, 0, 0))
  expect_equal(res, ref$best)
})

test_that("Skew-normal sample fit keeps the third start when it wins", {
  # The fits from the two mirrored starts are replaced by fits that stall near
  # alpha = 0 on the same side, so the fit from alpha = 0 runs and wins.
  set.seed(17)
  y <- rt(50, 5) + 0.3 * abs(rnorm(50))
  ref <- all_three_fits(y)
  stall <- function(alpha) {
    function(nll) {
      par <- c(0, 0, alpha)
      list(par = par, objective = nll$fn(par))
    }
  }
  rec <- local_record_starts(list(stall(0.01), stall(0.02)))
  res <- fit_skew_normal_samp(y)
  expect_length(rec$starts, 3)
  expect_equal(res, ref$to_list(ref$fits[[3]]))
})

test_that("Skew-normal sample fit falls back when a fit is not finite", {
  set.seed(17)
  y <- rt(50, 5) + 0.3 * abs(rnorm(50))
  ref <- all_three_fits(y)
  nan_fit <- function(nll) list(par = rep(NaN, 3), objective = NaN)

  # A non-finite first fit runs the third start, and the best finite fit wins
  rec <- local_record_starts(list(nan_fit))
  res <- fit_skew_normal_samp(y)
  expect_length(rec$starts, 3)
  objs <- vapply(ref$fits[2:3], `[[`, 0, "objective")
  expect_equal(res, ref$to_list(ref$fits[[which.min(objs) + 1]]))

  # With no finite fit, the result is the third start, alpha = 0
  rec <- local_record_starts(list(nan_fit, nan_fit, nan_fit))
  res <- fit_skew_normal_samp(y)
  expect_length(rec$starts, 3)
  expect_equal(
    res,
    list(xi = stats::median(y), omega = stats::sd(y), alpha = 0)
  )
})

test_that("Skew-normal sample fit warns when the draws cannot be fitted", {
  # Draws that do not vary give the point mass, with alpha = 0
  expect_warning(res <- fit_skew_normal_samp(rep(3, 100)), "must vary")
  expect_identical(res, list(xi = 3, omega = 0, alpha = 0))

  # One draw, or draws with NA, give NA, and draws with Inf give NaN
  set.seed(1)
  y <- rnorm(99)
  na <- list(xi = NA_real_, omega = NA_real_, alpha = 0)
  expect_warning(res <- fit_skew_normal_samp(3), "must vary")
  expect_identical(res, na)
  expect_warning(res <- fit_skew_normal_samp(c(y, NA)), "must vary")
  expect_identical(res, na)
  expect_warning(res <- fit_skew_normal_samp(c(y, Inf)), "must vary")
  expect_identical(res, list(xi = NaN, omega = NaN, alpha = 0))
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
    Omega = matrix(v)
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
    Omega = matrix(s^2)
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

test_that("inlavaan() checks the threshold and the grid of the scans", {
  local_deprecation_warnings()
  fit_with <- function(...) {
    acfa(
      "visual =~ x1 + x2 + x3",
      lavaan::HolzingerSwineford1939,
      silent = TRUE,
      test = "none",
      nsamp = 3,
      ...
    )
  }
  for (bad in list(0, 2, -Inf, NA, NA_real_, c(-6, -4), "-6")) {
    expect_error(fit_with(control = list(log_threshold = bad)), "log_threshold")
    # The deprecated argument is checked under its own name
    expect_error(fit_with(sn_fit_logthresh = bad), "sn_fit_logthresh")
  }
  for (bad in list(6, 0, Inf, NA, NA_real_, c(21, 31), "21")) {
    expect_error(fit_with(control = list(n_grid = bad)), "n_grid")
    err <- expect_error(fit_with(sn_fit_ngrid = bad), "sn_fit_ngrid")
    expect_false(inherits(err, "inlavanilla_error"))
  }
  expect_no_error(fit_with(control = list(n_grid = 7)))
  expect_no_error(fit_with(sn_fit_ngrid = 7))
  # The temperature is always 1, and sn_fit_temp has no effect
  expect_no_error(fit_with(sn_fit_temp = NA))
})

test_that("Skew-normal marginal quantiles are exact", {
  skip_if_not_installed("sn")
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  qcols <- c("2.5%", "25%", "50%", "75%", "97.5%")
  for (alpha in c(-6, -2, 0.5, 2, 6)) {
    res <- post_marg_skewnorm(
      j = 1,
      theta_star = 0.2,
      Omega = matrix(0.09),
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
