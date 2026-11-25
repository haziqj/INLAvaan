dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
fit_lav <- lavaan::cfa(mod, dat)
NSAMP <- 3
STDLV <- FALSE

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- acfa(
      mod,
      dat,
      marginal_method = "skewnorm",
      silent = TRUE,
      nsamp = NSAMP,
      std.lv = STDLV
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: gaussian", {
  expect_no_error({
    fit <- acfa(
      mod,
      dat,
      marginal_method = "gaussian",
      silent = TRUE,
      nsamp = NSAMP,
      std.lv = STDLV
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Method: sampling is not available", {
  expect_error(
    acfa(mod, dat, marginal_method = "sampling", silent = TRUE),
    class = "inlavaan_unavailable"
  )
})

test_that("Gradients are correct (Finite Difference Check)", {
  # The engine compares the analytic gradient at the mode with central
  # differences of the log-posterior, and grad_gap is the gap in posterior
  # SDs.
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  fit <- acfa(mod, dat, test = "none", silent = TRUE, nsamp = 3)
  gap <- get_inlavaan_internal(fit)$engine$diagnostics$marginals$grad_gap
  expect_true(all(is.finite(gap)))
  expect_lt(max(gap), 0.1)
})

test_that("Covariance coefficients are on the covariance scale", {
  # Regression test: coef() used to return the posterior-mean correlation
  # (tanh of theta) for covariance parameters, while summary() showed the
  # sample-based covariance. Invisible when factor sds are near 1, so use a
  # subsample where the two scales differ clearly.
  set.seed(20260612)
  dat_sub <- dat[sample(nrow(dat), 120L), ]
  fit <- acfa(mod, dat_sub, silent = TRUE, nsamp = NSAMP, test = "none")
  co <- coef(fit)
  summ <- get_inlavaan_internal(fit)$summary

  cov_names <- c("visual~~textual", "visual~~speed", "textual~~speed")
  expect_equal(
    unname(co[cov_names]),
    summ[cov_names, "Mean"],
    tolerance = 1e-10
  )
  # posterior-mean covariances must respect the positive-definiteness bound
  for (nm in cov_names) {
    lv <- strsplit(nm, "~~", fixed = TRUE)[[1]]
    bound <- sqrt(
      co[paste0(lv[1], "~~", lv[1])] * co[paste0(lv[2], "~~", lv[2])]
    )
    expect_lt(abs(co[nm]), bound)
  }
})

test_that("A single variance-into-covariance term keeps the gradient working", {
  # visual's variance is fixed, so only textual's enters the covariance
  expect_no_error(acfa(
    "visual =~ NA*x1 + x2 + x3\n textual =~ x4 + x5 + x6\n visual ~~ 1*visual",
    lavaan::HolzingerSwineford1939,
    silent = TRUE,
    nsamp = 3,
    test = "none"
  ))
})

test_that("A model with a single free parameter fits", {
  mod_one <- "
    speed =~ .616*x7 + .732*x8 + x9
    x7 ~~ .823*x7
    x8 ~~ .506*x8
    x9 ~~ .569*x9
  "
  for (method in c("skewnorm", "gaussian")) {
    expect_no_error({
      fit <- acfa(
        mod_one,
        lavaan::HolzingerSwineford1939,
        std.lv = TRUE,
        meanstructure = FALSE,
        marginal_method = method,
        silent = TRUE,
        nsamp = 3
      )
    })
    expect_length(coef(fit), 1)
    expect_equal(dim(vcov(fit)), c(1, 1))
  }
  expect_equal(dim(sampling(fit, nsamp = 3)), c(3, 1))
})
