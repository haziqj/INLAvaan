mod <- "
  # intercept and slope with fixed coefficients
  i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
  s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # regressions
  i ~ x1 + x2
  s ~ x1 + x2

  # time-varying covariates
  t1 ~ c1
  t2 ~ c2
  t3 ~ c3
  t4 ~ c4
"
dat <- lavaan::Demo.growth
fit_lav <- lavaan::cfa(mod, dat)
NSAMP <- 3

test_that("Method: gaussian (fast)", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "gaussian",
      vb_correction = FALSE,
      test = "none",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "skewnorm",
      vb_correction = FALSE,
      test = "none",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: sampling is not available", {
  expect_error(
    agrowth(mod, dat, marginal_method = "sampling", silent = TRUE),
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
  fit <- agrowth(mod, dat, test = "none", silent = TRUE, nsamp = 3)
  gap <- get_inlavaan_internal(fit)$engine$diagnostics$marginals$grad_gap
  expect_true(all(is.finite(gap)))
  expect_lt(max(gap), 0.1)
})

test_that("Latent change score model with constant change", {
  dat <- lavaan::Demo.growth
  colnames(dat)[1:4] <- c("X1", "X2", "X3", "X4")
  mod <- "
    lX1 =~ 1*X1
    lX2 =~ 1*X2
    lX3 =~ 1*X3
    lX4 =~ 1*X4
    lX2 ~ 1*lX1
    lX3 ~ 1*lX2
    lX4 ~ 1*lX3
    dX1 =~ 1*lX2
    dX2 =~ 1*lX3
    dX3 =~ 1*lX4
    intX   =~ 1*lX1
    slopeX =~ 1*dX1 + 1*dX2 + 1*dX3
    X1 ~~ rsdX*X1
    X2 ~~ rsdX*X2
    X3 ~~ rsdX*X3
    X4 ~~ rsdX*X4
    X1 ~ 0*1
    X2 ~ 0*1
    X3 ~ 0*1
    X4 ~ 0*1
    dX1 ~ 0*lX1
    dX2 ~ 0*lX2
    dX3 ~ 0*lX3
    slopeX ~ 1
    intX ~ 1
    slopeX ~~ slopeX
    intX ~~ intX
    slopeX ~~ intX
    lX1 ~ 0*1
    lX2 ~ 0*1
    lX3 ~ 0*1
    lX4 ~ 0*1
    dX1 ~ 0*1
    dX2 ~ 0*1
    dX3 ~ 0*1
    lX1 ~~ 0*lX1
    lX2 ~~ 0*lX2
    lX3 ~~ 0*lX3
    lX4 ~~ 0*lX4
    dX1 ~~ 0*dX1
    dX2 ~~ 0*dX2
    dX3 ~~ 0*dX3
  "
  fit <- inlavaan(mod, dat, test = "none", nsamp = 3, silent = TRUE)
  expect_s4_class(fit, "INLAvaan")
  expect_no_error(capture.output(summary(fit)))
  rsd <- coef(fit)[paste0("X", 1:4, "~~X", 1:4)]
  expect_equal(unname(rsd), rep(unname(rsd[1]), 4))
})
