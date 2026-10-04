test_that("Method: skewnorm", {
  set.seed(1234)
  X <- rnorm(100)
  M <- 0.5 * X + rnorm(100)
  Y <- 0.7 * M + rnorm(100)

  dat <- data.frame(X = X, Y = Y, M = M)
  mod <- "
    # Direct effect
    Y ~ c*X

    # Mediators
    M ~ a*X
    Y ~ b*M

    # Indirect effect (a*b)
    ab := a*b

    # Total effect
    total := c + (a*b)
  "

  fit_lav <- lavaan::sem(mod, dat)
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      verbose = FALSE
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

test_that("Defined parameters can use other defined parameters", {
  set.seed(1234)
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    textual ~ a*visual
    speed   ~ b*textual + c*visual

    # total uses ind before it is defined, and twice uses total after it
    total := c + ind
    ind   := a*b
    twice := 2*total
  "
  dat <- lavaan::HolzingerSwineford1939
  fit_lav <- lavaan::sem(mod, dat)
  expect_no_error({
    fit <- asem(mod, dat, verbose = FALSE, nsamp = 100, test = "none")
  })

  # Compare with the ML values implied by lavaan's a, b and c, each on the
  # scale of its posterior SD
  est <- lavaan::coef(fit_lav)
  ind <- est[["a"]] * est[["b"]]
  summ <- get_inlavaan_internal(fit)$summary
  expected <- c(
    ind = ind,
    total = est[["c"]] + ind,
    twice = 2 * (est[["c"]] + ind)
  )
  for (nm in names(expected)) {
    expect_lt(abs(summ[nm, "Mean"] - expected[[nm]]), summ[nm, "SD"])
  }
  expect_equal(
    summ["twice", "Mean"],
    2 * summ["total", "Mean"],
    tolerance = 1e-3
  )

  expect_no_error(out <- capture.output(summary(fit)))
})
