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

  fit_lav <- sem(mod, dat)
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
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})
