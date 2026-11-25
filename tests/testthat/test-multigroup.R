dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 # + x3
    textual =~ x4 + x5 # + x6
    # speed   =~ x7 + x8 + x9
  "
NSAMP <- 3
STDLV <- TRUE

test_that("Multigroup fitting and testing", {
  # Configural invariance
  expect_no_error({
    fit1 <- acfa(
      mod,
      dat,
      silent = TRUE,
      test = "none",
      std.lv = STDLV,
      group = "school"
    )
  })
  expect_no_error(out <- capture.output(summary(fit1)))

  # The rest of the ladder adds two fits and a compare() call. Convergence
  # (dx ~ 0) also depends on the platform's BLAS/compiler, so CI only.
  skip_on_cran()

  # Weak invariance
  expect_no_error({
    fit2 <- acfa(
      mod,
      dat,
      silent = TRUE,
      test = "none",
      std.lv = STDLV,
      group = "school",
      group.equal = "loadings"
    )
  })
  expect_no_error(out <- capture.output(summary(fit2)))

  # Weak invariance
  expect_no_error({
    fit3 <- acfa(
      mod,
      dat,
      silent = TRUE,
      test = "none",
      std.lv = STDLV,
      group = "school",
      group.equal = c("intercepts", "loadings")
    )
  })
  expect_no_error(out <- capture.output(summary(fit3)))

  # Comparison
  expect_no_error({
    cp <- compare(fit1, fit2, fit3)
    out <- capture.output(print(cp))
  })

  expect_equal(fit1@optim$dx, rep(0, length(fit1@optim$dx)), tolerance = 1e-3)
  expect_equal(fit2@optim$dx, rep(0, length(fit2@optim$dx)), tolerance = 1e-3)
  expect_equal(fit3@optim$dx, rep(0, length(fit3@optim$dx)), tolerance = 1e-3)
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
