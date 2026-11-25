test_that("saturated-mean intercepts get exactly zero VB shift", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  invisible(capture.output(suppressMessages(
    fit <- inlavaan(
      mod,
      data = lavaan::HolzingerSwineford1939,
      model.type = "cfa",
      meanstructure = TRUE,
      marginal_method = "marggaus",
      silent = TRUE,
      nsamp = 3,
      test = "none",
      debug = TRUE
    )
  )))

  pt <- fit$partable
  nu_id <- pt$free[pt$mat == "nu" & pt$free > 0]

  # The fast path must have recognised this mean structure for the shift to be
  # pinned; if it ever stops doing so, this expectation is the tripwire.
  expect_gt(length(nu_id), 0)
  expect_equal(fit$vb$correction[nu_id], rep(0, length(nu_id)))
  expect_true(any(abs(fit$vb$correction[-nu_id]) > 0))
})

test_that("the deterministic VB shift reports no quadrature error", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  fit <- inlavaan(
    mod,
    data = lavaan::HolzingerSwineford1939,
    model.type = "cfa",
    meanstructure = TRUE,
    marginal_method = "marggaus",
    silent = TRUE,
    nsamp = 3,
    test = "none"
  )

  glob <- diagnostics(fit, type = "global")
  par <- diagnostics(fit, type = "param")

  # The names stay, with NA: the shift comes from a deterministic rule
  expect_true(all(c("vb_mcse_max", "vb_mcse_mean") %in% names(glob)))
  expect_true("vb_mcse_sigma" %in% names(par))
  expect_true(is.na(glob[["vb_mcse_max"]]))
  expect_true(is.na(glob[["vb_mcse_mean"]]))
  expect_true(all(is.na(par$vb_mcse_sigma)))
  expect_equal(glob[["vb_applied"]], 1)
  expect_equal(glob[["vb_shift_max"]], max(abs(par$vb_shift_sigma)))
})

test_that("no VB correction means no quadrature error to report", {
  invisible(capture.output(suppressMessages(
    fit <- inlavaan(
      "visual =~ x1 + x2 + x3",
      data = lavaan::HolzingerSwineford1939,
      model.type = "cfa",
      vb_correction = FALSE,
      marginal_method = "marggaus",
      silent = TRUE,
      nsamp = 3,
      test = "none"
    )
  )))

  expect_true(is.na(diagnostics(fit, type = "global")[["vb_shift_max"]]))
  expect_true(is.na(diagnostics(fit, type = "global")[["vb_mcse_max"]]))
  expect_true(all(is.na(diagnostics(fit, type = "param")$vb_mcse_sigma)))
})

test_that("the VB shift adds its location term to the marginal log-likelihood", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  fit_with_vb <- function(vb_correction) {
    invisible(capture.output(suppressMessages(
      fit <- acfa(
        mod,
        lavaan::HolzingerSwineford1939,
        meanstructure = TRUE,
        vb_correction = vb_correction,
        marginal_method = "marggaus",
        silent = TRUE,
        nsamp = 3,
        test = "none"
      )
    )))
    fit
  }
  fit_on <- fit_with_vb(TRUE)
  fit_off <- fit_with_vb(FALSE)

  # The term is the KL divergence between the Laplace Gaussian and its
  # VB-shifted copy.
  int <- get_inlavaan_internal(fit_on)
  delta <- int$vb$correction
  kld <- 0.5 * drop(crossprod(delta, solve(int$Omega, delta)))
  expect_equal(diagnostics(fit_on)[["vb_kld_global"]], kld)

  # The VB step changes neither the mode nor the Hessian, so the two fits
  # share the Laplace value and differ by the location term alone. The term
  # is a small positive correction. The old code subtracted about m / 2.
  expect_equal(as.numeric(logLik(fit_on)) - as.numeric(logLik(fit_off)), kld)
  expect_gt(kld, 0)
  expect_lt(kld, 1)
})
