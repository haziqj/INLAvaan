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
      verbose = FALSE,
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

test_that("diagnostics() reports the VB shift's own quadrature error", {
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
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )))

  glob <- diagnostics(fit, type = "global")
  par <- diagnostics(fit, type = "param")

  expect_true(all(c("vb_mcse_max", "vb_mcse_mean") %in% names(glob)))
  expect_true("vb_mcse_sigma" %in% names(par))
  expect_true(all(is.finite(par$vb_mcse_sigma)))
  expect_true(all(par$vb_mcse_sigma >= 0))
  expect_equal(glob[["vb_mcse_max"]], max(par$vb_mcse_sigma))
  expect_equal(glob[["vb_mcse_mean"]], mean(par$vb_mcse_sigma))

  # Pinned coordinates use no quadrature, so they carry no quadrature error.
  int <- get_inlavaan_internal(fit)
  nu_id <- int$partable$free[int$partable$mat == "nu" & int$partable$free > 0]
  expect_equal(par$vb_mcse_sigma[nu_id], rep(0, length(nu_id)))
})

test_that("no VB correction means no quadrature error to report", {
  invisible(capture.output(suppressMessages(
    fit <- inlavaan(
      "visual =~ x1 + x2 + x3",
      data = lavaan::HolzingerSwineford1939,
      model.type = "cfa",
      vb_correction = FALSE,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )))

  expect_true(is.na(diagnostics(fit, type = "global")[["vb_mcse_max"]]))
  expect_true(all(is.na(diagnostics(fit, type = "param")$vb_mcse_sigma)))
})
