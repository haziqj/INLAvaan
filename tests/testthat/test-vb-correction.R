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
