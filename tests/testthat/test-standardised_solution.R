test_that("Standardised solution", {
  HS.model <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  utils::data("HolzingerSwineford1939", package = "lavaan")
  fit <- acfa(
    HS.model,
    data = HolzingerSwineford1939,
    marginal_correction = FALSE,
    vb_correction = FALSE,
    verbose = FALSE,
    nsamp = 3
  )
  expect_no_error(out <- standardisedsolution(fit))
  expect_no_error(out <- standardisedSolution(fit))
  expect_no_error(out <- standardizedsolution(fit))
  expect_no_error(out <- standardizedSolution(fit))

  idx <- which(
    paste0(out$lhs, out$op, out$rhs) %in%
      c("visual~~visual", "textual~~textual", "speed~~speed")
  )

  expect_equal(out$est.std[idx], rep(1, length(idx)))
  expect_s3_class(out, "data.frame")
})
