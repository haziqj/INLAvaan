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

test_that("standardisedsolution with postmedian and postmode", {
  mod <- "visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6"
  dat <- lavaan::HolzingerSwineford1939
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  out <- standardisedsolution(fit, postmedian = TRUE, postmode = TRUE)
  expect_true("median" %in% colnames(out))
  expect_true("mode" %in% colnames(out))
})

test_that("standardisedsolution dispatches to lavaan for lavaan objects", {
  mod <- "visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6"
  dat <- lavaan::HolzingerSwineford1939
  lav_fit <- lavaan::cfa(mod, dat)
  out <- standardisedsolution(lav_fit)
  expect_s3_class(out, "data.frame")
})

test_that("standardisedsolution handles := defined parameters", {
  mod <- "
    visual  =~ x1 + l2*x2 + x3
    textual =~ x4 + l5*x5 + x6
    ratio := l2 / l5
  "
  dat <- lavaan::HolzingerSwineford1939
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  out <- standardisedsolution(fit)
  expect_s3_class(out, "data.frame")
})
