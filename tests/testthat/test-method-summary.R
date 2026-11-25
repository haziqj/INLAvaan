mod <- "
  visual  =~ x1 + a*x2 + b*x3
  textual =~ x4 + x5 + x6
  a == b
"
dat <- lavaan::HolzingerSwineford1939
fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")

test_that("summary(standardized = TRUE) works with equality constraints", {
  expect_no_error(
    out <- capture.output(summary(fit, standardized = TRUE, nsamp = 5))
  )
  expect_true(any(grepl("Std.lv", out)))
  expect_true(any(grepl("Std.all", out)))
})

test_that("summary() without random slopes gains no annotation", {
  out <- capture.output(summary(fit))

  expect_false(any(grepl("random slope", out, fixed = TRUE)))
  # The user's own labels are left alone
  expect_true(any(grepl("(a)", out, fixed = TRUE)))
  expect_true(any(grepl("(b)", out, fixed = TRUE)))
})

test_that("summary(estimates = FALSE) prints the header only", {
  expect_no_error(out <- capture.output(summary(fit, estimates = FALSE)))
  expect_true(any(grepl("Number of model parameters", out)))
  expect_false(any(grepl("Parameter Estimates", out)))
})

test_that("summary(rsquare = TRUE) prints the R-square section", {
  expect_no_warning(out <- capture.output(summary(fit, rsquare = TRUE)))
  expect_true(any(grepl("R-Square", out)))
})

test_that("summary accepts the British spelling standardised = TRUE", {
  expect_no_error(
    out <- capture.output(summary(fit, standardised = TRUE, nsamp = 5))
  )
  expect_true(any(grepl("Std.lv", out)))
  expect_true(any(grepl("Std.all", out)))
})

test_that("summary(ci = FALSE) omits the credible interval columns", {
  out <- capture.output(summary(fit, ci = FALSE))
  expect_false(any(grepl("2.5%", out, fixed = TRUE)))
  out_ci <- capture.output(summary(fit))
  expect_true(any(grepl("2.5%", out_ci, fixed = TRUE)))
  expect_true(any(grepl("97.5%", out_ci, fixed = TRUE)))
})

test_that("summary(rsquare = TRUE, standardized = TRUE) rows stay aligned", {
  expect_no_error(
    out <- capture.output(
      summary(fit, rsquare = TRUE, standardized = TRUE, nsamp = 5)
    )
  )
  expect_true(any(grepl("R-Square", out)))
  expect_true(any(grepl("Std.all", out)))
})

test_that("summary() names the VB route of the fit", {
  vb_line <- function(fit) {
    out <- capture.output(summary(fit))
    line <- grep("VB correction", out, value = TRUE, fixed = TRUE)
    trimws(sub("VB correction", "", line, fixed = TRUE))
  }
  fit_vb <- function(vb_correction) {
    acfa(
      "visual =~ x1 + x2 + x3",
      dat,
      silent = TRUE,
      nsamp = 3,
      test = "none",
      vb_correction = vb_correction,
      marginal_method = "gaussian"
    )
  }
  # Before, "onestep" and "full" both gave TRUE
  fit_one <- fit_vb("onestep")
  expect_identical(vb_line(fit_one), "ONESTEP")
  expect_identical(vb_line(fit_vb("full")), "FULL")
  expect_identical(vb_line(fit_vb("none")), "NONE")
  # A fit that recorded no route
  fit_one@external$inlavaan_internal$vb$method <- NULL
  expect_identical(vb_line(fit_one), "YES")
})
