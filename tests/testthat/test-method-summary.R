mod <- "
  visual  =~ x1 + a*x2 + b*x3
  textual =~ x4 + x5 + x6
  a == b
"
dat <- lavaan::HolzingerSwineford1939
fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")

test_that("summary(standardized = TRUE) works with equality constraints", {
  expect_no_error(
    out <- capture.output(summary(fit, standardized = TRUE, nsamp = 5))
  )
  expect_true(any(grepl("Std.lv", out)))
  expect_true(any(grepl("Std.all", out)))
})
