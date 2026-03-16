dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("vcov() returns a matrix for INLAvaan objects", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  vc <- suppressMessages(vcov(fit))
  expect_true(is.matrix(vc))
  expect_equal(nrow(vc), length(coef(fit)))
  expect_equal(ncol(vc), length(coef(fit)))
})
