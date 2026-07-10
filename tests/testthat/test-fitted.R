dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("fitted() returns model-implied moments for INLAvaan objects", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  fv <- fitted(fit)
  expect_type(fv, "list")
  expect_true("cov" %in% names(fv))
  ov <- lavaan::lavNames(fit, "ov")
  expect_equal(dim(fv$cov), c(length(ov), length(ov)))
  expect_true(isSymmetric(unclass(fv$cov)))
})

test_that("fitted() includes the mean vector with a mean structure", {
  fit <- acfa(mod, dat, meanstructure = TRUE, verbose = FALSE,
              nsamp = 3, test = "none")
  fv <- fitted(fit)
  expect_true("mean" %in% names(fv))
  expect_length(fv$mean, length(lavaan::lavNames(fit, "ov")))
})

test_that("fitted(type = 'ov') returns casewise predicted values", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  cw <- fitted(fit, type = "ov")
  expect_true(is.matrix(cw))
  expect_equal(nrow(cw), nrow(dat))
  expect_equal(ncol(cw), length(lavaan::lavNames(fit, "ov")))
})

test_that("fitted.values() matches fitted()", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  expect_equal(fitted.values(fit), fitted(fit))
})
