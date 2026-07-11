dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
fit <- acfa(mod, dat, marginal_method = "marggaus", vb_correction = FALSE, verbose = FALSE, nsamp = 3)

test_that("deviance(type = 'mean') returns posterior mean deviance with pD/DIC attrs", {
  dev <- deviance(fit)
  int <- get_inlavaan_internal(fit)
  expect_s3_class(dev, "inlavaan_deviance")
  expect_equal(as.numeric(dev), int$DIC$Dbar)
  expect_equal(attr(dev, "pD"), int$DIC$pD)
  expect_equal(attr(dev, "DIC"), int$DIC$dic)
  expect_no_error(capture.output(print(dev)))
})

test_that("deviance(type = 'plugin') matches -2 * logLik(type = 'plugin')", {
  dev_plugin <- deviance(fit, type = "plugin")
  expect_equal(as.numeric(dev_plugin), as.numeric(-2 * logLik(fit, type = "plugin")))
})

test_that("deviance() errors without DIC components", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  expect_error(deviance(fit), "DIC")
})

test_that("plain lavaan fits are unaffected by the INLAvaan deviance() method", {
  fit_lav <- lavaan::cfa(mod, dat)
  expect_error(deviance(fit_lav))
})
