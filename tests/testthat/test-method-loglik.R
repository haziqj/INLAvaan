dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("logLik(type = 'marginal') returns the marginal log-likelihood", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  ll <- logLik(fit)
  expect_s3_class(ll, "inlavaan_logLik")
  expect_equal(as.numeric(ll), get_inlavaan_internal(fit)$mloglik)
  expect_no_error(capture.output(print(ll)))
})

test_that("logLik(type = 'plugin') is AIC/BIC-compatible", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "standard")
  ll <- logLik(fit, type = "plugin")
  expect_s3_class(ll, "logLik")
  expect_equal(attr(ll, "df"), length(coef(fit)))
  expect_equal(attr(ll, "nobs"), nobs(fit))
  expect_no_error(AIC(ll))
  expect_no_error(BIC(ll))
})

test_that("logLik(type = 'plugin') errors without DIC components", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  expect_error(logLik(fit, type = "plugin"), "DIC")
})

test_that("plain lavaan fits are unaffected by the INLAvaan logLik() method", {
  fit_lav <- lavaan::cfa(mod, dat)
  expect_s3_class(logLik(fit_lav), "logLik")
})

test_that("AIC()/BIC() on an INLAvaan fit redirect instead of computing", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "standard")
  expect_error(AIC(fit), "plugin")
  expect_error(BIC(fit), "plugin")
})

test_that("AIC()/BIC() still work via the logLik(type = 'plugin') escape hatch", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "standard")
  ll <- logLik(fit, type = "plugin")
  expect_true(is.finite(AIC(ll)))
  expect_true(is.finite(BIC(ll)))
  expect_equal(AIC(ll), -2 * as.numeric(ll) + 2 * attr(ll, "df"))
})

test_that("plain lavaan fits are unaffected by the INLAvaan AIC()/BIC() methods", {
  fit_lav <- lavaan::cfa(mod, dat)
  expect_no_error(AIC(fit_lav))
  expect_no_error(BIC(fit_lav))
})
