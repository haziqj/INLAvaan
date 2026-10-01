dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

# Fit shared models once (fast defaults)
fit_notest <- acfa(
  mod,
  dat,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus"
)
fit_test <- acfa(
  mod,
  dat,
  verbose = FALSE,
  nsamp = 10,
  vb_correction = FALSE,
  marginal_method = "marggaus"
)

null_mod <- "
  x1 ~~ x1
  x2 ~~ x2
  x3 ~~ x3
  x4 ~~ x4
  x5 ~~ x5
  x6 ~~ x6
"
fit_null <- acfa(
  null_mod,
  dat,
  verbose = FALSE,
  nsamp = 3,
  vb_correction = FALSE,
  marginal_method = "marggaus"
)

test_that("Basic fitMeasures returns expected names", {
  fm <- fitMeasures(fit_notest)
  expect_s3_class(fm, "fitmeasures.inlavaan_internal")
  expect_true("npar" %in% names(fm))
  expect_true("margloglik" %in% names(fm))
})

test_that("Bayesian absolute fit indices are computed with test != 'none'", {
  fm <- fitMeasures(fit_test)
  abs_names <- c("BRMSEA", "BGammaHat", "adjBGammaHat", "BMc")
  for (nm in abs_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
  expect_true(fm["BRMSEA"] >= 0)
  expect_true(fm["BGammaHat"] > 0 && fm["BGammaHat"] <= 1)
  expect_true(fm["BMc"] > 0 && fm["BMc"] <= 1)
})

test_that("Incremental indices use an automatic independence baseline", {
  fm <- fitMeasures(fit_test)
  inc_names <- c("BCFI", "BTLI", "BNFI")
  for (nm in inc_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
  # Two correlated factors remove most of the independence misfit
  expect_true(fm["BCFI"] > 0.5 && fm["BCFI"] <= 1)
  # The same numbers as an explicit independence baseline, up to the
  # Monte Carlo noise of the draws
  fm_null <- fitMeasures(fit_test, baseline.model = fit_null)
  expect_equal(unname(fm["BCFI"]), unname(fm_null["BCFI"]), tolerance = 0.2)
})

test_that("baseline.model = FALSE skips the incremental indices", {
  fm <- fitMeasures(fit_test, baseline.model = FALSE)
  for (nm in c("BCFI", "BTLI", "BNFI")) {
    expect_false(nm %in% names(fm), info = paste(nm, "should be absent"))
  }
  expect_true("BRMSEA" %in% names(fm))
})

test_that("Absolute indices alone do not fit a baseline", {
  fm <- fitMeasures(fit_test, fit.measures = c("BRMSEA", "BGammaHat"))
  expect_setequal(names(fm), c("BRMSEA", "BGammaHat"))
})

test_that("An independence model has no incremental indices of its own", {
  fm <- fitMeasures(fit_null)
  expect_false("BCFI" %in% names(fm))
  expect_true("BRMSEA" %in% names(fm))
})

test_that("A baseline with the same free parameters warns", {
  expect_warning(
    fitMeasures(fit_test, baseline.model = fit_test),
    "same free parameters"
  )
})

test_that("Incremental indices computed with baseline.model", {
  fm <- fitMeasures(fit_test, baseline.model = fit_null)
  inc_names <- c("BCFI", "BTLI", "BNFI")
  for (nm in inc_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
})

test_that("baseline.model must be INLAvaan", {
  expect_error(fitMeasures(fit_test, baseline.model = "not_a_model"))
})

test_that("Selecting specific fit measures works", {
  fm <- fitMeasures(fit_test, fit.measures = c("BRMSEA", "BMc"))
  expect_equal(length(fm), 2)
  expect_named(fm, c("BRMSEA", "BMc"))
})

test_that("rescale = 'MCMC' produces fit indices", {
  fm <- fitMeasures(fit_test, rescale = "MCMC")
  abs_names <- c("BRMSEA", "BGammaHat", "adjBGammaHat", "BMc")
  for (nm in abs_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
  expect_true(fm["BRMSEA"] >= 0)
})

test_that("rescale = 'devM' and 'MCMC' give different results", {
  fm_devm <- fitMeasures(fit_test, fit.measures = "BRMSEA", rescale = "devM")
  fm_mcmc <- fitMeasures(fit_test, fit.measures = "BRMSEA", rescale = "MCMC")
  expect_false(identical(fm_devm, fm_mcmc))
})

# --- bfit_indices S3 class tests ---

test_that("bfit_indices returns correct S3 class", {
  bfi <- bfit_indices(fit_test)
  expect_s3_class(bfi, "bfit_indices")
  expect_true(is.list(bfi$indices))
  expect_true(is.list(bfi$details))
})

test_that("bfit_indices stores per-sample vectors", {
  bfi <- bfit_indices(fit_test)
  for (nm in names(bfi$indices)) {
    expect_true(is.numeric(bfi$indices[[nm]]))
    expect_equal(length(bfi$indices[[nm]]), 10)
  }
})

test_that("summary.bfit_indices returns data.frame with correct columns", {
  bfi <- bfit_indices(fit_test)
  tab <- summary(bfi)
  expect_s3_class(tab, "data.frame")
  expect_equal(
    colnames(tab),
    c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
  )
  expect_equal(nrow(tab), length(bfi$indices))
  expect_equal(rownames(tab), names(bfi$indices))
})

test_that("print.bfit_indices runs without error", {
  bfi <- bfit_indices(fit_test)
  expect_output(print(bfi))
})

test_that("bfit_indices details has expected fields", {
  bfi <- bfit_indices(fit_test)
  expect_true(all(
    c("chisq", "df", "pD", "rescale", "nsamp") %in%
      names(bfi$details)
  ))
  expect_equal(bfi$details$nsamp, 10)
  expect_equal(bfi$details$rescale, "devM")
})

test_that("print.fitmeasures.inlavaan_internal formats output", {
  fm <- fitMeasures(fit_test)
  expect_output(print(fm), "npar")
  expect_output(print(fm), "margloglik")
})

test_that("fitMeasures errors on unrecognised measure names", {
  expect_error(fitMeasures(fit_test, fit.measures = "nonexistent_measure"))
})

test_that("bfit_indices errors for non-INLAvaan object", {
  expect_error(bfit_indices("not_a_model"), class = "error")
})

test_that("bfit_indices errors when DIC not available and rescale = devM", {
  expect_error(bfit_indices(fit_notest, rescale = "devM"), "DIC not available")
})

test_that("bfit_indices errors for non-INLAvaan baseline.model", {
  expect_error(
    bfit_indices(fit_test, baseline.model = "not_a_model"),
    class = "error"
  )
})

## ----- Saturated log-likelihood and moment count come from lavaan -----------

test_that("Moment count excludes fixed exogenous covariates, as in lavaan", {
  fit_x <- acfa(
    "f =~ x1 + x2 + x3\n f ~ ageyr",
    dat,
    verbose = FALSE,
    nsamp = 20,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )
  b <- bfit_indices(fit_x, baseline.model = FALSE)
  # df = p - pD, so p = df + pD must be lavaan's count (9, not 10)
  expect_equal(
    b$details$df + b$details$pD,
    lavaan::lav_partable_ndat(fit_x@ParTable)
  )
  expect_equal(lavaan::lav_partable_ndat(fit_x@ParTable), 9)
})

test_that("Two-level fit indices are finite and sane", {
  skip_on_cran()
  d2 <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:60, ]
  mod2 <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
  "
  fit2 <- asem(
    mod2,
    d2,
    cluster = "cluster",
    verbose = FALSE,
    nsamp = 30,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )
  fm <- fitMeasures(fit2)
  idx <- c("BRMSEA", "BGammaHat", "BCFI", "BTLI")
  expect_true(all(is.finite(fm[idx])))
  # The generating model: close fit, no longer BGammaHat = 1 and BTLI = 3.5
  expect_lt(fm["BRMSEA"], 0.1)
  expect_gt(fm["BCFI"], 0.9)
  expect_lt(fm["BTLI"], 1.1)
  b <- bfit_indices(fit2, baseline.model = FALSE)
  expect_equal(
    b$details$df + b$details$pD,
    lavaan::lav_partable_ndat(fit2@ParTable)
  )

  # Missing data at level 1: the h1 log-likelihood covers it too
  set.seed(3)
  d2$y1[sample(nrow(d2), 40)] <- NA
  fit2m <- asem(
    mod2,
    d2,
    cluster = "cluster",
    missing = "ml",
    verbose = FALSE,
    nsamp = 30,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )
  fmm <- fitMeasures(fit2m)
  expect_true(all(is.finite(fmm[idx])))
})
