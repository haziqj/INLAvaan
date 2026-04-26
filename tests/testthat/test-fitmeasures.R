dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

# Fit shared models once (fast defaults)
fit_notest <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none",
                   vb_correction = FALSE, marginal_method = "marggaus")
fit_test   <- acfa(mod, dat, verbose = FALSE, nsamp = 10,
                   vb_correction = FALSE, marginal_method = "marggaus")

null_mod <- "
  x1 ~~ x1
  x2 ~~ x2
  x3 ~~ x3
  x4 ~~ x4
  x5 ~~ x5
  x6 ~~ x6
"
fit_null <- acfa(null_mod, dat, verbose = FALSE, nsamp = 3,
                 vb_correction = FALSE, marginal_method = "marggaus")

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

test_that("Incremental indices require baseline.model", {
  fm <- fitMeasures(fit_test)
  inc_names <- c("BCFI", "BTLI", "BNFI")
  for (nm in inc_names) {
    expect_false(nm %in% names(fm), info = paste(nm, "should be absent"))
  }
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

test_that("Bayesian fit indices stay on the native backend when available", {
  int <- fit_test@external$inlavaan_internal
  expect_false(is.null(int$native_backend))

  lavoptions <- reconstruct_lavoptions(fit_test)
  chisq_native <- compute_chisq_dev(
    x_samp = sample_params(
      theta_star = int$theta_star,
      Sigma_theta = int$Sigma_theta,
      method = int$marginal_method,
      approx_data = int$approx_data,
      pt = int$partable,
      lavmodel = int$lavmodel,
      nsamp = 4L,
      R_star = int$R_star
    )$x_samp,
    lavmodel = int$lavmodel,
    lavsamplestats = int$lavsamplestats,
    lavdata = int$lavdata,
    lavoptions = lavoptions,
    lavcache = fit_test@Cache,
    native_backend = int$native_backend
  )
  expect_true(all(is.finite(chisq_native)))
  expect_true(all(chisq_native >= 0))
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
  expect_equal(colnames(tab), c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode"))
  expect_equal(nrow(tab), length(bfi$indices))
  expect_equal(rownames(tab), names(bfi$indices))
})

test_that("print.bfit_indices runs without error", {
  bfi <- bfit_indices(fit_test)
  expect_output(print(bfi))
})

test_that("bfit_indices details has expected fields", {
  bfi <- bfit_indices(fit_test)
  expect_true(all(c("chisq", "df", "pD", "rescale", "nsamp") %in%
                    names(bfi$details)))
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
  expect_error(bfit_indices(fit_test, baseline.model = "not_a_model"), class = "error")
})
