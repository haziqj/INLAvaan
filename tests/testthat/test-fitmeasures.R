dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("Basic fitMeasures returns expected names", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  fm <- fitMeasures(fit)
  expect_s3_class(fm, "fitmeasures.inlavaan_internal")
  expect_true("npar" %in% names(fm))
  expect_true("margloglik" %in% names(fm))
})

test_that("Bayesian absolute fit indices are computed with test != 'none'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  fm <- fitMeasures(fit)
  abs_names <- c("BRMSEA", "BGammaHat", "adjBGammaHat", "BMc")
  for (nm in abs_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
  # BRMSEA should be non-negative

  expect_true(fm["BRMSEA"] >= 0)
  # BGammaHat and adjBGammaHat should be in (0, 1]
  expect_true(fm["BGammaHat"] > 0 && fm["BGammaHat"] <= 1)
  # BMc should be in (0, 1]
  expect_true(fm["BMc"] > 0 && fm["BMc"] <= 1)
})

test_that("Incremental indices require baseline.model", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  fm <- fitMeasures(fit)
  inc_names <- c("BCFI", "BTLI", "BNFI")
  # Without baseline.model, incremental indices should not appear
  for (nm in inc_names) {
    expect_false(nm %in% names(fm), info = paste(nm, "should be absent"))
  }
})

test_that("Incremental indices computed with baseline.model", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)

  # Fit a baseline (independence) model
  null_mod <- "
    x1 ~~ x1
    x2 ~~ x2
    x3 ~~ x3
    x4 ~~ x4
    x5 ~~ x5
    x6 ~~ x6
  "
  fit_null <- acfa(null_mod, dat, verbose = FALSE, nsamp = 3)

  fm <- fitMeasures(fit, baseline.model = fit_null)
  inc_names <- c("BCFI", "BTLI", "BNFI")
  for (nm in inc_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
})

test_that("baseline.model must be INLAvaan", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  expect_error(fitMeasures(fit, baseline.model = "not_a_model"))
})

test_that("Selecting specific fit measures works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  fm <- fitMeasures(fit, fit.measures = c("BRMSEA", "BMc"))
  expect_equal(length(fm), 2)
  expect_named(fm, c("BRMSEA", "BMc"))
})

test_that("rescale = 'MCMC' produces fit indices", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3)
  fm <- fitMeasures(fit, rescale = "MCMC")
  abs_names <- c("BRMSEA", "BGammaHat", "adjBGammaHat", "BMc")
  for (nm in abs_names) {
    expect_true(nm %in% names(fm), info = paste(nm, "missing"))
  }
  expect_true(fm["BRMSEA"] >= 0)
})

test_that("rescale = 'devM' and 'MCMC' give different results", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 10)
  fm_devm <- fitMeasures(fit, fit.measures = "BRMSEA", rescale = "devM")
  fm_mcmc <- fitMeasures(fit, fit.measures = "BRMSEA", rescale = "MCMC")
  # They should not be identical due to different rescaling
  expect_false(identical(fm_devm, fm_mcmc))
})

# --- bfit_indices S3 class tests ---

test_that("bfit_indices returns correct S3 class", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  bfi <- bfit_indices(fit)
  expect_s3_class(bfi, "bfit_indices")
  expect_true(is.list(bfi$indices))
  expect_true(is.list(bfi$details))
})

test_that("bfit_indices stores per-sample vectors", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  bfi <- bfit_indices(fit)
  for (nm in names(bfi$indices)) {
    expect_true(is.numeric(bfi$indices[[nm]]))
    expect_equal(length(bfi$indices[[nm]]), 5)
  }
})

test_that("summary.bfit_indices returns data.frame with correct columns", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 10)
  bfi <- bfit_indices(fit)
  tab <- summary(bfi)
  expect_s3_class(tab, "data.frame")
  expect_equal(colnames(tab), c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode"))
  expect_equal(nrow(tab), length(bfi$indices))
  expect_equal(rownames(tab), names(bfi$indices))
})

test_that("print.bfit_indices runs without error", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  bfi <- bfit_indices(fit)
  expect_output(print(bfi))
})

test_that("bfit_indices details has expected fields", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  bfi <- bfit_indices(fit)
  expect_true(all(c("chisq", "df", "pD", "rescale", "nsamp") %in%
                    names(bfi$details)))
  expect_equal(bfi$details$nsamp, 5)
  expect_equal(bfi$details$rescale, "devM")
})
