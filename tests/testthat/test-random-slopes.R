## ----- Shared random-slope fixture (route A, 24 clusters) --------------------
# The `rv()` modifier makes the level-1 slope of x1 a level-2 latent
# variable. Twenty-four clusters (300 rows) keep every fit in this file to a
# couple of seconds.
d_rs <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
mod_rs <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~ w1
"
fit0_rs <- lavaan::sem(mod_rs, d_rs, cluster = "cluster", do.fit = FALSE)
fit_rs <- asem(
  mod_rs,
  d_rs,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)
# A second fit carrying the one fit measure a random-slope model keeps
fit_rs_dic <- asem(
  mod_rs,
  d_rs,
  cluster = "cluster",
  verbose = FALSE,
  test = "dic",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)
# The maximum-likelihood comparator. Two variances sit slightly below zero
# on this subset, which lavaan reports and the priors keep positive.
suppressWarnings(
  fit_lav <- lavaan::sem(mod_rs, d_rs, cluster = "cluster")
)

test_that("Random slopes are detected from the lavaan model", {
  expect_true(has_random_slopes(fit0_rs@Model))

  mod_fixed <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
  "
  fit0_fixed <- lavaan::sem(
    mod_fixed,
    d_rs,
    cluster = "cluster",
    do.fit = FALSE
  )
  expect_false(has_random_slopes(fit0_fixed@Model))
  expect_null(rs_spec(list(lavmodel = fit0_fixed@Model)))
})

test_that("rs_spec() describes the closed-form route", {
  spec <- rs_spec(list(lavmodel = fit0_rs@Model, lavcache = fit0_rs@Cache))

  expect_equal(spec$route, "A")
  expect_equal(spec$slopes, "s1")
  expect_setequal(spec$cond, c("x1", "w1"))
  expect_equal(spec$ncl, 24L)
  expect_equal(sum(spec$nobs), nrow(d_rs))

  # A fit stored before the cache was carried along cannot be described
  expect_error(
    rs_spec(list(lavmodel = fit0_rs@Model, lavcache = NULL)),
    class = "inlavaan_rs_cache"
  )
})

test_that("Random slopes: fit and posterior means", {
  expect_s4_class(fit_rs, "INLAvaan")

  # The slope variance is the boundary parameter here (its MLE is slightly
  # negative), so it is compared on its own
  keep <- setdiff(names(coef(fit_lav)), "s1~~s1.l2")
  expect_equal(coef(fit_rs)[keep], coef(fit_lav)[keep], tolerance = 0.15)
  expect_gte(unname(coef(fit_rs)["s1~~s1.l2"]), 0)

  # The stored cache travels with the fit, so rs_spec() works off the
  # INLAvaan object as well
  spec <- rs_spec(get_inlavaan_internal(fit_rs))
  expect_equal(spec$route, "A")
  expect_equal(spec$ncl, 24L)
})

test_that("Random slopes: fixed.x = FALSE is refused", {
  expect_error(
    asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      fixed.x = FALSE,
      verbose = FALSE,
      test = "none",
      nsamp = 3
    ),
    class = "inlavaan_rs_fixedx"
  )
})

test_that("Random slopes: PPP is dropped from the default test", {
  # Two warnings must stay inside: lavaan's "test statistics are not
  # available ... test set to none" and the silent PPP drop under the
  # default `test`
  expect_no_warning(
    fit_std <- asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "standard",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    )
  )

  rec <- get_inlavaan_internal(fit_std, "test")
  expect_false("ppp" %in% rec$computed)
  expect_true("dic" %in% rec$computed)
  expect_true("ppp" %in% rec$requested)
  expect_true("ppp" %in% names(rec$skipped))
  expect_match(rec$skipped[["ppp"]], "within-cluster covariance")

  # Naming ppp explicitly is worth a warning
  expect_warning(
    asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "ppp",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    ),
    class = "inlavaan_rs_ppp"
  )
})

test_that("Random slopes: fitmeasures keeps only what exists", {
  fm <- fitMeasures(fit_rs_dic)

  expect_true(all(c("npar", "margloglik", "dic", "p_dic") %in% names(fm)))
  gone <- c(
    "ppp",
    "BRMSEA",
    "BGammaHat",
    "adjBGammaHat",
    "BMc",
    "chisq",
    "cfi",
    "rmsea",
    "aic",
    "bic"
  )
  expect_false(any(gone %in% names(fm)))

  # Asking for one of them by name says why nothing came back
  expect_error(
    fitMeasures(fit_rs_dic, "BRMSEA"),
    class = "inlavaan_rs_fitmeasures"
  )
})

test_that("Random slopes: the quantities that do not exist are gated", {
  expect_error(bfit_indices(fit_rs), class = "inlavaan_rs_bfit")
  expect_error(simulate(fit_rs, nsim = 1), class = "inlavaan_rs_simulate")
  expect_error(predict(fit_rs, type = "yhat"), class = "inlavaan_rs_predict")
  expect_error(fitted(fit_rs), class = "inlavaan_rs_moments")
  expect_error(residuals(fit_rs), class = "inlavaan_rs_moments")
  expect_error(loo(fit_rs), class = "inlavaan_rs_loo")
  expect_error(loo(fit_rs, type = "loso"), class = "inlavaan_rs_loso")
})
