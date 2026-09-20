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
