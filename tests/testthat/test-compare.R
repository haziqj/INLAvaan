# Small deterministic subset of HolzingerSwineford1939 shared by every fit in
# this file, in place of the full 301-row dataset
set.seed(1)
dat <- lavaan::HolzingerSwineford1939[
  sample(nrow(lavaan::HolzingerSwineford1939), 40),
]

mod_null <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
mod_full <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  visual ~~ textual
"
mod_speed <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

# Shared by every test below that just needs *some* comparable pair of
# no-mean-structure, test = "none" fits (historically each re-fit these
# identically)
fit1 <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3, test = "none")
fit2 <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3, test = "none")

# Same, but with a mean structure (needed for loo = TRUE comparisons); shared
# across every loo-comparison test that doesn't need its own bespoke fit
fit1_ms <- acfa(
  mod_null,
  dat,
  meanstructure = TRUE,
  marginal_method = "marggaus",
  vb_correction = FALSE,
  verbose = FALSE,
  nsamp = 3,
  test = "none"
)
fit2_ms <- acfa(
  mod_full,
  dat,
  meanstructure = TRUE,
  marginal_method = "marggaus",
  vb_correction = FALSE,
  verbose = FALSE,
  nsamp = 3,
  test = "none"
)

test_that("compare() returns compare.inlavaan_internal data.frame", {
  cmp <- compare(fit1, fit2)
  expect_s3_class(cmp, "compare.inlavaan_internal")
  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 2)
  expect_true("npar" %in% names(cmp))
  expect_true("Marg.Loglik" %in% names(cmp))
  expect_true("logBF" %in% names(cmp))
  # sorted by descending marginal log-likelihood; the best model has logBF 0
  expect_equal(cmp$Marg.Loglik, sort(cmp$Marg.Loglik, decreasing = TRUE))
  expect_equal(cmp$logBF[1], 0)
})

test_that("compare() print runs without error", {
  cmp <- compare(fit1, fit2)
  expect_output(print(cmp), "Bayesian Model Comparison")
  expect_output(print(cmp), "marginal log-likelihood")
})

test_that("compare() with fit.measures appends extra columns", {
  cmp <- compare(fit1, fit2, fit.measures = "margloglik")
  expect_true("margloglik" %in% names(cmp))
  expect_output(print(cmp), "Baseline model")
})

test_that("compare() includes DIC/pD when the fit computed the DIC", {
  # the default test = "standard" computes the DIC but no fit-time LOO/WAIC,
  # so nothing here warns
  fit1_std <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3)
  fit2_std <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3)
  cmp <- compare(fit1_std, fit2_std)
  expect_true("DIC" %in% names(cmp))
  expect_true("pD" %in% names(cmp))
})

test_that("compare.inlavaan_internal S3 method works", {
  int1 <- INLAvaan:::get_inlavaan_internal(fit1)
  int2 <- INLAvaan:::get_inlavaan_internal(fit2)
  cmp <- INLAvaan:::compare.inlavaan_internal(int1, int2)
  expect_s3_class(cmp, "compare.inlavaan_internal")
  expect_equal(nrow(cmp), 2)
})

test_that("compare() warns when mean-structure treatments differ", {
  fit_ms <- acfa(
    mod_null,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  fit_nms <- acfa(
    mod_null,
    dat,
    meanstructure = FALSE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  expect_warning(compare(fit_ms, fit_nms), "mean structure")
  # ... but the same comparison under loo = TRUE is unaffected (leave-one-out
  # conditionals are proper under both treatments)
  expect_warning(
    cmp <- compare(fit_ms, fit_nms, loo = TRUE),
    "Interpret only the ELPD columns"
  )
  expect_true(all(is.finite(cmp$ELPD)))
})

test_that("compare() accepts more than two models via ...", {
  fit_speed <- acfa(mod_speed, dat, verbose = FALSE, nsamp = 3, test = "none")
  cmp <- compare(fit1, fit2, fit_speed)
  expect_equal(nrow(cmp), 3)
  expect_setequal(cmp$Model, c("fit1", "fit2", "fit_speed"))
})

test_that("compare(loo = TRUE) appends ELPD columns with paired SEs", {
  cmp <- compare(fit1_ms, fit2_ms, loo = TRUE)
  expect_true(all(
    c("ELPD", "SE", "p_loo", "elpd_diff", "se_diff") %in% names(cmp)
  ))
  # Sorted by descending ELPD; the best model has zero differences
  expect_equal(cmp$ELPD, sort(cmp$ELPD, decreasing = TRUE))
  expect_equal(cmp$elpd_diff[1], 0)
  expect_equal(cmp$se_diff[1], 0)
  expect_true(all(cmp$elpd_diff <= 0))
  expect_true(all(is.finite(cmp$se_diff)) && all(cmp$se_diff >= 0))
  # ELPD agrees with loo() on each fit
  expect_equal(
    sort(cmp$ELPD, decreasing = TRUE),
    sort(
      c(
        unname(loo(fit1_ms)$estimates["elpd_loo", "Estimate"]),
        unname(loo(fit2_ms)$estimates["elpd_loo", "Estimate"])
      ),
      decreasing = TRUE
    ),
    tolerance = 1e-3
  )
  expect_output(print(cmp), "paired differences")

  # Stored LOO results are reused; add_loo() also stores the WAIC (same
  # Taylor pass), which warns on this fixture (a unit with no second-order
  # lpd)
  cmp2 <- compare(
    suppressWarnings(add_loo(fit1_ms)),
    suppressWarnings(add_loo(fit2_ms)),
    loo = TRUE
  )
  expect_equal(cmp2$ELPD, cmp$ELPD)
})

test_that("compare(loo = TRUE) scores every model at one common order", {
  # Both models clean: second order throughout
  cmp2 <- compare(fit1_ms, fit2_ms, loo = TRUE)
  expect_equal(attr(cmp2, "loo_order"), 2L)
  expect_output(print(cmp2), "second-order")

  # A doctored LOO stored on one model only: inflating Omega drives some of
  # its units past k = 1, so it has no second-order total while its rival
  # still does. compare() reuses stored results, so this reaches the table.
  S <- get_inlavaan_internal(fit1_ms)$Sigma_theta
  bad <- suppressWarnings(loo(fit1_ms, Omega = S * 4, cores = 1L))
  expect_false(bad$use_second)
  fit1_bad <- fit1_ms
  fit1_bad@external$inlavaan_internal$loo <- bad

  good <- loo(fit2_ms)
  expect_true(good$use_second)

  # The clean model comes down to first order too, rather than meeting a
  # first-order rival at second order
  cmp <- compare(fit1_bad, fit2_ms, loo = TRUE)
  expect_equal(attr(cmp, "loo_order"), 1L)
  expect_true(any(abs(cmp$ELPD - good$elpd_1) < 1e-3))
  expect_false(any(abs(cmp$ELPD - good$elpd_2) < 1e-3))
  expect_output(print(cmp), "first-order")
  expect_output(print(cmp), "no second-order term")
})

test_that("compare(loo = TRUE) aborts for models on different data", {
  # Twenty rows are too few to keep the skew-normal tails inside the scanned
  # window, so the fit-time endpoint-mass check fires. That is the expected
  # small-sample behaviour of the diagnostic, not a fault in this fixture.
  fit3 <- suppressWarnings(
    acfa(
      mod_null,
      dat[1:20, ],
      meanstructure = TRUE,
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    ),
    classes = "inlavaan_diagnostics_warning"
  )
  expect_error(compare(fit1_ms, fit3, loo = TRUE), "same data")
})

test_that("compare(loo = TRUE) aborts when the variable sets differ", {
  fit9 <- acfa(
    mod_speed,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  expect_error(compare(fit1_ms, fit9, loo = TRUE), "same set of observed")
})

test_that("compare(loo = TRUE) aborts when conditional outcome sets differ", {
  # Both fixed.x = TRUE (conditional flavour), but the outcome variable sets
  # differ (covariate sets may differ under conditional scoring, but outcomes
  # must match) -- distinct from the joint-flavour "same set of observed
  # variables" case above
  modA <- "
    visual =~ x1 + x2 + x3
    visual ~ ageyr
  "
  modB <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    visual ~ ageyr
  "
  suppressWarnings(
    fitA <- asem(
      modA,
      dat,
      fixed.x = TRUE,
      meanstructure = TRUE,
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )
  suppressWarnings(
    fitB <- asem(
      modB,
      dat,
      fixed.x = TRUE,
      meanstructure = TRUE,
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )
  expect_error(
    suppressWarnings(compare(fitA, fitB, loo = TRUE)),
    "outcome variables"
  )
})

## ----- Random slopes ---------------------------------------------------------
# A random-slope likelihood is conditional on the exogenous covariates, so the
# table only means anything across fits that condition on the same ones. The
# fixtures are the 24-cluster subset of lavaan::Demo.twolevel used throughout
# the random-slope tests, and `test = "dic"` is what fills the DIC/pD columns.
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
# The nested no-random-slope fit: one constraint away from mod_rs, and it
# keeps the conditioning set whole
mod_rs0 <- paste0(mod_rs, "    s1 ~~ 0*s1\n")
# The exact fixed-slope comparator: with no level-2 regression on the slope
# either, s1 is a single constant shared by every cluster
mod_rs0e <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~~ 0*s1
"
mod_fx <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
"
fit_twolevel <- function(mod, ...) {
  asem(
    mod,
    d_rs,
    cluster = "cluster",
    verbose = FALSE,
    nsamp = 3,
    test = "dic",
    marginal_correction = "none",
    vb_correction = FALSE,
    ...
  )
}
fit_rs <- fit_twolevel(mod_rs)
fit_rs0 <- fit_twolevel(mod_rs0)
fit_fx <- fit_twolevel(mod_fx)

test_that("random-slope fits are compared on marginal likelihood and DIC", {
  cmp <- compare(fit_rs, fit_rs0, fit_fx)
  expect_named(cmp, c("Model", "npar", "Marg.Loglik", "logBF", "DIC", "pD"))
  expect_false(any(c("AIC", "BIC") %in% names(cmp)))
  # 19 parameters with the random slope, one fewer once its variance is fixed
  # at zero, and one fewer again without the level-2 regression on it
  expect_equal(
    cmp$npar[match(c("fit_rs", "fit_rs0", "fit_fx"), cmp$Model)],
    c(19L, 18L, 17L)
  )
  expect_true(all(is.finite(cmp$Marg.Loglik)))
  expect_true(all(is.finite(cmp$DIC)))
})

test_that("a zero-variance slope is on the same scale as a fixed slope", {
  # Fixing the slope variance at zero *and* dropping its level-2 regression
  # leaves exactly the fixed-slope model, so the two marginal likelihoods are
  # one number computed two ways -- the regression test that the random-slope
  # kernel and the ordinary two-level one live on a single scale
  fit_rs0e <- fit_twolevel(mod_rs0e)
  cmp <- compare(fit_rs0e, fit_fx)
  expect_equal(cmp$npar, c(17L, 17L))
  expect_equal(diff(cmp$Marg.Loglik), 0, tolerance = 1e-3)
})

test_that("comparing a random-slope fit with a fixed.x = FALSE fit aborts", {
  fit_fx_free <- fit_twolevel(mod_fx, fixed.x = FALSE)
  expect_error(
    compare(fit_rs, fit_fx_free),
    class = "inlavaan_rs_compare_fixedx"
  )
})

test_that("comparing across conditioning sets aborts", {
  # Without `fb ~ w1` this fit conditions on x1 alone, so its marginal
  # log-likelihood is a density of different things
  mod_nw <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
  "
  fit_nw <- fit_twolevel(mod_nw)
  err <- expect_error(
    compare(fit_rs, fit_nw),
    class = "inlavaan_rs_compare_cond"
  )
  expect_match(conditionMessage(err), "w1")
  expect_match(conditionMessage(err), "s1 ~~ 0\\*s1")
})

test_that("compare(loo = TRUE) works across random-slope fits", {
  cmp <- compare(fit_rs, fit_rs0, loo = TRUE)
  expect_true(all(
    c("ELPD", "SE", "p_loo", "elpd_diff", "se_diff") %in% names(cmp)
  ))
  expect_true(all(is.finite(cmp$ELPD)))
  expect_equal(attr(cmp, "loo_n_models"), 2L)
  expect_equal(sum(cmp$elpd_diff == 0), 1L)
})
