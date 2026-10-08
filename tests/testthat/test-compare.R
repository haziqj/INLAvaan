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

test_that("compare() names a model given as a long call on one line", {
  cmp <- compare(
    fit1,
    list(a_long_name_that_pushes_the_call_past_the_cutoff = fit2, b = 1)[[1]]
  )
  expect_equal(nrow(cmp), 2)
  expect_true(any(grepl("a_long_name", cmp$Model)))
})

test_that("compare() print runs without error", {
  cmp <- compare(fit1, fit2)
  expect_output(print(cmp), "Bayesian Model Comparison")
  expect_output(print(cmp), "marginal log-likelihood")
})

test_that("compare() with fit.measures appends extra columns", {
  cmp <- compare(fit1, fit2, fit.measures = "margloglik")
  expect_true("margloglik" %in% names(cmp))
  expect_output(print(cmp), "Bayesian Model Comparison")
  expect_no_match(
    paste(capture.output(print(cmp)), collapse = ""),
    "Baseline model"
  )
})

test_that("compare() includes DIC/pD when the fit computed the DIC", {
  skip_on_cran()
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
  skip_on_cran()
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

test_that("compare() warns when fits differ in the VB correction", {
  fit2_novb <- acfa(
    mod_full,
    dat,
    vb_correction = FALSE,
    marginal_method = "marggaus",
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  expect_warning(compare(fit1, fit2_novb), "VB correction")
  # Fits that agree on the setting compare without it, with or without VB
  expect_no_warning(compare(fit1, fit2))
  expect_no_warning(compare(fit1_ms, fit2_ms))
})

test_that("compare() accepts more than two models via ...", {
  skip_on_cran()
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
      vb_correction = FALSE, # as fit1_ms
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    ),
    classes = "inlavaan_diagnostics_warning"
  )
  expect_error(compare(fit1_ms, fit3, loo = TRUE), "same data")
})

test_that("compare(loo = TRUE) aborts when the variable sets differ", {
  skip_on_cran()
  fit9 <- acfa(
    mod_speed,
    dat,
    meanstructure = TRUE,
    vb_correction = FALSE, # as fit1_ms
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  expect_error(compare(fit1_ms, fit9, loo = TRUE), "same set of observed")
})

test_that("compare(loo = TRUE) aborts when conditional outcome sets differ", {
  skip_on_cran()
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
test_that("compare() scales incremental indices against the independence model", {
  fit_a <- acfa(
    mod_null,
    dat,
    verbose = FALSE,
    nsamp = 50,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )
  fit_b <- acfa(
    mod_full,
    dat,
    verbose = FALSE,
    nsamp = 50,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )
  cmp <- compare(fit_a, fit_b, fit.measures = c("BCFI", "BTLI"))
  expect_true(all(is.finite(cmp$BCFI)))
  # The first model is no longer its own baseline, so it does not sit at 0
  expect_gt(cmp$BCFI[cmp$Model == "fit_a"], 0.3)
  expect_true(all(cmp$BCFI <= 1))
})

## ----- Composites ------------------------------------------------------------

# lavaan fixes the (co)variances of composite indicators at their sample values,
# and the marginal likelihood and DIC condition on them
mod_comp <- "
  C <~ x1 + x2 + x3
  x4 ~ C
  x5 ~ C
  x4 ~~ x5
"
fit_args_comp <- list(
  composites.cov = "fixed",
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus"
)
fit_comp <- do.call(asem, c(list(mod_comp, dat), fit_args_comp))

test_that("compare() warns when fits do not fix the same composite blocks", {
  # The same composite on the same data: comparable
  mod_comp0 <- "
    C <~ x1 + x2 + x3
    x4 ~ C
    x5 ~ C
    x4 ~~ 0*x5
  "
  fit_comp0 <- do.call(asem, c(list(mod_comp0, dat), fit_args_comp))
  expect_no_warning(compare(fit_comp, fit_comp0))

  # The same model with the indicator moments estimated is not
  mod_phantom <- "
    C =~ 0
    C ~ 1*x1 + x2 + x3
    C ~~ 0*C
    x4 ~ C
    x5 ~ C
    x4 ~~ x5
  "
  fit_phantom <- do.call(
    asem,
    c(list(mod_phantom, dat, fixed.x = FALSE), fit_args_comp)
  )
  expect_warning(compare(fit_comp, fit_phantom), "same composites")
  expect_warning(
    cmp <- compare(fit_comp, fit_phantom, loo = TRUE),
    "Interpret only the ELPD columns"
  )
  expect_true(all(is.finite(cmp$ELPD)))
})

test_that("compare(loo = TRUE) says why a composite fit is scored jointly", {
  fit_reg <- do.call(
    asem,
    c(list("x4 ~ x1 + x2 + x3\n x5 ~ x1 + x2 + x3", dat), fit_args_comp)
  )
  expect_error(
    suppressWarnings(compare(fit_comp, fit_reg, loo = TRUE)),
    "not fixed covariates"
  )
  # Without composites the usual hint stands
  fit_reg_joint <- do.call(
    asem,
    c(
      list("x4 ~ x1 + x2 + x3\n x5 ~ x1 + x2 + x3", dat, fixed.x = FALSE),
      fit_args_comp
    )
  )
  expect_error(
    compare(fit_reg_joint, fit_reg, loo = TRUE),
    "same `fixed.x` setting"
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

test_that("random-slope fits may differ in their covariates", {
  # Without w1 the outcomes cannot depend on it, so this fit's density of the
  # outcomes given x1 is also its density given x1 and w1. It is the same
  # model as one that keeps w1 with its effects fixed at zero.
  mod_nw <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
  "
  mod_w0 <- paste0(mod_nw, "    fb ~ 0*w1\n    s1 ~ 0*w1\n")
  fit_nw <- fit_twolevel(mod_nw)
  fit_w0 <- fit_twolevel(mod_w0)
  cmp <- compare(fit_nw, fit_w0)
  expect_equal(diff(cmp$Marg.Loglik), 0, tolerance = 1e-6)
  cmp_loo <- compare(fit_nw, fit_w0, loo = TRUE)
  expect_equal(diff(cmp_loo$ELPD), 0, tolerance = 1e-3)

  # Against fits where w1 has effects, random slope or fixed
  expect_no_error(cmp_w <- compare(fit_rs, fit_nw, fit_fx))
  expect_true(all(is.finite(cmp_w$Marg.Loglik)))
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

test_that("a between-level factor is compared on the kernel's own sets", {
  skip_on_cran()
  # `fz =~ w1 + w2` makes w1 and w2 part of what the kernel models, not
  # part of what it conditions on, so this fit is on the same scale as the
  # plain fixed-slope fit that models them the same way
  mod_zb <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fz =~ w1 + w2
      fb ~ fz
      s1 ~ fz
  "
  mod_zb_fx <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fz =~ w1 + w2
      fb ~ fz
  "
  # Three parameters of these small fits trip the marginal-fit diagnostic,
  # which has nothing to do with what is being tested here
  fit_zb <- suppressWarnings(fit_twolevel(mod_zb))
  fit_zb_fx <- suppressWarnings(fit_twolevel(mod_zb_fx))

  spec <- rs_spec(get_inlavaan_internal(fit_zb))
  expect_equal(spec$cond, "x1")
  expect_setequal(spec$resp, c("y1", "y2", "y3", "w1", "w2"))

  expect_no_error(cmp <- compare(fit_zb, fit_zb_fx))
  expect_true(all(is.finite(cmp$Marg.Loglik)))

  # Regressing on w1 and w2 instead conditions on them, so they are no longer
  # scored and the two fits score different variables
  mod_exo <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1 + w2
      s1 ~ w1 + w2
  "
  fit_exo <- suppressWarnings(fit_twolevel(mod_exo))
  expect_setequal(
    rs_spec(get_inlavaan_internal(fit_exo))$cond,
    c("x1", "w1", "w2")
  )
  expect_error(
    compare(fit_zb, fit_exo),
    class = "inlavaan_rs_compare_resp"
  )
})

test_that("quadrature fits are compared on the covariates they condition on", {
  skip_on_cran()
  # The quadrature kernel scores a covariate that lives at both levels with
  # the outcomes, and the fixed.x shift takes its density out again, so
  # these fits condition on their covariates like any fixed.x fit
  set.seed(2)
  J <- 12
  n <- 6
  cl <- rep(seq_len(J), each = n)
  xw <- stats::rnorm(J * n)
  x1 <- stats::rnorm(J)[cl] + xw
  x2 <- stats::rnorm(J)[cl] + stats::rnorm(J * n)
  u0 <- stats::rnorm(J, 0, sqrt(0.5))
  u1 <- stats::rnorm(J, 0, 0.5)
  d_b <- data.frame(
    y1 = 1 + u0[cl] + (0.5 + u1[cl]) * xw + stats::rnorm(J * n),
    x1 = x1,
    x2 = x2,
    cluster = cl
  )
  fit_b <- function(mod) {
    suppressWarnings(
      asem(
        mod,
        d_b,
        cluster = "cluster",
        integration.ngh = 5,
        verbose = FALSE,
        nsamp = 3,
        test = "none",
        marginal_correction = "none",
        vb_correction = FALSE
      )
    )
  }
  fit_b1 <- fit_b(
    "
    level: 1
      y1 ~ rv('s1')*x1
    level: 2
      y1 ~ x1
      y1 ~~ y1
      s1 ~~ s1
  "
  )
  fit_b2 <- fit_b(
    "
    level: 1
      y1 ~ rv('s1')*x1 + rv('s2')*x2
    level: 2
      y1 ~ x1 + x2
      y1 ~~ y1
      s1 ~~ s1
      s2 ~~ s2
  "
  )

  spec1 <- rs_spec(get_inlavaan_internal(fit_b1))
  expect_setequal(spec1$resp, c("y1", "x1"))
  expect_length(spec1$cond, 0L)
  sets1 <- rs_scored_sets(get_inlavaan_internal(fit_b1), spec1)
  expect_equal(sets1$cond, "x1")
  expect_equal(sets1$resp, "y1")

  # A second covariate is covariate selection, as for any fixed.x fit
  expect_no_error(cmp_b <- compare(fit_b1, fit_b2))
  expect_true(all(is.finite(cmp_b$Marg.Loglik)))

  # The fixed-slope model is the comparator, here where a zero slope variance
  # is not available
  fit_bfx <- fit_b(
    "
    level: 1
      y1 ~ x1
    level: 2
      y1 ~ x1
      y1 ~~ y1
  "
  )
  expect_no_error(cmp <- compare(fit_b1, fit_bfx))
  expect_true(all(is.finite(cmp$Marg.Loglik)))
  cmp_loo <- expect_warning(
    compare(fit_b1, fit_bfx, loo = TRUE),
    class = "inlavaan_rs_route_b"
  )
  expect_true(all(is.finite(cmp_loo$ELPD)))
})

test_that("a zero slope variance matches the fixed slope with a split covariate", {
  skip_on_cran()
  # x2 varies at both levels and carries no slope. lavaan's kernel scores it
  # with the outcomes, and the evidence and the LOO both take its density
  # out (about 423 here), so the comparison with the fixed-slope fit is on
  # one scale. The two kernels treat x2 slightly differently, by 0.11 at the
  # same parameter values on this subset, which bounds what remains.
  mod_split0 <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1 + x2
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ x2 + w1
      s1 ~~ 0*s1
  "
  mod_split_fx <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1 + x2
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ x2 + w1
  "
  fit_split0 <- suppressWarnings(fit_twolevel(mod_split0))
  fit_split_fx <- suppressWarnings(fit_twolevel(mod_split_fx))
  cmp <- compare(fit_split0, fit_split_fx)
  expect_lt(abs(diff(cmp$Marg.Loglik)), 0.25)
  cmp_loo <- compare(fit_split0, fit_split_fx, loo = TRUE)
  expect_lt(abs(diff(cmp_loo$ELPD)), 0.25)
})
