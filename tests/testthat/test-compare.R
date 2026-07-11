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

test_that("compare() includes DIC/pD when test != 'none'", {
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

  # Stored LOO results are reused
  cmp2 <- compare(add_loo(fit1_ms), add_loo(fit2_ms), loo = TRUE)
  expect_equal(cmp2$ELPD, cmp$ELPD)
})

test_that("compare(loo = TRUE) aborts for models on different data", {
  fit3 <- acfa(
    mod_null,
    dat[1:20, ],
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
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
  suppressWarnings(fitA <- asem(
    modA,
    dat,
    fixed.x = TRUE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  ))
  suppressWarnings(fitB <- asem(
    modB,
    dat,
    fixed.x = TRUE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  ))
  expect_error(
    suppressWarnings(compare(fitA, fitB, loo = TRUE)),
    "outcome variables"
  )
})
