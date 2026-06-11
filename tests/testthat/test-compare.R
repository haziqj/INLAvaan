dat <- lavaan::HolzingerSwineford1939
mod_null <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
mod_full <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  visual ~~ textual
"

test_that("compare() returns compare.inlavaan_internal data.frame", {
  fit1 <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3, test = "none")
  fit2 <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3, test = "none")
  cmp <- compare(fit1, fit2)
  expect_s3_class(cmp, "compare.inlavaan_internal")
  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 2)
  expect_true("npar" %in% names(cmp))
  expect_true("Marg.Loglik" %in% names(cmp))
  expect_true("logBF" %in% names(cmp))
})

test_that("compare() print runs without error", {
  fit1 <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3, test = "none")
  fit2 <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3, test = "none")
  cmp <- compare(fit1, fit2)
  expect_output(print(cmp), "Bayesian Model Comparison")
})

test_that("compare() with fit.measures appends extra columns", {
  fit1 <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3, test = "none")
  fit2 <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3, test = "none")
  cmp <- compare(fit1, fit2, fit.measures = "margloglik")
  expect_true("margloglik" %in% names(cmp))
  expect_output(print(cmp), "Baseline model")
})

test_that("compare() includes DIC/pD when test != 'none'", {
  fit1 <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3)
  fit2 <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3)
  cmp <- compare(fit1, fit2)
  expect_true("DIC" %in% names(cmp))
  expect_true("pD" %in% names(cmp))
})

test_that("compare.inlavaan_internal S3 method works", {
  fit1 <- acfa(mod_null, dat, verbose = FALSE, nsamp = 3, test = "none")
  fit2 <- acfa(mod_full, dat, verbose = FALSE, nsamp = 3, test = "none")
  int1 <- INLAvaan:::get_inlavaan_internal(fit1)
  int2 <- INLAvaan:::get_inlavaan_internal(fit2)
  cmp <- INLAvaan:::compare.inlavaan_internal(int1, int2)
  expect_s3_class(cmp, "compare.inlavaan_internal")
  expect_equal(nrow(cmp), 2)
})

test_that("compare(loo = TRUE) appends ELPD columns with paired SEs", {
  fit1 <- acfa(
    mod_null,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  fit2 <- acfa(
    mod_full,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  cmp <- compare(fit1, fit2, loo = TRUE)
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
        unname(loo(fit1)$estimates["elpd_loo", "Estimate"]),
        unname(loo(fit2)$estimates["elpd_loo", "Estimate"])
      ),
      decreasing = TRUE
    ),
    tolerance = 1e-3
  )
  expect_output(print(cmp), "paired differences")

  # Stored LOO results are reused
  cmp2 <- compare(add_loo(fit1), add_loo(fit2), loo = TRUE)
  expect_equal(cmp2$ELPD, cmp$ELPD)
})

test_that("compare(loo = TRUE) aborts for models on different data", {
  fit1 <- acfa(
    mod_null,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  fit3 <- acfa(
    mod_null,
    dat[1:150, ],
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  expect_error(compare(fit1, fit3, loo = TRUE), "same data")
})
