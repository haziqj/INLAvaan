mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
dat <- lavaan::HolzingerSwineford1939

fit_with <- function(test, ...) {
  acfa(
    mod,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = test,
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none",
    ...
  )
}

rec <- function(fit) get_inlavaan_internal(fit, "test")

test_that("resolve_test() maps the five table rows", {
  expect_identical(INLAvaan:::resolve_test("none"), character(0))
  expect_identical(INLAvaan:::resolve_test("standard"), c("ppp", "dic"))
  expect_identical(INLAvaan:::resolve_test("default"), c("ppp", "dic"))
  expect_identical(
    INLAvaan:::resolve_test("full"),
    c("ppp", "dic", "loo", "waic")
  )
  expect_identical(INLAvaan:::resolve_test(c("dic", "loo")), c("dic", "loo"))
})

test_that("aliases and atoms are unioned without duplicates", {
  expect_identical(
    INLAvaan:::resolve_test(c("standard", "loo")),
    c("ppp", "dic", "loo")
  )
  expect_identical(
    INLAvaan:::resolve_test(c("loo", "loo", "standard")),
    c("ppp", "dic", "loo")
  )
  expect_identical(INLAvaan:::resolve_test(c("none", "dic")), "dic")
})

test_that("unknown or malformed values in test error, listing valid ones", {
  expect_error(INLAvaan:::resolve_test("noloo"), "Unknown value")
  expect_error(INLAvaan:::resolve_test("noloo"), "standard")
  expect_error(INLAvaan:::resolve_test("browne.residual.adf"), "Unknown value")
  expect_error(INLAvaan:::resolve_test(TRUE))
  expect_error(INLAvaan:::resolve_test(NULL))
  expect_error(INLAvaan:::resolve_test(character(0)))
})

test_that("test_record() falls back to object presence for older fits", {
  legacy_some <- list(ppp = 0.5, DIC = list(dic = 1))
  expect_identical(
    INLAvaan:::test_record(legacy_some)$computed,
    c("ppp", "dic")
  )
  legacy_none <- list()
  expect_identical(INLAvaan:::test_record(legacy_none)$computed, character(0))
  current <- list(
    test = list(
      requested = character(0),
      computed = character(0),
      skipped = character(0)
    )
  )
  expect_identical(INLAvaan:::test_record(current)$computed, character(0))
})

test_that("test = 'none' computes nothing and the record says so", {
  fit <- fit_with("none")
  expect_identical(rec(fit)$requested, character(0))
  expect_identical(rec(fit)$computed, character(0))
  expect_null(get_inlavaan_internal(fit, "ppp"))
  expect_null(get_inlavaan_internal(fit, "DIC"))
  expect_null(get_inlavaan_internal(fit, "loo"))
  expect_null(get_inlavaan_internal(fit, "waic"))
  expect_false(grepl("PPP", paste(capture.output(show(fit)), collapse = "\n")))
  expect_false(any(
    c("ppp", "dic", "p_dic") %in% names(fitMeasures(fit))
  ))
  expect_error(deviance(fit), "DIC")
  expect_error(timing(fit, what = "loo"), "not computed", fixed = TRUE)
  expect_identical(fit@Options$test, "none")
})

test_that("the default and 'standard' compute PPP and DIC only", {
  fit_default <- acfa(
    mod,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  fit_std <- fit_with("standard")
  for (fit in list(fit_default, fit_std)) {
    expect_identical(rec(fit)$computed, c("ppp", "dic"))
    expect_null(get_inlavaan_internal(fit, "loo"))
    expect_null(get_inlavaan_internal(fit, "waic"))
    fm <- fitMeasures(fit)
    expect_false(any(c("elpd_loo", "waic") %in% names(fm)))
    expect_true(all(c("ppp", "dic", "p_dic") %in% names(fm)))
    expect_s3_class(loo(fit), "inlavaan_loo")
    expect_identical(fit@Options$test, "standard")
  }
})

test_that("test = 'full' stores all four quantities", {
  fit <- suppressWarnings(fit_with("full"))
  expect_identical(rec(fit)$requested, c("ppp", "dic", "loo", "waic"))
  expect_identical(rec(fit)$computed, c("ppp", "dic", "loo", "waic"))
  expect_identical(rec(fit)$skipped, character(0))
  expect_identical(loo(fit), get_inlavaan_internal(fit, "loo"))
  expect_identical(waic(fit), get_inlavaan_internal(fit, "waic"))
  expect_named(timing(fit, what = c("loo", "waic")), c("loo", "waic"))
})

test_that("test = 'loo' stores the WAIC too (bug fix)", {
  fit <- suppressWarnings(fit_with("loo"))
  expect_identical(rec(fit)$requested, "loo")
  expect_identical(rec(fit)$computed, c("loo", "waic"))
  expect_null(get_inlavaan_internal(fit, "ppp"))
  expect_null(get_inlavaan_internal(fit, "DIC"))
  fm <- fitMeasures(fit)
  expect_true("waic" %in% names(fm))
  expect_false(any(c("ppp", "dic") %in% names(fm)))
})

test_that("test = 'waic' alone also stores both", {
  fit <- suppressWarnings(fit_with("waic"))
  expect_identical(rec(fit)$requested, "waic")
  expect_identical(rec(fit)$computed, c("loo", "waic"))
  expect_s3_class(get_inlavaan_internal(fit, "loo"), "inlavaan_loo")
})

test_that("test = 'dic' alone reports the DIC everywhere and no PPP", {
  fit <- fit_with("dic")
  expect_identical(rec(fit)$computed, "dic")
  expect_null(get_inlavaan_internal(fit, "ppp"))
  summ_out <- paste(
    capture.output(summary(fit, estimates = FALSE)),
    collapse = "\n"
  )
  expect_true(grepl("Deviance (DIC)", summ_out, fixed = TRUE))
  show_out <- paste(capture.output(show(fit)), collapse = "\n")
  expect_false(grepl("PPP", show_out))
  fm <- fitMeasures(fit)
  expect_true(all(c("dic", "p_dic", "BRMSEA") %in% names(fm)))
  expect_false("ppp" %in% names(fm))
  expect_no_error(deviance(fit))
  expect_no_error(logLik(fit, type = "plugin"))
  expect_no_error(bfit_indices(fit, rescale = "devM"))
  expect_length(fit@Fit@test$ppp, 0)
})

test_that("test = 'ppp' alone reports the PPP and no DIC", {
  fit <- fit_with("ppp")
  expect_identical(rec(fit)$computed, "ppp")
  expect_null(get_inlavaan_internal(fit, "DIC"))
  show_out <- paste(capture.output(show(fit)), collapse = "\n")
  expect_true(grepl("PPP", show_out))
  summ_out <- paste(
    capture.output(summary(fit, estimates = FALSE)),
    collapse = "\n"
  )
  expect_false(grepl("Information Criteria", summ_out, fixed = TRUE))
  fm <- fitMeasures(fit)
  expect_true("ppp" %in% names(fm))
  expect_false(any(c("dic", "p_dic") %in% names(fm)))
  expect_error(deviance(fit), "DIC")
  expect_error(logLik(fit, type = "plugin"), "DIC")
  expect_error(bfit_indices(fit, rescale = "devM"), "DIC not available")
})

test_that("an unsupported model warns and records the skipped quantities", {
  mod_cx <- "visual =~ x1 + x2 + x3\nvisual ~ ageyr"
  fit_cx <- function() {
    asem(
      mod_cx,
      dat,
      conditional.x = TRUE,
      fixed.x = TRUE,
      meanstructure = TRUE,
      test = "loo",
      verbose = FALSE,
      nsamp = 3,
      vb_correction = FALSE,
      marginal_method = "marggaus",
      marginal_correction = "none"
    )
  }
  expect_warning(fit_cx(), "Skipping")
  fit <- suppressWarnings(fit_cx())
  expect_identical(rec(fit)$requested, "loo")
  expect_identical(rec(fit)$computed, character(0))
  expect_named(rec(fit)$skipped, c("loo", "waic"))
  expect_match(rec(fit)$skipped[["loo"]], "conditional.x")
  expect_null(get_inlavaan_internal(fit, "loo"))
  expect_null(get_inlavaan_internal(fit, "waic"))
  err <- tryCatch(
    timing(fit, what = "loo"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "not computed")
  expect_match(conditionMessage(err), "requested")
  expect_false("waic" %in% names(fitMeasures(fit)))
  expect_s4_class(fit, "INLAvaan")
})

test_that("the wrappers pass test through unchanged", {
  expect_identical(formals(acfa)$test, formals(inlavaan)$test)
  expect_identical(formals(asem)$test, formals(inlavaan)$test)
  expect_identical(formals(agrowth)$test, formals(inlavaan)$test)

  fit_a <- fit_with("dic")
  expect_identical(rec(fit_a)$computed, "dic")

  fit_s <- asem(
    mod,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "dic",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  expect_identical(rec(fit_s)$computed, "dic")

  growth_mod <- "
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  "
  fit_g <- agrowth(
    growth_mod,
    lavaan::Demo.growth,
    verbose = FALSE,
    nsamp = 3,
    test = "dic",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  expect_identical(get_inlavaan_internal(fit_g, "test")$computed, "dic")
})

test_that("add_loo() updates the record and stores the WAIC too", {
  fit <- fit_with("standard")
  fit2 <- suppressWarnings(add_loo(fit))
  expect_true(all(c("loo", "waic") %in% rec(fit2)$computed))
  expect_false(any(c("loo", "waic") %in% rec(fit)$computed))
  expect_identical(waic(fit2), get_inlavaan_internal(fit2, "waic"))
})
