# The arguments of inlavaan() and its wrappers, and how update() refits

hs <- lavaan::HolzingerSwineford1939
mod <- "visual =~ x1 + x2 + x3"
fit_quick <- function(..., silent = TRUE) {
  acfa(
    mod,
    ...,
    silent = silent,
    nsamp = 3,
    test = "none",
    marginal_method = "gaussian"
  )
}

## ----- data ------------------------------------------------------------------

test_that("a model fits from sample.cov and sample.nobs alone", {
  for (fn in list(inlavaan, acfa, asem, agrowth)) {
    expect_null(formals(fn)$data)
  }
  S <- stats::cov(hs[, c("x1", "x2", "x3")])
  fit <- fit_quick(sample.cov = S, sample.nobs = 301)
  expect_s4_class(fit, "INLAvaan")
  # lavaan rescales sample.cov to the divisor n of the data
  expect_equal(
    get_inlavaan_internal(fit, "theta_star_novbc"),
    get_inlavaan_internal(fit_quick(hs), "theta_star_novbc"),
    tolerance = 1e-6
  )
})

test_that("a fit with no data stops with an error that says what to give", {
  # Before, the fit stopped late with an error about the identification
  for (fn in list(inlavaan, acfa, asem, agrowth)) {
    err <- expect_error(fn(mod, silent = TRUE), class = "inlavaan_no_data")
    expect_match(conditionMessage(err), "sample.nobs", fixed = TRUE)
  }
  expect_error(
    fit_quick(data = NULL, sample.cov = NULL, sample.nobs = 301),
    class = "inlavaan_no_data"
  )
  # The snake-case names of lavaan count too
  S <- stats::cov(hs[, c("x1", "x2", "x3")])
  expect_s4_class(fit_quick(sample_cov = S, sample_nobs = 301), "INLAvaan")
})

## ----- model.type ------------------------------------------------------------

test_that("model.type must name a lavaan function that inlavaan() uses", {
  for (bad in list("foo", "efa", NA_character_, c("cfa", "sem"), 1)) {
    err <- expect_error(
      inlavaan(mod, hs, model.type = bad, silent = TRUE),
      class = "inlavaan_model_type"
    )
    expect_match(conditionMessage(err), "growth", fixed = TRUE)
  }
  fit <- inlavaan(
    mod,
    hs,
    model.type = "cfa",
    silent = TRUE,
    nsamp = 3,
    test = "none",
    marginal_method = "gaussian"
  )
  expect_identical(fit@Options$model.type, "cfa")
})

## ----- cores -----------------------------------------------------------------

test_that("cores must be NULL or a single positive whole number", {
  # Before, most of these ran, on one core or on the whole part of the number
  for (bad in list(0, 2.5, NA, -3, Inf, "2", TRUE, c(1, 2))) {
    expect_error(
      fit_quick(hs, cores = bad),
      "positive whole number",
      class = "inlavanilla_error"
    )
  }
  fit_one <- fit_quick(hs, cores = 1)
  expect_identical(
    get_inlavaan_internal(fit_one, "theta_star_novbc"),
    get_inlavaan_internal(fit_quick(hs), "theta_star_novbc")
  )
})

## ----- dp --------------------------------------------------------------------

test_that("a name of dp that no parameter type uses warns", {
  w <- expect_warning(
    fit <- fit_quick(
      hs,
      dp = priors_for(lamda = "normal(0,1)"),
      silent = FALSE
    ),
    class = "inlavaan_dp_unused"
  )
  expect_match(conditionMessage(w), "lamda", fixed = TRUE)
  # The fit goes on with the default prior of the loadings
  pt <- get_inlavaan_internal(fit, "partable")
  expect_true(all(pt$prior[pt$op == "=~" & pt$free > 0] == "normal(0,10)"))
  # Every type of priors_for() is used
  expect_no_warning(
    fit_quick(hs, dp = priors_for(lambda = "normal(0,1)"), silent = FALSE),
    class = "inlavaan_dp_unused"
  )
})

test_that("dp = blavaan::dpriors() gives no warning", {
  skip_if_not_installed("blavaan")
  dp <- blavaan::dpriors()
  expect_true("ibpsi" %in% names(dp))
  expect_no_warning(
    fit_quick(hs, dp = dp, silent = FALSE),
    class = "inlavaan_dp_unused"
  )
})
