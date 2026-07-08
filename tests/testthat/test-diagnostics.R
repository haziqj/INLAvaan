dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
fit <- acfa(
  mod,
  dat,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus"
)

# ---- diagnostics() ----

test_that("diagnostics(type = 'global') returns named numeric vector", {
  dg <- diagnostics(fit)
  expect_s3_class(dg, "diagnostics.INLAvaan")
  expect_type(dg, "double")
  expected_names <- c(
    "npar",
    "nsamp",
    "converged",
    "iterations",
    "grad_inf",
    "grad_inf_rel",
    "grad_l2",
    "mode_shift_max",
    "hess_cond",
    "vb_applied",
    "vb_kld_global",
    "kld_max",
    "kld_mean",
    "nmad_max",
    "nmad_mean"
  )
  expect_named(dg, expected_names)
  expect_equal(unname(dg["npar"]), length(coef(fit)))
  expect_equal(unname(dg["converged"]), 1)
})

test_that("diagnostics(type = 'param') returns data frame", {
  dp <- diagnostics(fit, type = "param")
  expect_s3_class(dp, "diagnostics.INLAvaan.param")
  expect_s3_class(dp, "data.frame")
  expect_equal(nrow(dp), length(coef(fit)))
  expect_true(all(
    c("names", "grad", "grad_num", "grad_diff", "mode_shift_sigma") %in%
      names(dp)
  ))
})

# ---- fit-time diagnostics warnings ----

test_that("a healthy fit passes warn_fit_diagnostics() silently", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  expect_no_warning(INLAvaan:::warn_fit_diagnostics(int))
})

test_that("an unconverged optimiser triggers the fit-time warning", {
  w <- expect_warning(
    acfa(
      mod,
      dat,
      verbose = FALSE,
      nsamp = 3,
      test = "none",
      vb_correction = FALSE,
      marginal_method = "marggaus",
      std.lv = TRUE, # keeps the Hessian PD at the early stopping point
      control = list(iter.max = 2)
    ),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "did not converge")
  expect_match(conditionMessage(w), "posterior mode is not zero")
})

test_that("warn_fit_diagnostics() flags high NMAD marginals by name", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  m <- length(coef(fit))
  int$approx_data <- matrix(
    0,
    nrow = m,
    ncol = 1,
    dimnames = list(names(coef(fit)), "nmad")
  )
  int$approx_data[1:4, "nmad"] <- c(0.5, 0.3, 0.2, 0.15)
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "NMAD")
  expect_match(conditionMessage(w), names(coef(fit))[1], fixed = TRUE)
  expect_match(conditionMessage(w), "1 other") # top 3 shown, 1 elided
})

test_that("warn_fit_diagnostics() flags large VB shifts", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  m <- length(coef(fit))
  se <- sqrt(diag(int$Sigma_theta))
  int$vb$correction <- 2 * se
  int$vb$kld <- rep(2, m)
  int$vb$kld_global <- 1
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "VB correction")
})

test_that("warn_fit_diagnostics() flags a near-singular Hessian", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  m <- length(coef(fit))
  # zero out the gradient so only the condition-number check fires
  int$opt$dx_analytic <- int$opt$dx <- rep(0, m)
  int$Sigma_theta[1, 1] <- 1e12
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "near-singular")
})

test_that("print.diagnostics.INLAvaan works", {
  dg <- diagnostics(fit)
  expect_output(print(dg))
})

test_that("print.diagnostics.INLAvaan.param works", {
  dp <- diagnostics(fit, type = "param")
  expect_output(print(dp))
})

# ---- timing() ----

test_that("timing(what = 'total') returns total time", {
  tt <- timing(fit)
  expect_s3_class(tt, "timing.INLAvaan")
  expect_type(tt, "double")
  expect_named(tt, "total")
  expect_true(tt > 0)
})

test_that("timing(what = 'all') returns all segments", {
  tt <- timing(fit, what = "all")
  expect_s3_class(tt, "timing.INLAvaan")
  expect_true(length(tt) > 1)
  expect_true("total" %in% names(tt))
})

test_that("timing(what = ...) selects specific segments", {
  tt <- timing(fit, what = c("optim", "total"))
  expect_named(tt, c("optim", "total"))
})

test_that("timing rejects unknown segments", {
  expect_error(timing(fit, what = "nonexistent"), "Unknown")
})

test_that("print.timing.INLAvaan works", {
  tt <- timing(fit, what = "all")
  expect_output(print(tt))
})
