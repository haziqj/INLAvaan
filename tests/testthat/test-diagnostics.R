dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
fit <- acfa(
  mod, dat,
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
    "npar", "nsamp", "converged", "iterations",
    "grad_inf", "grad_inf_rel", "grad_l2", "hess_cond",
    "vb_applied", "vb_kld_global",
    "kld_max", "kld_mean", "nmad_max", "nmad_mean"
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
  expect_true(all(c("names", "grad", "grad_num", "grad_diff") %in% names(dp)))
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
