mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
dat <- lavaan::HolzingerSwineford1939

fit_fast <- function(...) {
  # Covariances are summarised from the posterior draws
  set.seed(1)
  acfa(
    mod,
    dat,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none",
    ...
  )
}

test_that("likelihood = 'wishart' is ignored, with a message", {
  fit_normal <- fit_fast()
  expect_message(
    fit_wishart <- fit_fast(likelihood = "wishart"),
    "always uses the normal likelihood"
  )
  expect_identical(fit_wishart@Options$likelihood, "normal")
  expect_equal(coef(fit_wishart), coef(fit_normal))
})

test_that("mimic = 'EQS' does not switch to the Wishart likelihood", {
  fit_normal <- fit_fast()
  expect_message(
    fit_eqs <- fit_fast(mimic = "EQS"),
    "always uses the normal likelihood"
  )
  expect_identical(fit_eqs@Options$likelihood, "normal")
  expect_equal(coef(fit_eqs), coef(fit_normal))
})

test_that("the normal likelihood gives no message", {
  expect_no_message(
    fit_fast(likelihood = "normal"),
    message = "normal likelihood"
  )
})
