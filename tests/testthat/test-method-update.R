dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("update() records the call and returns an INLAvaan fit", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  expect_false(is.null(get_inlavaan_internal(fit, "call")))

  fit2 <- update(fit, verbose = FALSE)
  expect_s4_class(fit2, "INLAvaan")
  # A no-op update reproduces the (deterministic) posterior mode
  expect_equal(
    get_inlavaan_internal(fit2, "theta_star_novbc"),
    get_inlavaan_internal(fit, "theta_star_novbc"),
    tolerance = 1e-3
  )
})

test_that("update() overrides arguments and preserves expressions", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")

  cl <- update(fit, dp = priors_for(lambda = "normal(0,1)"), evaluate = FALSE)
  expect_true(is.call(cl))
  # The dot argument is kept as its literal expression, not a `..1` placeholder
  expect_equal(cl$dp, quote(priors_for(lambda = "normal(0,1)")))
  # warm-start `start` never leaks into the returned call
  expect_false("start" %in% names(as.list(cl)))
})

test_that("update() applies a changed prior", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  fit_tight <- update(
    fit,
    dp = priors_for(lambda = "normal(0,0.3)"),
    verbose = FALSE
  )
  # A tight loading prior shrinks loadings towards zero
  expect_lt(coef(fit_tight)["visual=~x2"], coef(fit)["visual=~x2"])
})

test_that("update(add=) extends the model structure", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  # nsamp = 3 can trip the marginal-fit diagnostic; irrelevant to structure here
  fit_add <- suppressWarnings(update(fit, add = "x1 ~~ x2", verbose = FALSE))
  expect_s4_class(fit_add, "INLAvaan")
  expect_equal(length(coef(fit_add)), length(coef(fit)) + 1L)
})

test_that("warm start reaches the same posterior mode as a cold fit", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  dp2 <- priors_for(lambda = "normal(0,0.3)")
  fit_warm <- update(fit, dp = dp2, verbose = FALSE)
  fit_cold <- acfa(mod, dat, dp = dp2, verbose = FALSE, nsamp = 3, test = "none")
  # Compare the deterministic optimiser target (the mode), not sample-based
  # summaries which carry Monte Carlo noise at nsamp = 3
  expect_equal(
    get_inlavaan_internal(fit_warm, "theta_star_novbc"),
    get_inlavaan_internal(fit_cold, "theta_star_novbc"),
    tolerance = 1e-3
  )
})

test_that("update() errors on a call-less fit", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  fit@external$inlavaan_internal$call <- NULL
  expect_error(update(fit), "did not record its call")
})

test_that("start of wrong length is rejected", {
  expect_error(
    acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none", start = 1:3),
    "free parameter"
  )
})
