dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

# A refit must give the fit of a fresh call
expect_same_fit <- function(fit, expected) {
  expect_identical(names(coef(fit)), names(coef(expected)))
  for (field in c("theta_star_novbc", "mloglik")) {
    expect_equal(
      get_inlavaan_internal(fit, field),
      get_inlavaan_internal(expected, field),
      tolerance = 1e-6
    )
  }
}

test_that("update() records the call and returns an INLAvaan fit", {
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  expect_false(is.null(get_inlavaan_internal(fit, "call")))

  fit2 <- update(fit, silent = TRUE)
  expect_s4_class(fit2, "INLAvaan")
  # A no-op update reproduces the (deterministic) posterior mode
  expect_equal(
    get_inlavaan_internal(fit2, "theta_star_novbc"),
    get_inlavaan_internal(fit, "theta_star_novbc"),
    tolerance = 1e-3
  )
})

test_that("update() overrides arguments and preserves expressions", {
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")

  cl <- update(fit, dp = priors_for(lambda = "normal(0,1)"), evaluate = FALSE)
  expect_true(is.call(cl))
  # The dot argument is kept as its literal expression, not a `..1` placeholder
  expect_equal(cl$dp, quote(priors_for(lambda = "normal(0,1)")))
  # warm-start `start` never leaks into the returned call
  expect_false("start" %in% names(as.list(cl)))
})

test_that("update() applies a changed prior", {
  skip_on_cran()
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  fit_tight <- update(
    fit,
    dp = priors_for(lambda = "normal(0,0.3)"),
    silent = TRUE
  )
  # A tight loading prior shrinks loadings towards zero
  expect_lt(coef(fit_tight)["visual=~x2"], coef(fit)["visual=~x2"])
})

test_that("update(add=) extends the model structure", {
  skip_on_cran()
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  # nsamp = 3 can trip the marginal-fit diagnostic; irrelevant to structure here
  fit_add <- suppressWarnings(update(fit, add = "x1 ~~ x2", silent = TRUE))
  expect_s4_class(fit_add, "INLAvaan")
  expect_equal(length(coef(fit_add)), length(coef(fit)) + 1L)
})

test_that("warm start reaches the same posterior mode as a cold fit", {
  skip_on_cran()
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  dp2 <- priors_for(lambda = "normal(0,0.3)")
  fit_warm <- update(fit, dp = dp2, silent = TRUE)
  fit_cold <- acfa(
    mod,
    dat,
    dp = dp2,
    silent = TRUE,
    nsamp = 3,
    test = "none"
  )
  # Compare the deterministic optimiser target (the mode), not sample-based
  # summaries which carry Monte Carlo noise at nsamp = 3.
  expect_equal(
    get_inlavaan_internal(fit_warm, "theta_star_novbc"),
    get_inlavaan_internal(fit_cold, "theta_star_novbc"),
    tolerance = 1e-3
  )
})

test_that("update() errors on a call-less fit", {
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  fit@external$inlavaan_internal$call <- NULL
  expect_error(update(fit), "did not record its call")
})

test_that("start of wrong length is rejected", {
  expect_error(
    acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none", start = 1:3),
    "free parameter"
  )
})

test_that("update() re-parses a recorded test = c('standard', 'loo') call", {
  skip_on_cran()
  fit <- suppressWarnings(acfa(
    mod,
    dat,
    meanstructure = TRUE,
    silent = TRUE,
    nsamp = 3,
    test = c("standard", "loo")
  ))
  fit2 <- suppressWarnings(update(fit, nsamp = 5, silent = TRUE))
  computed <- get_inlavaan_internal(fit2, "test")$computed
  expect_true(all(c("loo", "waic") %in% computed))
})

test_that("update() swaps verbose = TRUE and silent = TRUE", {
  one_factor <- "visual =~ x1 + x2 + x3"
  suppressMessages(
    fit_v <- acfa(one_factor, dat, verbose = TRUE, nsamp = 3, test = "none")
  )
  cl <- update(fit_v, silent = TRUE, evaluate = FALSE)
  expect_null(cl$verbose)
  expect_true(cl$silent)
  expect_silent(fit_s <- update(fit_v, silent = TRUE))
  expect_s4_class(fit_s, "INLAvaan")
  # The reverse, with the value in a variable
  loud <- TRUE
  cl <- update(fit_s, verbose = loud, evaluate = FALSE)
  expect_null(cl$silent)
  expect_identical(cl$verbose, quote(loud))
  # silent = FALSE keeps verbose = TRUE
  cl <- update(fit_v, silent = FALSE, evaluate = FALSE)
  expect_true(cl$verbose)
  # Both asked for still gives the error of inlavaan()
  expect_error(update(fit_v, silent = TRUE, verbose = TRUE), "cannot both be")
})

test_that("the warm start is used only when the parameters keep their names", {
  one_factor <- "visual =~ x1 + x2 + x3"
  fit_one <- function(...) {
    acfa(one_factor, dat, silent = TRUE, nsamp = 3, test = "none", ...)
  }
  iterations <- function(fit) get_inlavaan_internal(fit, "opt")$iterations
  fit <- fit_one()
  # The same parameters: the search for the mode starts at the mode
  expect_lt(iterations(update(fit, nsamp = 5)), iterations(fit))
  # More parameters: no warm start, and no error about its length
  fit_ms <- update(fit, meanstructure = TRUE)
  expect_length(coef(fit_ms), length(coef(fit)) + 3L)
  # As many parameters, under other names: the fit of a cold start
  expect_identical(
    get_inlavaan_internal(update(fit, std.lv = TRUE), "theta_star_novbc"),
    get_inlavaan_internal(fit_one(std.lv = TRUE), "theta_star_novbc")
  )
})

test_that("update() drops an argument given as NULL", {
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  cl <- update(fit, silent = NULL, evaluate = FALSE)
  expect_false("silent" %in% names(cl))
  # Before, NULL for an argument that the call does not have failed with an
  # error of base R.
  cl <- update(fit, ordered = NULL, evaluate = FALSE)
  expect_true("ordered" %in% names(cl))
  expect_null(cl$ordered)
})

test_that("update() replaces an argument given under another spelling", {
  fit <- acfa(
    "visual =~ x1 + x2 + x3",
    dat,
    std.ov = TRUE,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  )
  cl <- update(fit, std_ov = FALSE, evaluate = FALSE)
  expect_false("std.ov" %in% names(cl))
  expect_false(cl$std_ov)
  # Before, lavaan stopped on std.ov and std_ov given together
  fit_new <- update(fit, std_ov = FALSE)
  expect_true(lavaan::lavInspect(fit, "options")$std.ov)
  expect_false(lavaan::lavInspect(fit_new, "options")$std.ov)
})


test_that("update() stops with a clear error when it cannot find the data", {
  dd <- dat[dat$school == "Pasteur", ]
  fit <- acfa(
    "visual =~ x1 + x2 + x3",
    dd,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  )
  dd_given <- dd
  rm(dd)
  err <- expect_error(update(fit), class = "inlavaan_update_data_not_found")
  msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
  expect_match(msg, "cannot find the data `dd`", fixed = TRUE)
  expect_match(msg, "update(fit, data = dd)", fixed = TRUE)
  expect_match(msg, "inlavaan()", fixed = TRUE)
  expect_error(
    update(fit, add = "x1 ~~ x2"),
    class = "inlavaan_update_data_not_found"
  )
  # The call can still be inspected, and the data given again refit
  expect_true(is.call(update(fit, evaluate = FALSE)))
  fit_new <- update(fit, data = dd_given)
  expect_equal(lavaan::lavInspect(fit_new, "nobs"), 156L)
  expect_same_fit(fit_new, fit)
})

test_that("update() counts data named after a function as not found", {
  # In a new session, data named `df` find stats::df(), which lavaan cannot
  # read as data.
  df <- dat[dat$school == "Pasteur", ]
  fit <- acfa(
    "visual =~ x1 + x2 + x3",
    df,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    marginal_method = "gaussian"
  )
  rm(df)
  expect_true(is.function(df))
  expect_error(
    update(fit),
    "cannot find the data `df`",
    class = "inlavaan_update_data_not_found"
  )
})

test_that("update() with a new model gives the fit of a fresh call", {
  skip_on_cran()
  args <- list(
    data = dat,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  )
  # Other exogenous variables
  fit <- do.call(asem, c(list("x1 ~ x2 + x3"), args))
  expect_same_fit(
    update(fit, model = "x2 ~ x1 + x3"),
    do.call(asem, c(list("x2 ~ x1 + x3"), args))
  )
  # The variables in the order of the data, with conditional.x = TRUE
  extra <- list(ov.order = "data", conditional.x = TRUE)
  fit <- do.call(asem, c(list("x1 ~ x2 + x3"), args, extra))
  expect_same_fit(
    update(fit, model = "x2 ~ x1 + x3"),
    do.call(asem, c(list("x2 ~ x1 + x3"), args, extra))
  )
})
