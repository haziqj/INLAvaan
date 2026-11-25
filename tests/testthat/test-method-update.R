dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

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
  # summaries which carry Monte Carlo noise at nsamp = 3
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

test_that("update() uses the stored data when the data object is gone", {
  one_factor <- "visual =~ x1 + x2 + x3"
  dd <- dat[dat$school == "Pasteur", ]
  fit <- acfa(
    one_factor,
    dd,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  )
  rm(dd)
  suppressMessages(expect_message(
    fit_new <- update(fit, silent = FALSE),
    class = "inlavaan_update_stored_data"
  ))
  expect_equal(lavaan::lavInspect(fit_new, "nobs"), 156L)
  expect_equal(coef(fit_new), coef(fit), tolerance = 1e-5)
  # A silent refit says nothing, and the refit can be updated again
  expect_silent(fit_silent <- update(fit))
  expect_silent(fit_again <- update(fit_silent, nsamp = 5))
  expect_equal(coef(fit_again), coef(fit), tolerance = 1e-5)
  # Given again, the data are read as usual
  expect_no_message(
    update(fit, data = dat, silent = TRUE),
    class = "inlavaan_update_stored_data"
  )
})

test_that("update() drops an argument given as NULL", {
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  cl <- update(fit, silent = NULL, evaluate = FALSE)
  expect_false("silent" %in% names(cl))
  # Before, NULL for an argument that the call does not have failed with an
  # error of base R
  cl <- update(fit, ordered = NULL, evaluate = FALSE)
  expect_true("ordered" %in% names(cl))
  expect_null(cl$ordered)
})

test_that("update() on the stored data stops when it changes how they are read", {
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
  rm(dd)
  err <- "inlavaan_update_data_not_found"
  # Before, lavaan ignored the new options and fitted one group
  expect_error(update(fit, group = "sex"), "cannot change", class = err)
  expect_error(update(fit, group = "sex", silent = TRUE), class = err)
  expect_error(update(fit, ordered = c("x1", "x2", "x3")), class = err)
  expect_error(update(fit, sampling_weights = "grade"), class = err)
  # A model with other observed variables
  expect_error(
    update(fit, model = "visual =~ x1 + x2"),
    "other observed variables",
    class = err
  )
  expect_error(update(fit, add = "visual =~ x4"), class = err)
  # A refit on the stored data checks too, and data given again replace them
  fit_stored <- update(fit, nsamp = 5)
  expect_error(update(fit_stored, group = "sex"), class = err)
  cl <- update(fit_stored, data = dat, evaluate = FALSE)
  expect_false(any(c("slotData", "slot_data") %in% names(cl)))
  fit_full <- update(fit_stored, data = dat, group = "school")
  expect_equal(lavaan::lavInspect(fit_full, "ngroups"), 2L)
  expect_equal(lavaan::lavInspect(fit_full, "nobs"), c(156L, 145L))
})

test_that("update() on the stored data of a listwise fit stops on missing", {
  dd <- dat
  dd$x1[1:20] <- NA
  fit <- acfa(
    "visual =~ x1 + x2 + x3",
    dd,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  )
  rm(dd)
  # Before, this failed with an error of base R
  expect_error(
    update(fit, missing = "ml"),
    "cannot change `missing`",
    class = "inlavaan_update_data_not_found"
  )
})

test_that("check_stored_data_update() lets unchanged options and variables pass", {
  fit <- acfa(
    "visual =~ x1 + x2 + x3",
    dat,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  )
  old_call <- quote(acfa(model, dd, group = "school"))
  # The same expression, also under the other name of lavaan
  expect_silent(check_stored_data_update(
    old_call,
    list(group = "school"),
    NULL,
    fit
  ))
  expect_error(
    check_stored_data_update(old_call, list(group = "sex"), NULL, fit),
    class = "inlavaan_update_data_not_found"
  )
  expect_error(
    check_stored_data_update(old_call, list(sample.cov = quote(S)), NULL, fit),
    class = "inlavaan_update_data_not_found"
  )
  # Options that do not set how the data are read pass
  expect_silent(check_stored_data_update(
    old_call,
    list(std.lv = TRUE),
    NULL,
    fit
  ))
  # The same observed variables, in another order or with priors
  same_vars <- 'visual =~ x3 + prior("normal(0,1)")*x2 + x1'
  expect_silent(check_stored_data_update(old_call, list(), same_vars, fit))
  expect_silent(
    check_stored_data_update(old_call, list(), lavaan::parTable(fit), fit)
  )
  # A model that lavaan cannot parse here is left to lavaan
  expect_null(model_ov_names("visual =~ x1 +"))
  expect_silent(check_stored_data_update(
    old_call,
    list(),
    "visual =~ x1 +",
    fit
  ))
})
