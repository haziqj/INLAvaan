# A fit made before INLAvaan fitted with the engine has no field `engine`. The
# methods that read the fields stop on it with class inlavaan_old_fit, and
# update() refits it. Such a fit is made here by removing the field from a
# current fit.

dat <- lavaan::HolzingerSwineford1939
mod <- "visual =~ x1 + x2 + x3"

make_old_fit <- function(version = "0.3.5") {
  fit <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  fit@external$inlavaan_internal$engine <- NULL
  fit@external$inlavaan_internal$version <- version
  fit
}

test_that("the methods that read the fields stop on a fit made before the engine", {
  old <- make_old_fit()
  int <- get_inlavaan_internal(old)
  cls <- "inlavaan_old_fit"
  expect_error(diagnostics(old), class = cls)
  expect_error(diagnostics(old, type = "param"), class = cls)
  expect_error(sampling(old, nsamp = 3), class = cls)
  expect_error(sampling(int, nsamp = 3), class = cls)
  expect_error(loo(old), class = cls)
  expect_error(loo(int), class = cls)
  expect_error(predict(old, nsamp = 3), class = cls)
  expect_error(predict(int, nsamp = 3), class = cls)
  expect_error(simulate(old), class = cls)
  expect_error(fitmeasures(old), class = cls)
  expect_error(fitMeasures(old, "BRMSEA"), class = cls)
  expect_error(waic(old), class = cls)
  expect_error(waic(int), class = cls)
  expect_error(standardisedsolution(old, nsamp = 3), class = cls)
  expect_error(standardizedSolution(old, nsamp = 3), class = cls)
  expect_error(summary(old, standardised = TRUE), class = cls)
  expect_error(bfit_indices(old), class = cls)
})

test_that("the error names the method, the version and the way to refit", {
  err <- expect_error(loo(make_old_fit()), class = "inlavaan_old_fit")
  msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
  expect_match(msg, "loo()", fixed = TRUE)
  expect_match(
    msg,
    "a fit made before the inlavanilla engine (INLAvaan 0.3.5)",
    fixed = TRUE
  )
  expect_match(msg, "fit <- update(fit)", fixed = TRUE)
  expect_null(conditionCall(err))

  # A fit that records no version
  old <- make_old_fit(version = NULL)
  err <- expect_error(simulate(old), class = "inlavaan_old_fit")
  expect_match(
    gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err))),
    "before the inlavanilla engine (an older version of INLAvaan)",
    fixed = TRUE
  )
})

test_that("update() refits a fit made before the engine", {
  old <- make_old_fit()
  cl <- update(old, evaluate = FALSE)
  expect_true(is.call(cl))
  fit <- update(old)
  expect_false(is.null(get_inlavaan_internal(fit, "engine")))
  expect_identical(
    get_inlavaan_internal(fit, "version"),
    as.character(utils::packageVersion("INLAvaan"))
  )
  expect_no_error(diagnostics(fit))
})

test_that("summary() and show() still work on a fit made before the engine", {
  old <- make_old_fit()
  expect_output(show(old), "INLAvaan 0.3.5", fixed = TRUE)
  expect_output(summary(old))
})
