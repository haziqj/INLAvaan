dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
fit_lav <- lavaan::cfa(mod, dat)
NSAMP <- 3
STDLV <- FALSE

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- acfa(
      mod,
      dat,
      marginal_method = "skewnorm",
      verbose = FALSE,
      nsamp = NSAMP,
      std.lv = STDLV
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: asymgaus", {
  expect_no_error({
    fit <- acfa(
      mod,
      dat,
      marginal_method = "asymgaus",
      verbose = FALSE,
      nsamp = NSAMP,
      std.lv = STDLV
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Method: marggaus", {
  expect_no_error({
    fit <- acfa(
      mod,
      dat,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = NSAMP,
      std.lv = STDLV
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Method: sampling", {
  expect_no_error({
    fit <- acfa(
      mod,
      dat,
      marginal_method = "sampling",
      verbose = FALSE,
      # pure-sampling summaries are means of the draws, so a handful of
      # draws is too seed-sensitive for the coef comparison below
      nsamp = 100,
      std.lv = STDLV
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Gradients are correct (Finite Difference Check)", {
  suppressMessages(
    tmp <- capture.output(fit <- acfa(mod, dat, test = "none", debug = TRUE))
  )
  test_df <- read.table(text = tmp, skip = 1)[, -1]
  colnames(test_df) <- c("fd", "analytic", "diff")

  expect_equal(
    as.numeric(test_df$fd),
    as.numeric(test_df$diff),
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(test_df$diff),
    rep(0, nrow(test_df)),
    tolerance = 1e-3
  )
})

test_that("Covariance coefficients are on the covariance scale", {
  # Regression test: coef() used to return the posterior-mean correlation
  # (tanh of theta) for covariance parameters, while summary() showed the
  # sample-based covariance. Invisible when factor sds are near 1, so use a
  # subsample where the two scales differ clearly.
  set.seed(20260612)
  dat_sub <- dat[sample(nrow(dat), 120L), ]
  fit <- acfa(mod, dat_sub, verbose = FALSE, nsamp = NSAMP, test = "none")
  co <- coef(fit)
  summ <- get_inlavaan_internal(fit)$summary

  cov_names <- c("visual~~textual", "visual~~speed", "textual~~speed")
  expect_equal(
    unname(co[cov_names]),
    summ[cov_names, "Mean"],
    tolerance = 1e-10
  )
  # posterior-mean covariances must respect the positive-definiteness bound
  for (nm in cov_names) {
    lv <- strsplit(nm, "~~", fixed = TRUE)[[1]]
    bound <- sqrt(co[paste0(lv[1], "~~", lv[1])] * co[paste0(lv[2], "~~", lv[2])])
    expect_lt(abs(co[nm]), bound)
  }
})
