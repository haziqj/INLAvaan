mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
"
dat <- lavaan::PoliticalDemocracy
fit_lav <- lavaan::cfa(mod, dat)
NSAMP <- 3

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "skewnorm",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  # Summary
  expect_no_error(
    out <- capture.output(summary(fit, postmedian = TRUE, postmode = TRUE))
  )
  expect_warning(out <- capture.output(summary(fit, standardized = TRUE)))
  expect_no_error({
    tmp <- get_inlavaan_internal(fit)
    out <- capture.output(print(tmp))
    out <- capture.output(print(summary(tmp)))
  })

  expect_s4_class(fit, "INLAvaan")
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: asymgaus", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "asymgaus",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  # expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Method: marggaus", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  # expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Method: sampling", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "sampling",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  # expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Gradients are correct (Finite Difference Check)", {
  suppressMessages(
    tmp <- capture.output(fit <- asem(mod, dat, test = NA, debug = TRUE))
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

################################################################################
## CHECK AGAINST MCMC ##########################################################
################################################################################
testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())
library(blavaan)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)

fit_blav <- bsem(
  mod,
  dat,
  bcontrol = list(cores = 3),
  burnin = 1000,
  sample = 2000
)


fit_inl1 <- asem(
  mod,
  dat,
  marginal_method = "skewnorm",
  debug = TRUE,
  test = FALSE
)
fit_inl2 <- asem(
  mod,
  dat,
  marginal_method = "skewnorm",
  marginal_correction = "none",
  debug = TRUE,
  test = FALSE
)
fit_inl3 <- asem(
  mod,
  dat,
  marginal_method = "asymgaus",
  debug = TRUE,
  test = FALSE
)

res <- compare_mcmc(
  fit_blav,
  "skewnorm" = fit_inl1,
  "sn_nocor" = fit_inl2,
  "asymgaus" = fit_inl3,
  show_error = FALSE
)
print(res$p_compare)
# print(res$p_errors)
# print(res$metrics_df, n = 1000)
