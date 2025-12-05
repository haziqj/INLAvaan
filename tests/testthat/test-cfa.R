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
      nsamp = NSAMP,
      std.lv = STDLV
    )
  })

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
})

test_that("Gradients are correct (Finite Difference Check)", {
  suppressMessages(
    tmp <- capture.output(fit <- acfa(mod, dat, test = NA, debug = TRUE))
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
library(tidyverse)
library(blavaan)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)

fit_blav <- bcfa(mod, dat, bcontrol = list(cores = 3))
fit_inl1 <- acfa(mod, dat, marginal_method = "skewnorm", debug = TRUE)
fit_inl2 <- acfa(mod, dat, marginal_method = "asymgaus", debug = TRUE)
fit_inl3 <- acfa(mod, dat, marginal_method = "sampling", debug = TRUE)

res <- compare_mcmc(
  fit_blav,
  "skewnorm" = fit_inl1,
  "asymgaus" = fit_inl2,
  "sampling" = fit_inl3
)
print(res$p_compare)
print(res$p_errors)
print(res$metrics_df, n = 1000)
