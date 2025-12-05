mod <- "
  # intercept with coefficients fixed to 1
  i =~  1*Day0 + 1*Day1 + 1*Day2 + 1*Day3 + 1*Day4 +
        1*Day5 + 1*Day6 + 1*Day7 + 1*Day8 + 1*Day9

  # slope with coefficients fixed to 0:9 (number of days)
  s =~  0*Day0 + 1*Day1 + 2*Day2 + 3*Day3 + 4*Day4 +
        5*Day5 + 6*Day6 + 7*Day7 + 8*Day8 + 9*Day9

  i ~~ i
  i ~ 1

  s ~~ s
  s ~ 1

  i ~~ s

  # fix intercepts
  Day0 ~ 0*1
  Day1 ~ 0*1
  Day2 ~ 0*1
  Day3 ~ 0*1
  Day4 ~ 0*1
  Day5 ~ 0*1
  Day6 ~ 0*1
  Day7 ~ 0*1
  Day8 ~ 0*1
  Day9 ~ 0*1

  # apply equality constraints
  Day0 ~~ v*Day0
  Day1 ~~ v*Day1
  Day2 ~~ v*Day2
  Day3 ~~ v*Day3
  Day4 ~~ v*Day4
  Day5 ~~ v*Day5
  Day6 ~~ v*Day6
  Day7 ~~ v*Day7
  Day8 ~~ v*Day8
  Day9 ~~ v*Day9
  "
dat <- reshape(
  lme4::sleepstudy,
  timevar = "Days",
  idvar = "Subject",
  direction = "wide"
)
names(dat) <- sub("^Reaction\\.(.*)$", "Day\\1", names(dat))
fit_lav <- lavaan::growth(mod, dat)
NSAMP <- 3

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "skewnorm",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  gr_at_opt <- fit@optim$dx
  gt_at_opt <- as.numeric(fit@Model@ceq.simple.K %*% gr_at_opt)
  expect_equal(gt_at_opt, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: asymgaus", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "asymgaus",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: marggaus", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: sampling", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "sampling",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
})

test_that("Gradients are correct (Finite Difference Check)", {
  suppressMessages(
    tmp <- capture.output(fit <- agrowth(mod, dat, test = NA, debug = TRUE))
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

fit_blav <- bgrowth(
  mod,
  dat,
  bcontrol = list(cores = 3),
  burnin = 1000,
  sample = 2000
)
fit_inl1 <- agrowth(
  mod,
  dat,
  marginal_method = "skewnorm",
  debug = TRUE,
  test = FALSE
)
fit_inl2 <- agrowth(
  mod,
  dat,
  marginal_method = "asymgaus",
  debug = TRUE,
  test = FALSE
)
fit_inl3 <- agrowth(
  mod,
  dat,
  marginal_method = "sampling",
  debug = TRUE,
  test = FALSE
)

res <- compare_mcmc(
  fit_blav,
  "skewnorm" = fit_inl1,
  "asymgaus" = fit_inl2,
  "sampling" = fit_inl3
)
print(res$p_compare)
print(res$p_errors)
print(res$metrics_df, n = 1000)
