## Validation: Growth model against MCMC (blavaan)
## Extracted from tests/testthat/test-growth.R
## Requires: future, blavaan

mod <- "
  # intercept and slope with fixed coefficients
  i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
  s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

  # regressions
  i ~ x1 + x2
  s ~ x1 + x2

  # time-varying covariates
  t1 ~ c1
  t2 ~ c2
  t3 ~ c3
  t4 ~ c4
"
dat <- lavaan::Demo.growth

testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())
testthat::skip_if_not_installed("future")
library(blavaan)
future::plan("multisession", workers = future::availableCores() - 2)

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
  test = "none"
)
fit_inl2 <- agrowth(
  mod,
  dat,
  marginal_method = "asymgaus",
  debug = TRUE,
  test = "none"
)
fit_inl3 <- agrowth(
  mod,
  dat,
  marginal_method = "sampling",
  debug = TRUE,
  test = "none"
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
