## Validation: SEM model against MCMC (blavaan)
## Extracted from tests/testthat/test-sem.R
## Requires: future, blavaan

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

testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())
testthat::skip_if_not_installed("future")
library(blavaan)
future::plan("multisession", workers = future::availableCores() - 2)

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
  test = "none"
)
fit_inl2 <- asem(
  mod,
  dat,
  marginal_method = "skewnorm",
  marginal_correction = "none",
  debug = TRUE,
  test = "none"
)
fit_inl3 <- asem(
  mod,
  dat,
  marginal_method = "skewnorm",
  # marginal_correction = "none",
  vb_correction = FALSE,
  debug = TRUE,
  test = "none"
)

res <- compare_mcmc(
  fit_blav,
  "skewnorm" = fit_inl1,
  # "sn_nocor" = fit_inl2,
  # "sn_novb" = fit_inl3,
  show_error = !FALSE
)
print(res$p_compare)
# print(res$p_errors)
# print(res$metrics_df, n = 1000)
