## Validation: CFA model against MCMC (blavaan)
## Extracted from tests/testthat/test-cfa.R
## Requires: future, blavaan

dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())
testthat::skip_if_not_installed("future")
library(blavaan)
future::plan("multisession", workers = future::availableCores() - 2)

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
