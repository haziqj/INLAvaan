## Validation: Binary/ordered CFA model against MCMC (blavaan)
## Extracted from tests/testthat/test-binary.R
## Requires: future, blavaan

# Simulate binary data
set.seed(141)
n <- 100
truval <- c(0.8, 0.7, 0.6, 0.5, 0.4, -1.43, -0.55, -0.13, -0.72, -1.13)
dat <- lavaan::simulateData(
  "eta =~ 0.8*y1 + 0.7*5y2 + 0.6*y3 + 0.5*y4 + 0.4*y5
   y1 | -1.43*t1
   y2 | -0.55*t1
   y3 | -0.13*t1
   y4 | -0.72*t1
   y5 | -1.13*t1",
  ordered = TRUE,
  sample.nobs = n
)
mod <- "eta  =~ y1 + y2 + y3 + y4 + y5"

testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())
testthat::skip_if_not_installed("future")
library(blavaan)
future::plan("multisession", workers = future::availableCores() - 2)

fit_blav <- bcfa(
  mod,
  dat,
  ordered = TRUE,
  std.lv = TRUE,
  # burnin = 500,
  # sample = 1000,
  # n.chains = 1
  bcontrol = list(cores = 3)
)
fit_inl1 <- acfa(mod, dat, ordered = TRUE, std.lv = TRUE, debug = TRUE)

# Compare
res <- compare_mcmc(fit_blav, INLAvaan = fit_inl1, truth = truval)
print(res$p_compare)
