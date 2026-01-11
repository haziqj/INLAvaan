testthat::skip()
library(blavaan)
library(lavaan)

truval <- c(0.8, 0.7, 0.6, 0.5, 0.4, -1.43, -0.55, -0.13, -0.72, -1.13)
dat <- lavaan::simulateData(
  "eta =~ 0.8*y1 + 0.7*5y2 + 0.6*y3 + 0.5*y4 + 0.4*y5
   y1 | -1.43*t1
   y2 | -0.55*t1
   y3 | -0.13*t1
   y4 | -0.72*t1
   y5 | -1.13*t1",
  ordered = TRUE,
  sample.nobs = 10000
)
mod <- "eta  =~ y1 + y2 + y3 + y4 + y5"
fit <- acfa(
  mod,
  dat,
  estimator = "PML",
  ordered = TRUE,
  std.lv = TRUE,
  # add_priors = !TRUE
  # numerical_grad = TRUE,
  # vb_correction = FALSE,
  # marginal_method = "marggaus"
)
plot(fit, truth = truval)

# fit_lav <- cfa(mod, dat, ordered = TRUE, std.lv = TRUE)
fit_blav <- bcfa(
  mod,
  dat,
  ordered = TRUE,
  std.lv = TRUE,
  burnin = 500,
  sample = 1000,
  n.chains = 1
)

res <- compare_mcmc(fit_blav, inlavaan = fit, truth = truval)
print(res$p_compare)
