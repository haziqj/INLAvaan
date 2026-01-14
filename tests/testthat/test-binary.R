testthat::skip()
library(blavaan)
library(lavaan)
set.seed(141)

# Data
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

# Scaling factor (kappa)
fit_lav <- cfa(mod, dat, estimator = "PML", ordered = TRUE, std.lv = TRUE)
V <- lavaan___lav_model_vcov(
  # vcov robust sandwich
  lavmodel = fit_lav@Model,
  lavsamplestats = fit_lav@SampleStats,
  lavoptions = fit_lav@Options,
  lavdata = fit_lav@Data,
  lavpartable = fit_lav@ParTable,
  lavcache = fit_lav@Cache
) *
  n
V_naive <- attr(V, "E.inv") # naive vcov (H^{-1})
(kappa <- sum(diag(V_naive)) / sum(diag(V)))

# Fit INLAvaan
fit <- acfa(
  mod,
  dat,
  estimator = "PML",
  ordered = TRUE,
  std.lv = TRUE,
  parameterization = "theta" ## << important
  # add_priors = !TRUE
  # numerical_grad = TRUE,
  # vb_correction = FALSE,
  # marginal_method = "marggaus"
)
plot(fit, truth = truval)

# Fit blavaan
fit_blav <- bcfa(
  mod,
  dat,
  ordered = TRUE,
  std.lv = TRUE,
  burnin = 500,
  sample = 1000,
  n.chains = 1
)

# Compare
res <- compare_mcmc(fit_blav, skewnorm = fit, truth = truval)
print(res$p_compare)
