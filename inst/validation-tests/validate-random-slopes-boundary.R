################################################################################
#
# Document the boundary case: a random-slope variance whose maximum-likelihood
# estimate is negative.
#
# Fits the random-slope Demo.twolevel model, where lavaan's own MLE of the slope
# variance `s1~~s1` is slightly negative (about -0.003), i.e. the truth sits at
# the boundary of the parameter space. Reports the posterior summary of the
# slope variance under both marginal_method = 'skewnorm' (the default) and
# marginal_method = 'sampling', the diagnostics the package exposes for it, and
# a one-dimensional profile of the joint log-posterior along its log-variance
# axis. This script documents behaviour rather than validating recovery of a
# known truth. Run after devtools::load_all(".").
################################################################################

## ----- Configuration ---------------------------------------------------------
smoke <- FALSE # TRUE shrinks nsamp for a quick development run
set.seed(202605)
nsamp <- if (smoke) 200L else 2000L
n_grid <- 15L # points in the log-posterior profile

mod <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    s1 ~ w1
"

## =============================================================================
## Fit the boundary model
## =============================================================================
fit_lav <- suppressWarnings(
  lavaan::sem(mod, lavaan::Demo.twolevel, cluster = "cluster")
)
mle_var <- unname(coef(fit_lav)["s1~~s1.l2"])
cat(sprintf("\nlavaan MLE of s1~~s1: %.5f\n", mle_var))

fit_sn <- asem(
  mod,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  nsamp = nsamp,
  marginal_method = "skewnorm"
)
fit_samp <- asem(
  mod,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  nsamp = nsamp,
  marginal_method = "sampling"
)

## =============================================================================
## Posterior summaries of the slope variance
## =============================================================================

## ----- Comparison table ------------------------------------------------------
summ_sn <- get_inlavaan_internal(fit_sn)$summary["s1~~s1.l2", ]
summ_samp <- get_inlavaan_internal(fit_samp)$summary["s1~~s1.l2", ]

tab <- data.frame(
  method = c("lavaan MLE", "skewnorm", "sampling"),
  mean = c(mle_var, summ_sn[["Mean"]], summ_samp[["Mean"]]),
  mode = c(NA, summ_sn[["Mode"]], summ_samp[["Mode"]]),
  ci_2.5 = c(NA, summ_sn[["2.5%"]], summ_samp[["2.5%"]]),
  ci_97.5 = c(NA, summ_sn[["97.5%"]], summ_samp[["97.5%"]])
)
print(tab, digits = 4, row.names = FALSE)

## ----- Diagnostics for the slope variance ------------------------------------
diag_param_sn <- diagnostics(fit_sn, type = "param")
diag_row_sn <- diag_param_sn[diag_param_sn$names == "s1~~s1.l2", ]
print(diag_row_sn, row.names = FALSE)

approx_row <- get_inlavaan_internal(fit_sn)$approx_data["s1~~s1.l2", ]
cat(sprintf(
  "\nnmad (skew-normal fit vs scanned posterior, skewnorm method): %.4f\n",
  approx_row[["nmad"]]
))

## =============================================================================
## One-dimensional profile of the log-posterior
## =============================================================================
# Reconstruct the joint log-posterior joint_lp() builds inside inlavaan()
# (see R/inlavaan.R): the lavaan log-likelihood plus the log prior density,
# both evaluated in lavaan-x space after the packed-theta -> x map. The
# `lavoptions` used at fit time are not stored on the internal list (they
# get relabelled to "Bayes" on the returned INLAvaan object), so they are
# rebuilt with a do.fit = FALSE call on the same model and data.
int_sn <- get_inlavaan_internal(fit_sn)
pt <- int_sn$partable
lavoptions <- lavaan::sem(
  mod,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  do.fit = FALSE
)@Options
prior_cache <- INLAvaan:::prepare_priors_for_optim(pt)

idx <- which(rownames(int_sn$summary) == "s1~~s1.l2")
theta_star <- int_sn$theta_star
se_j <- sqrt(int_sn$Sigma_theta[idx, idx])
grid_z <- seq(-4, 4, length.out = n_grid)
theta_grid <- theta_star[idx] + grid_z * se_j

log_post <- vapply(
  theta_grid,
  function(tj) {
    th <- theta_star
    th[idx] <- tj
    x <- INLAvaan:::pars_to_x(th, pt)
    ll <- INLAvaan:::inlav_model_loglik(
      x,
      int_sn$lavmodel,
      int_sn$lavsamplestats,
      int_sn$lavdata,
      lavoptions,
      int_sn$lavcache
    )
    ll + INLAvaan:::prior_logdens_vectorized(th, prior_cache)
  },
  numeric(1)
)

# Trapezoidal-normalise the profile on its own grid so it sits on the same
# scale as the fitted skew-normal density.
post_dens <- exp(log_post - max(log_post))
area <- sum((post_dens[-1] + post_dens[-n_grid]) / 2 * diff(theta_grid))
post_dens <- post_dens / area

sn_dens <- dsnorm(
  theta_grid,
  xi = approx_row[["xi"]],
  omega = approx_row[["omega"]],
  alpha = approx_row[["alpha"]]
)

profile_tab <- data.frame(
  z = grid_z,
  log_variance = theta_grid,
  variance = exp(theta_grid),
  post_density = post_dens,
  skewnorm_density = sn_dens
)
print(profile_tab, digits = 4, row.names = FALSE)
cat(
  "\nThe leftmost rows (large negative z, variance close to 0) show the",
  "\nexponential-tailed decay the skew-normal fit is designed to track.\n"
)

## =============================================================================
## Assertions
## =============================================================================
# This script documents behaviour rather than testing recovery of a known
# truth, so the only checks are the qualitative signature of a boundary
# parameter: a non-negative posterior mean and a 95% interval that reaches
# down to (numerically) zero. The variance is strictly positive under the
# log-variance parameterisation, so the interval cannot touch zero exactly.
# "Reaches down to zero" is read as a lower bound far smaller than the
# upper bound.
near_zero <- 0.01
sn_lo <- summ_sn[["2.5%"]]
samp_lo <- summ_samp[["2.5%"]]
stopifnot(
  "skewnorm posterior mean is non-negative" = summ_sn[["Mean"]] >= 0,
  "skewnorm 95% interval reaches down to (numerically) zero" = sn_lo <
    near_zero,
  "sampling posterior mean is non-negative" = summ_samp[["Mean"]] >= 0,
  "sampling 95% interval reaches down to (numerically) zero" = samp_lo <
    near_zero
)
