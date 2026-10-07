################################################################################
#
# Validation: composites (<~) against MCMC (blavaan). blavaan has no <~, so the
# MCMC side fits the phantom specification C =~ 0, C ~ 1*x1 + w2*x2 + w3*x3,
# C ~~ 0*C, which reproduces lavaan's <~ fit exactly when the composite's
# indicators are exogenous observed variables. With two or more composites the
# phantom is a different model, so every section has a single composite. Both
# models carry the same labels so that compare_mcmc() matches the parameters.
# Requires: blavaan.
#
################################################################################

## ----- Configuration ---------------------------------------------------------
testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())
testthat::skip_if_not_installed("blavaan")
library(blavaan)

# Weakly identified composites give a curved posterior that needs a small Stan
# step size, otherwise bsem() reports divergent transitions.
n_chains <- 4
n_burnin <- 2000
n_sample <- 5000
mcmc_control <- list(cores = n_chains, adapt_delta = 0.99)
n_sim <- 300

## ----- Helpers ---------------------------------------------------------------
# Three correlated indicators x, a true composite x %*% a and two outcomes
# y = g * composite + e. On lavaan's scale (first weight fixed to 1) the truth
# is w_j = a_j / a_1 and b_k = g_k * a_1.
sim_composite <- function(n, a, g, rho_x = 0.3, rho_e = 0.3, seed = 1) {
  set.seed(seed)
  p <- length(a)
  r_x <- matrix(rho_x, p, p)
  diag(r_x) <- 1
  x <- matrix(rnorm(n * p), n, p) %*% chol(r_x)
  comp <- drop(x %*% a)
  psi <- matrix(c(1, rho_e, rho_e, 1), 2, 2)
  e <- matrix(rnorm(n * 2), n, 2) %*% chol(psi)
  y <- outer(comp, g) + e
  dat <- data.frame(x, y)
  names(dat) <- c(paste0("x", seq_len(p)), "y1", "y2")
  dat
}

truth_composite <- function(a, g, rho_e = 0.3) {
  c(
    w2 = a[2] / a[1],
    w3 = a[3] / a[1],
    b1 = g[1] * a[1],
    b2 = g[2] * a[1],
    "y1~~y2" = rho_e,
    "y1~~y1" = 1,
    "y2~~y2" = 1
  )
}

# compare_mcmc() scores the densities on INLAvaan's grid (the mode plus or minus
# four Laplace SDs), so it misses MCMC mass in a heavy tail. This table puts the
# moments and the 95% limits of both posteriors side by side, with the share of
# MCMC draws outside INLAvaan's grid.
quantile_table <- function(fit_blav, fit_inl) {
  draws <- do.call("rbind", blavInspect(fit_blav, "mcmc"))
  int <- get_inlavaan_internal(fit_inl)
  summ <- int$summary
  pars <- intersect(colnames(draws), rownames(summ))
  out <- t(vapply(
    pars,
    function(p) {
      d <- draws[, p]
      grid <- range(int$pdf_data[[p]]$x)
      c(
        mcmc_mean = mean(d),
        inla_mean = summ[p, "Mean"],
        mcmc_sd = sd(d),
        inla_sd = summ[p, "SD"],
        mcmc_q025 = unname(quantile(d, 0.025)),
        inla_q025 = summ[p, "2.5%"],
        mcmc_q975 = unname(quantile(d, 0.975)),
        inla_q975 = summ[p, "97.5%"],
        outside = mean(d < grid[1] | d > grid[2])
      )
    },
    numeric(9)
  ))
  round(out, 3)
}

mod_sim <- "
  C <~ 1*x1 + w2*x2 + w3*x3
  y1 ~ b1*C
  y2 ~ b2*C
  y1 ~~ y2
"
mod_sim_phantom <- "
  C =~ 0
  C ~ 1*x1 + w2*x2 + w3*x3
  C ~~ 0*C
  y1 ~ b1*C
  y2 ~ b2*C
  y1 ~~ y2
"

## =============================================================================
## Reference model --- HolzingerSwineford1939
## =============================================================================

dat <- lavaan::HolzingerSwineford1939
mod <- "
  C <~ 1*x1 + w2*x2 + w3*x3
  x4 ~ b4*C
  x5 ~ b5*C
  x4 ~~ x5
"
mod_phantom <- "
  C =~ 0
  C ~ 1*x1 + w2*x2 + w3*x3
  C ~~ 0*C
  x4 ~ b4*C
  x5 ~ b5*C
  x4 ~~ x5
"

set.seed(1)
fit_blav <- bsem(
  mod_phantom,
  dat,
  n.chains = n_chains,
  burnin = n_burnin,
  sample = n_sample,
  bcontrol = mcmc_control
)
fit_inl <- asem(mod, dat, test = "none")

res_ref <- compare_mcmc(fit_blav, skewnorm = fit_inl)
print(res_ref$p_compare)
print(res_ref$metrics_df)
print(quantile_table(fit_blav, fit_inl))

## =============================================================================
## Composite predicting a factor --- HolzingerSwineford1939
## =============================================================================

mod_fac <- "
  C <~ 1*x1 + w2*x2 + w3*x3
  F =~ x4 + l5*x5 + l6*x6
  F ~ b*C
"
mod_fac_phantom <- "
  C =~ 0
  C ~ 1*x1 + w2*x2 + w3*x3
  C ~~ 0*C
  F =~ x4 + l5*x5 + l6*x6
  F ~ b*C
"

set.seed(1)
fit_blav_fac <- bsem(
  mod_fac_phantom,
  dat,
  n.chains = n_chains,
  burnin = n_burnin,
  sample = n_sample,
  bcontrol = mcmc_control
)
fit_inl_fac <- asem(mod_fac, dat, test = "none")

res_fac <- compare_mcmc(fit_blav_fac, skewnorm = fit_inl_fac)
print(res_fac$p_compare)
print(res_fac$metrics_df)
print(quantile_table(fit_blav_fac, fit_inl_fac))

## =============================================================================
## Weak marker --- small true weight on the indicator fixed to 1
## =============================================================================

a_marker <- c(0.3, 1, 1)
g_marker <- c(0.5, 0.5)
dat_marker <- sim_composite(n_sim, a_marker, g_marker, seed = 3)

set.seed(1)
fit_blav_marker <- bsem(
  mod_sim_phantom,
  dat_marker,
  n.chains = n_chains,
  burnin = n_burnin,
  sample = n_sample,
  bcontrol = mcmc_control
)
fit_inl_marker <- asem(mod_sim, dat_marker, test = "none")

res_marker <- compare_mcmc(
  fit_blav_marker,
  skewnorm = fit_inl_marker,
  truth = truth_composite(a_marker, g_marker)
)
print(res_marker$p_compare)
print(res_marker$metrics_df)
print(quantile_table(fit_blav_marker, fit_inl_marker))

## =============================================================================
## Weak paths --- the composite barely predicts the outcomes
## =============================================================================

a_paths <- c(1, 1, 1)
g_paths <- c(0.15, 0.15)
dat_paths <- sim_composite(n_sim, a_paths, g_paths, seed = 2)

set.seed(1)
fit_blav_paths <- bsem(
  mod_sim_phantom,
  dat_paths,
  n.chains = n_chains,
  burnin = n_burnin,
  sample = n_sample,
  bcontrol = mcmc_control
)
fit_inl_paths <- asem(mod_sim, dat_paths, test = "none")

res_paths <- compare_mcmc(
  fit_blav_paths,
  skewnorm = fit_inl_paths,
  truth = truth_composite(a_paths, g_paths)
)
print(res_paths$p_compare)
print(res_paths$metrics_df)
print(quantile_table(fit_blav_paths, fit_inl_paths))

## =============================================================================
## Two groups --- HolzingerSwineford1939 by school
## =============================================================================

mod_mg <- "
  C <~ 1*x1 + c(w2a, w2b)*x2 + c(w3a, w3b)*x3
  x4 ~ c(b4a, b4b)*C
  x5 ~ c(b5a, b5b)*C
  x4 ~~ x5
"
mod_mg_phantom <- "
  C =~ 0
  C ~ 1*x1 + c(w2a, w2b)*x2 + c(w3a, w3b)*x3
  C ~~ 0*C
  x4 ~ c(b4a, b4b)*C
  x5 ~ c(b5a, b5b)*C
  x4 ~~ x5
"

set.seed(1)
fit_blav_mg <- bsem(
  mod_mg_phantom,
  dat,
  group = "school",
  n.chains = n_chains,
  burnin = n_burnin,
  sample = n_sample,
  bcontrol = c(mcmc_control, max_treedepth = 12)
)
fit_inl_mg <- asem(mod_mg, dat, group = "school", test = "none")

# INLAvaan also reports the indicator intercepts (x1 ~ 1 and so on), which the
# phantom model fixes at the sample means, so they have no MCMC counterpart.
res_mg <- compare_mcmc(fit_blav_mg, skewnorm = fit_inl_mg)
print(res_mg$p_compare)
print(res_mg$metrics_df)
print(quantile_table(fit_blav_mg, fit_inl_mg))
