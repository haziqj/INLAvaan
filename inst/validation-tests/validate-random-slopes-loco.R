################################################################################
#
# Validate Taylor-approximated leave-one-cluster-out (LOCO) cross-validation
# against brute-force deleted-cluster refits, for a random-slope model. Run
# after devtools::load_all(".").
################################################################################

## ----- Configuration ---------------------------------------------------------
smoke <- FALSE # TRUE shrinks J and nsamp for a quick development run
set.seed(202602)

J <- if (smoke) 20L else 60L
n <- 10L
nsamp_fit <- if (smoke) 200L else 1000L
nsamp_refit <- if (smoke) 200L else 2000L
n_loco <- 5L # clusters compared by brute force

## =============================================================================
## Simulate data
## =============================================================================
# Same DGP as validate-random-slopes-lmer.R: a random intercept and a random
# slope on a single within-cluster covariate, with a nonzero intercept-slope
# covariance.
cluster <- rep(seq_len(J), each = n)
x1 <- rep(rnorm(J), each = n) + rnorm(J * n)

Sigma_u <- matrix(c(0.5, 0.1, 0.1, 0.25), nrow = 2)
u <- matrix(rnorm(J * 2), nrow = J) %*% chol(Sigma_u)
u0 <- u[, 1]
u1 <- u[, 2]

y1 <- 1 + u0[cluster] + (0.5 + u1[cluster]) * x1 + rnorm(J * n)
d <- data.frame(y1 = y1, x1 = x1, cluster = cluster)

mod <- "
  level: 1
    y1 ~ rv('s1')*x1
  level: 2
    y1 ~~ y1
    s1 ~~ s1
    s1 ~~ y1
"

## =============================================================================
## Fit the full-data model
## =============================================================================
fit <- asem(mod, d, cluster = "cluster", nsamp = nsamp_fit)
int_full <- get_inlavaan_internal(fit)
print(round(int_full$summary[, c("Mean", "2.5%", "97.5%")], 3))

## =============================================================================
## Leave-one-cluster-out comparison
## =============================================================================
# The guard below reports a failing loo() instead of stopping the script.
loo_try <- try(loo(fit), silent = TRUE)
loo_ready <- !inherits(loo_try, "try-error")

if (!loo_ready) {
  cat(
    "\nloo() failed; skipping the LOCO comparison.\n",
    conditionMessage(attr(loo_try, "condition")),
    "\n"
  )
} else {
  clusters_loco <- sort(sample(seq_len(J), n_loco))

  # Brute force: refit with cluster j held out, draw from its posterior, and
  # evaluate cluster j's log-likelihood at each draw under the FULL-data
  # cache, which still knows every cluster (including j).
  brute <- numeric(n_loco)
  for (k in seq_along(clusters_loco)) {
    j <- clusters_loco[k]
    fit_j <- asem(
      mod,
      d[d$cluster != j, ],
      cluster = "cluster",
      nsamp = nsamp_refit,
      verbose = FALSE
    )
    x_samp <- INLAvaan:::sample_params_posterior(
      get_inlavaan_internal(fit_j),
      nsamp = nsamp_refit,
      samp_copula = TRUE
    )$x_samp
    l_j <- apply(x_samp, 1, function(x) {
      attr(
        INLAvaan:::lavaan___lav_mvn_cl_rs_m2ll(
          lavmodel = lavaan::lav_model_set_parameters(int_full$lavmodel, x),
          rs = int_full$lavcache[[1L]]$rs,
          log2pi = TRUE,
          minus_two = FALSE,
          per_cluster = TRUE
        ),
        "loglik.cluster"
      )[j]
    })
    mx <- max(l_j)
    brute[k] <- log(mean(exp(l_j - mx))) + mx
  }

  pu <- loo_try$per_unit
  idx <- match(clusters_loco, pu$unit)
  tab <- data.frame(
    cluster = clusters_loco,
    nobs = pu$nobs[idx],
    brute = brute,
    taylor_1 = pu$log_cpo_1[idx],
    taylor_2 = pu$log_cpo_2[idx],
    k_max = pu$k_max[idx]
  )
  print(tab, digits = 4, row.names = FALSE)
  cat(sprintf(
    "\nSummed gap (taylor_2 - brute): %.4f\n",
    sum(tab$taylor_2 - tab$brute)
  ))
}
