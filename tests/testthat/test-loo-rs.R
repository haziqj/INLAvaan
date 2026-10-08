## ----- Shared random-slope fixture (route A, 24 clusters) --------------------
# Same 24-cluster subset of lavaan::Demo.twolevel as the other random-slope
# tests: 300 rows, cluster sizes cycling 5/10/15/20, every fit a second or
# two. `rv('s1')` makes the level-1 slope of x1 a level-2 latent variable,
# so the likelihood comes from lavaan's per-cluster random-slope kernels
# rather than from the model-implied moments.
d_rs <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
mod_rs <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~ w1
"

fit_rs <- asem(
  mod_rs,
  d_rs,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)
res <- loo(fit_rs)
int <- get_inlavaan_internal(fit_rs)
spec <- INLAvaan:::rs_spec(int)

# The log-likelihood at the scoring point, through INLAvaan's own wrapper.
# The fit records estimator "Bayes" in its options, which the lavaan kernel
# does not recognise, so the ML label is restored for the call.
rs_model_loglik <- function(fit) {
  int <- get_inlavaan_internal(fit)
  lavmodel <- int$lavmodel
  theta <- int$theta_star
  if (isTRUE(lavmodel@ceq.simple.only)) {
    theta <- as.numeric(lavmodel@ceq.simple.K %*% theta)
  }
  opts <- fit@Options
  opts$estimator <- "ML"
  INLAvaan:::inlav_model_loglik(
    INLAvaan:::pars_to_x(theta, int$partable),
    lavmodel,
    int$lavsamplestats,
    int$lavdata,
    opts,
    int$lavcache
  )
}

test_that("LOCO scores a random-slope fit cluster by cluster", {
  expect_s3_class(res, "inlavaan_loo")
  expect_equal(res$type, "loco")
  expect_equal(res$n_units, 24L)
  expect_equal(sum(res$per_unit$nobs), nrow(d_rs))
  expect_equal(res$per_unit$unit, seq_len(24L))
  expect_true(is.finite(res$elpd_1))
  expect_true(is.finite(res$elpd_2))
  expect_true(is.finite(res$estimates["elpd_loo", "Estimate"]))
  expect_true(all(res$per_unit$ok))
  expect_true(all(res$per_unit$k_max < 1))
})

test_that("the random-slope kernel is scored conditionally", {
  expect_equal(res$flavour, "conditional")

  # The cluster contributions sum to the fitted log-likelihood exactly: the
  # kernel is already the conditional density of the outcomes given the
  # covariates, so nothing is subtracted from it.
  ll <- rs_model_loglik(fit_rs)
  expect_equal(sum(res$per_unit$l_star), ll, tolerance = 1e-6)

  # The frozen-covariate constant the complete-data branch would subtract is
  # substantial, so the conditional scale is not an accident of a small shift
  css <- INLAvaan:::loco_suff_stats(int$lavdata)
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  fx_const <- INLAvaan:::loo_fixedx_const_loco(
    int,
    css,
    seq_len(24L),
    cache$mom
  )
  expect_gt(abs(sum(fx_const)), 1)
  expect_false(isTRUE(all.equal(
    sum(res$per_unit$l_star),
    ll - sum(fx_const)
  )))
})

test_that("the per-cluster scores are lavaan's analytic scores", {
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  lavmodel_x <- lavaan::lav_model_set_parameters(int$lavmodel, cache$x)
  G_x <- INLAvaan:::lavaan___lav_mvn_cl_rs_scores(
    lavmodel = lavmodel_x,
    rs = spec$rs
  )
  expect_equal(nrow(G_x), 24L)

  # Summing the per-cluster scores must reproduce lavaan's own total
  # gradient, which is the derivative of the average of -2 * loglik
  grad <- INLAvaan:::lavaan___lav_model_grad(
    lavmodel = lavmodel_x,
    lavsamplestats = int$lavsamplestats,
    lavdata = int$lavdata,
    lavcache = int$lavcache
  )
  expect_equal(
    colSums(G_x),
    -int$lavsamplestats@ntotal * as.numeric(grad),
    tolerance = 1e-8
  )
})

test_that("theta-space cluster scores match finite differences", {
  units <- 1:3
  s_an <- INLAvaan:::loco_rs_scores_theta(
    int$theta_star,
    spec$rs,
    int$lavmodel,
    int$partable,
    units
  )
  h <- 1e-5
  s_fd <- vapply(
    seq_along(int$theta_star),
    function(k) {
      th_up <- th_dn <- int$theta_star
      th_up[k] <- th_up[k] + h
      th_dn[k] <- th_dn[k] - h
      (INLAvaan:::loco_rs_loglik_all(
        th_up,
        spec$rs,
        int$lavmodel,
        int$partable,
        units
      ) -
        INLAvaan:::loco_rs_loglik_all(
          th_dn,
          spec$rs,
          int$lavmodel,
          int$partable,
          units
        )) /
        (2 * h)
    },
    numeric(length(units))
  )
  expect_equal(dim(s_an), dim(s_fd))
  expect_lt(max(abs(s_an - s_fd) / pmax(abs(s_fd), 1)), 1e-5)
})

test_that("WAIC comes off the same per-cluster expansion", {
  w <- waic(fit_rs)
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, res$n_units)
  expect_equal(w$type, "loco")
  expect_true(is.finite(w$estimates["elpd_waic", "Estimate"]))
  expect_true(is.finite(w$estimates["p_waic", "Estimate"]))
})

test_that("a units subset scores the same clusters as the full run", {
  res5 <- loo(fit_rs, units = 1:5)
  expect_equal(res5$n_units, 5L)
  expect_equal(res5$per_unit$unit, 1:5)
  expect_equal(res5$per_unit$l_star, res$per_unit$l_star[1:5])
  expect_equal(res5$per_unit$nobs, res$per_unit$nobs[1:5])
})

test_that("leave-one-unit-out remains refused for random slopes", {
  expect_error(loo(fit_rs, type = "loso"), class = "inlavaan_rs_loso")
})

test_that("a level-2 covariance is carried through the chain rule", {
  # `fb ~~ s1` adds a correlation parameter, whose theta -> x map has an
  # off-diagonal Jacobian block (cache$jcb_mat) that the plain diagonal
  # chain rule would miss
  fit_cov <- asem(
    paste0(mod_rs, "    fb ~~ s1\n"),
    d_rs,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  int_cov <- get_inlavaan_internal(fit_cov)
  spec_cov <- INLAvaan:::rs_spec(int_cov)
  cache <- INLAvaan:::loo_grad_cache(
    int_cov$theta_star,
    int_cov$lavmodel,
    int_cov$partable,
    two_level = TRUE
  )
  expect_gt(nrow(cache$jcb_mat), 0L)

  res_cov <- loo(fit_cov)
  expect_equal(res_cov$n_units, 24L)
  expect_true(is.finite(res_cov$estimates["elpd_loo", "Estimate"]))
  expect_equal(
    sum(res_cov$per_unit$l_star),
    rs_model_loglik(fit_cov),
    tolerance = 1e-6
  )

  units <- 1:3
  s_an <- INLAvaan:::loco_rs_scores_theta(
    int_cov$theta_star,
    spec_cov$rs,
    int_cov$lavmodel,
    int_cov$partable,
    units
  )
  h <- 1e-5
  s_fd <- vapply(
    seq_along(int_cov$theta_star),
    function(k) {
      th_up <- th_dn <- int_cov$theta_star
      th_up[k] <- th_up[k] + h
      th_dn[k] <- th_dn[k] - h
      (INLAvaan:::loco_rs_loglik_all(
        th_up,
        spec_cov$rs,
        int_cov$lavmodel,
        int_cov$partable,
        units
      ) -
        INLAvaan:::loco_rs_loglik_all(
          th_dn,
          spec_cov$rs,
          int_cov$lavmodel,
          int_cov$partable,
          units
        )) /
        (2 * h)
    },
    numeric(length(units))
  )
  expect_lt(max(abs(s_an - s_fd) / pmax(abs(s_fd), 1)), 1e-5)
})

test_that("incomplete data goes through lavaan's own missing handling", {
  # The random-slope kernels take FIML themselves, so this fit must not be
  # routed to the sufficient-statistic missing-data branch
  set.seed(4321)
  d_mis <- d_rs
  for (v in c("y1", "y2", "y3")) {
    d_mis[[v]][sample(nrow(d_mis), round(0.05 * nrow(d_mis)))] <- NA
  }
  fit_mis <- asem(
    mod_rs,
    d_mis,
    cluster = "cluster",
    missing = "ml",
    verbose = FALSE,
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  expect_true(get_inlavaan_internal(fit_mis)$lavsamplestats@missing.flag)

  res_mis <- loo(fit_mis)
  expect_equal(res_mis$type, "loco")
  expect_equal(res_mis$flavour, "conditional")
  expect_equal(res_mis$n_units, 24L)
  expect_true(is.finite(res_mis$estimates["elpd_loo", "Estimate"]))
  expect_equal(
    sum(res_mis$per_unit$l_star),
    rs_model_loglik(fit_mis),
    tolerance = 1e-6
  )
})

test_that("a full test set computes LOO and WAIC but still skips PPP", {
  expect_warning(
    fit_full <- asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "full",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    ),
    class = "inlavaan_rs_ppp"
  )
  int_full <- get_inlavaan_internal(fit_full)
  expect_true(is.finite(int_full$DIC$dic))
  expect_s3_class(int_full$loo, "inlavaan_loo")
  expect_s3_class(int_full$waic, "inlavaan_waic")
  expect_setequal(int_full$test$computed, c("dic", "loo", "waic"))
  expect_true("ppp" %in% names(int_full$test$skipped))
  expect_false("loo" %in% names(int_full$test$skipped))
  expect_equal(int_full$loo$n_units, 24L)
})
