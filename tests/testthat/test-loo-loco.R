twolevel_model <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"

fit <- asem(
  twolevel_model,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  meanstructure = TRUE,
  fixed.x = FALSE,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
res <- loo(fit)

test_that("LOCO matches reference values", {
  # Reference values computed with an independent implementation of the same
  # Taylor LOO formulas on this exact fit
  expect_equal(res$type, "loco")
  expect_equal(res$n_units, 200L)
  expect_equal(res$elpd_1, -23326.8621243898, tolerance = 1e-4)
  expect_equal(res$elpd_2, -23344.5513534494, tolerance = 1e-4)
  expect_equal(res$se_1, 731.0451908607, tolerance = 1e-4)
  expect_equal(res$se_2, 731.4621700642, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 33.8733495508, tolerance = 1e-4)
  expect_equal(res$p_loo_2, 34.8487202421, tolerance = 1e-4)

  pu <- res$per_unit[c(1L, 50L, 200L), ]
  expect_equal(pu$nobs, c(5L, 10L, 20L))
  expect_equal(
    pu$l_star,
    c(-45.4095378095, -95.2988814936, -189.5348571672),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-45.4309594960, -95.3467774741, -189.6552767982),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-45.4376155763, -95.4125424934, -189.8302808479),
    tolerance = 1e-4
  )
  expect_equal(
    pu$det_term,
    c(-0.0066567054683, -0.0652632247016, -0.1708177137535),
    tolerance = 1e-3
  )
})

test_that("LOCO structure and internal identities", {
  expect_s3_class(res, "inlavaan_loo")
  expect_true(all(res$per_unit$ok))
  expect_equal(sum(res$per_unit$nobs), nrow(lavaan::Demo.twolevel))
  expect_equal(
    res$per_unit$lpd_1 + res$per_unit$log_cpo_1,
    2 * res$per_unit$l_star
  )
  expect_output(print(res), "leave-one-cluster-out")

  # Sum of cluster logliks equals the model loglik at the mode
  int <- get_inlavaan_internal(fit)
  x <- INLAvaan:::pars_to_x(int$theta_star, int$partable)
  lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
  opts <- fit@Options
  opts$estimator <- "ML"
  ll <- INLAvaan:::lavaan___lav_model_loglik(
    lavdata = int$lavdata,
    lavsamplestats = int$lavsamplestats,
    lavimplied = lavaan::lav_model_implied(lm_x),
    lavmodel = lm_x,
    lavoptions = opts
  )$loglik
  expect_equal(sum(res$per_unit$l_star), ll, tolerance = 1e-6)
})

test_that("LOCO unit subsetting and theta/Sigma override", {
  sub <- c(3L, 7L, 11L)
  res_sub <- loo(fit, units = sub)
  expect_equal(res_sub$per_unit$unit, sub)
  expect_equal(
    res_sub$per_unit$log_cpo_2,
    res$per_unit$log_cpo_2[sub],
    tolerance = 1e-8
  )

  int <- get_inlavaan_internal(fit)
  res_same <- loo(
    fit,
    theta = int$theta_star,
    Sigma = int$Sigma_theta,
    units = sub
  )
  expect_true(res_same$theta_overridden)
  expect_equal(
    res_same$per_unit$log_cpo_2,
    res$per_unit$log_cpo_2[sub],
    tolerance = 1e-8
  )
})

test_that("two-level LOSO override scores row deletions", {
  expect_warning(
    res_row <- loo(fit, type = "loso", units = 1:8),
    "per-row deletion"
  )
  expect_equal(res_row$type, "loso")
  expect_equal(nrow(res_row$per_unit), 8L)
  expect_true(all(res_row$per_unit$ok))
  expect_true(all(res_row$per_unit$nobs == 1L))

  int <- get_inlavaan_internal(fit)
  css <- INLAvaan:::loco_suff_stats(int$lavdata)
  X <- int$lavdata@X[[1L]]
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )

  # The downdated sufficient statistics agree with rebuilding the
  # cluster-minus-row statistics from the raw data
  i <- 2L
  j <- css$cluster_idx[i]
  rows <- setdiff(which(css$cluster_idx == j), i)
  Yj <- X[rows, , drop = FALSE]
  us_raw <- INLAvaan:::loco_stats_build(
    length(rows),
    crossprod(sweep(Yj, 2L, colMeans(Yj), "-")),
    colMeans(Yj)[css$zy_idx],
    css
  )
  ll_minus_raw <- INLAvaan:::loco_loglik_us(us_raw, cache$mom)
  ll_full <- INLAvaan:::loco_loglik_one(j, css, cache$mom)
  expect_equal(
    res_row$per_unit$l_star[i],
    ll_full - ll_minus_raw,
    tolerance = 1e-8
  )

  # Analytic row score matches a numerical derivative
  s2 <- as.numeric(INLAvaan:::loso2l_scores_theta(
    int$theta_star,
    css,
    X,
    int$lavmodel,
    int$partable,
    units = i
  ))
  h <- 1e-6
  g_num <- vapply(
    seq_along(int$theta_star),
    function(k) {
      tp <- tm <- int$theta_star
      tp[k] <- tp[k] + h
      tm[k] <- tm[k] - h
      cp <- INLAvaan:::loo_grad_cache(
        tp,
        int$lavmodel,
        int$partable,
        two_level = TRUE
      )
      cm <- INLAvaan:::loo_grad_cache(
        tm,
        int$lavmodel,
        int$partable,
        two_level = TRUE
      )
      (INLAvaan:::loso2l_loglik_all(i, css, X, cp$mom) -
        INLAvaan:::loso2l_loglik_all(i, css, X, cm$mom)) /
        (2 * h)
    },
    numeric(1)
  )
  expect_equal(s2, g_num, tolerance = 1e-5)
})

test_that("fixed.x two-level fits abort with refit advice", {
  fit_fx <- asem(
    twolevel_model,
    lavaan::Demo.twolevel,
    cluster = "cluster",
    meanstructure = TRUE,
    fixed.x = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  expect_error(loo(fit_fx), "fixed.x = FALSE")
})
