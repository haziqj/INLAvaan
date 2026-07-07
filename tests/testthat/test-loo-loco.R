twolevel_model <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"

# Shrunk two-level fixture: first 24 cluster ids (6 full cycles of the
# 5/10/15/20 cluster-size pattern that repeats every 4 cluster ids in
# lavaan::Demo.twolevel), 300 rows total. Shared across both the main
# `fit` below and `fit_fx` (the fixed.x = TRUE variant further down).
d_sub <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]

fit <- asem(
  twolevel_model,
  d_sub,
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
  expect_equal(res$n_units, 24L)
  expect_equal(res$elpd_1, -2811.3389254026, tolerance = 1e-4)
  expect_equal(res$elpd_2, -2832.9728115262, tolerance = 1e-4)
  expect_equal(res$se_1, 259.1529664233, tolerance = 1e-4)
  expect_equal(res$se_2, 260.9893088931, tolerance = 1e-4)
  # p_loo aggregates small differences of nearly-equal numbers, so it is
  # more sensitive to BLAS/optimiser endpoint noise than the elpd totals
  expect_equal(res$p_loo_1, 30.7792227732, tolerance = 1e-2)
  expect_equal(res$p_loo_2, 35.1657403910, tolerance = 1e-2)

  # cluster ids 1, 6, 8 have sizes 5, 10, 20 respectively under the
  # 5/10/15/20 cycle
  pu <- res$per_unit[c(1L, 6L, 8L), ]
  expect_equal(pu$nobs, c(5L, 10L, 20L))
  expect_equal(
    pu$l_star,
    c(-45.1868700825, -91.3569642108, -177.6891056793),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-45.3396350528, -91.5858133174, -178.4008589303),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-45.5376421501, -92.0469159588, -179.5548931984),
    tolerance = 1e-4
  )
  # det_term values are tiny finite-difference remainders whose relative
  # error is dominated by cross-platform noise; their correctness is pinned
  # through log_cpo_2 above, so only check structure here
  expect_true(all(is.finite(pu$det_term)))
  expect_true(all(pu$det_term < 0))
})

test_that("LOCO structure and internal identities", {
  expect_s3_class(res, "inlavaan_loo")
  expect_true(all(res$per_unit$ok))
  expect_equal(sum(res$per_unit$nobs), nrow(d_sub))
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
    "leave-one-unit-out"
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

test_that("fixed.x two-level fits are scored conditionally", {
  fit_fx <- asem(
    twolevel_model,
    d_sub,
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
  res_fx <- loo(fit_fx, units = 1:5)
  expect_equal(res_fx$flavour, "conditional")
  expect_true(all(is.finite(res_fx$per_unit$log_cpo_2)))
})

test_that("waic gains type: conditional (leave-one-unit-out) WAIC", {
  # default is marginal (per-cluster) WAIC
  set.seed(1)
  w_marg <- suppressWarnings(waic(fit, nsamp = 120))
  expect_equal(w_marg$type, "loco")

  # type = "loso" warns and scores the conditional (leave-one-unit-out) WAIC,
  # the same estimand as loo(type = "loso"); the two routes agree loosely
  w_cond <- testthat::capture_warnings(
    w <- waic(fit, type = "loso", units = 1:5, nsamp = 120)
  )
  expect_true(any(grepl("leave-one-unit-out", w_cond)))
  expect_equal(w$type, "loso")
  expect_equal(w$n_units, 5L)
  expect_true(all(w$per_unit$nobs == 1L))

  l <- suppressWarnings(loo(fit, type = "loso", units = 1:5))
  expect_equal(
    unname(w$estimates["elpd_waic", "Estimate"]),
    l$elpd_2,
    tolerance = 0.05
  )
})
