HS_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

fit <- acfa(
  HS_model,
  lavaan::HolzingerSwineford1939,
  meanstructure = TRUE,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
res <- loo(fit)

test_that("LOSO matches reference values", {
  # Reference values computed with an independent implementation of the same
  # Taylor LOO formulas on this exact fit
  expect_equal(res$type, "loso")
  expect_equal(res$n_units, 301L)
  expect_equal(res$elpd_1, -3753.9827314325, tolerance = 1e-4)
  expect_equal(res$elpd_2, -3769.8932098595, tolerance = 1e-4)
  expect_equal(res$se_1, 43.2298484428, tolerance = 1e-4)
  expect_equal(res$se_2, 43.4126638097, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 32.4399589132, tolerance = 1e-4)
  expect_equal(res$p_loo_2, 33.6076795556, tolerance = 1e-4)

  pu <- res$per_unit[c(1L, 150L, 301L), ]
  expect_equal(
    pu$l_star,
    c(-17.2934204992, -13.4493341972, -11.4168729139),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-17.4103164657, -13.4961506289, -11.4422442947),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-17.4614494940, -13.5529791063, -11.4698745368),
    tolerance = 1e-4
  )
  expect_equal(
    pu$det_term,
    c(-0.0494450472080, -0.0561824850593, -0.0273618609405),
    tolerance = 1e-3
  )
})

test_that("loo object structure and internal identities", {
  expect_s3_class(res, "inlavaan_loo")
  expect_named(
    res$per_unit,
    c(
      "unit",
      "nobs",
      "l_star",
      "score_norm",
      "lpd_1",
      "lpd_2",
      "log_cpo_1",
      "log_cpo_2",
      "det_term",
      "ok"
    )
  )
  expect_true(all(res$per_unit$ok))
  expect_true(all(res$per_unit$nobs == 1L))

  # First-order CPO and LPD are symmetric about l_star by construction
  expect_equal(
    res$per_unit$lpd_1 + res$per_unit$log_cpo_1,
    2 * res$per_unit$l_star
  )
  expect_equal(
    unname(res$estimates["elpd_loo", "Estimate"]),
    res$elpd_2
  )
  expect_equal(
    unname(res$estimates["looic", "Estimate"]),
    -2 * res$elpd_2
  )
  expect_equal(
    unname(res$estimates["looic", "SE"]),
    2 * res$se_2
  )

  expect_output(print(res), "leave-one-subject-out")
  expect_output(print(res), "elpd_loo")
})

test_that("sum of unit logliks equals the model loglik at the mode", {
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

test_that("first-order only and unit subsetting", {
  res1 <- loo(fit, second_order = FALSE)
  expect_true(all(is.na(res1$per_unit$log_cpo_2)))
  expect_equal(res1$elpd_1, res$elpd_1)
  expect_equal(
    unname(res1$estimates["elpd_loo", "Estimate"]),
    res1$elpd_1
  )

  res25 <- loo(fit, units = 1:25)
  expect_equal(nrow(res25$per_unit), 25L)
  expect_equal(
    res25$per_unit$log_cpo_2,
    res$per_unit$log_cpo_2[1:25],
    tolerance = 1e-8
  )

  expect_error(loo(fit, units = c(1L, 1L)), "distinct")
  expect_error(loo(fit, units = 0L), "distinct")
})

test_that("theta/Sigma override scores arbitrary summaries", {
  int <- get_inlavaan_internal(fit)
  res_same <- loo(fit, theta = int$theta_star, Sigma = int$Sigma_theta)
  expect_true(res_same$theta_overridden)
  expect_equal(res_same$elpd_2, res$elpd_2)

  res_pert <- loo(fit, theta = int$theta_star * 1.01)
  expect_false(isTRUE(all.equal(res_pert$elpd_2, res$elpd_2)))

  # Conditioning a parameter to zero gives a singular Sigma; the active
  # block restriction handles it
  p <- 1L
  theta_c <- int$theta_star -
    int$Sigma_theta[, p] * (int$theta_star[p] / int$Sigma_theta[p, p])
  Sigma_c <- int$Sigma_theta -
    tcrossprod(int$Sigma_theta[, p]) / int$Sigma_theta[p, p]
  res_cond <- loo(fit, theta = theta_c, Sigma = Sigma_c, units = 1:10)
  expect_true(all(is.finite(res_cond$per_unit$log_cpo_1)))

  expect_error(loo(fit, theta = 1:3), "length")
  expect_error(loo(fit, Sigma = diag(3)), "covariance")
})

test_that("type override and parallel agree with serial", {
  expect_error(loo(fit, type = "loco"), "two-level")

  res_ser <- loo(fit, units = 1:10)
  res_par <- loo(fit, units = 1:10, cores = 2L)
  expect_equal(
    res_par$per_unit$log_cpo_2,
    res_ser$per_unit$log_cpo_2,
    tolerance = 1e-10
  )
})

test_that("equality constraints (ceq.simple) are handled", {
  hs_eq <- "
    visual  =~ x1 + a*x2 + a*x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  fit_eq <- acfa(
    hs_eq,
    lavaan::HolzingerSwineford1939,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  int <- get_inlavaan_internal(fit_eq)
  expect_true(int$lavmodel@ceq.simple.only)

  res_eq <- loo(fit_eq)
  expect_true(all(res_eq$per_unit$ok))

  # Total loglik consistency in the packed parameter space
  theta_unp <- as.numeric(int$lavmodel@ceq.simple.K %*% int$theta_star)
  x <- INLAvaan:::pars_to_x(theta_unp, int$partable)
  lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
  opts <- fit_eq@Options
  opts$estimator <- "ML"
  ll <- INLAvaan:::lavaan___lav_model_loglik(
    lavdata = int$lavdata,
    lavsamplestats = int$lavsamplestats,
    lavimplied = lavaan::lav_model_implied(lm_x),
    lavmodel = lm_x,
    lavoptions = opts
  )$loglik
  expect_equal(sum(res_eq$per_unit$l_star), ll, tolerance = 1e-6)

  # Analytic unit score matches a numerical derivative in packed theta space
  Y1 <- int$lavdata@X[[1L]][1L, , drop = FALSE]
  s1 <- as.numeric(INLAvaan:::loso_scores_theta(
    int$theta_star,
    Y1,
    int$lavmodel,
    int$partable
  ))
  h <- 1e-6
  g_num <- vapply(
    seq_along(int$theta_star),
    function(k) {
      tp <- tm <- int$theta_star
      tp[k] <- tp[k] + h
      tm[k] <- tm[k] - h
      cp <- INLAvaan:::loo_grad_cache(tp, int$lavmodel, int$partable)
      cm <- INLAvaan:::loo_grad_cache(tm, int$lavmodel, int$partable)
      (INLAvaan:::loso_loglik_all(Y1, cp$mom[[1L]]) -
        INLAvaan:::loso_loglik_all(Y1, cm$mom[[1L]])) /
        (2 * h)
    },
    numeric(1)
  )
  expect_equal(s1, g_num, tolerance = 1e-5)
})

test_that("fit-time LOO via test = 'loo' and add_loo()", {
  fit_loo <- acfa(
    HS_model,
    lavaan::HolzingerSwineford1939,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "loo",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  stored <- get_inlavaan_internal(fit_loo, "loo")
  expect_s3_class(stored, "inlavaan_loo")
  expect_equal(stored$elpd_2, res$elpd_2, tolerance = 1e-10)

  # loo() returns the stored result for default arguments only
  expect_identical(loo(fit_loo), stored)
  res_sub <- loo(fit_loo, units = 1:5)
  expect_equal(nrow(res_sub$per_unit), 5L)

  # add_loo() returns an updated copy; the original fit is unchanged
  fit2 <- add_loo(fit)
  expect_null(fit@external$inlavaan_internal$loo)
  expect_s3_class(get_inlavaan_internal(fit2, "loo"), "inlavaan_loo")
  expect_equal(get_inlavaan_internal(fit2, "loo")$elpd_2, res$elpd_2)
  expect_identical(loo(fit2), get_inlavaan_internal(fit2, "loo"))
})

test_that("fitMeasures reports LOO measures on request or when stored", {
  # Not stored: bare fitMeasures() excludes the LOO measures
  fm_all <- fitMeasures(fit)
  expect_false(any(
    c("elpd_loo", "se_loo", "p_loo", "looic") %in%
      names(fm_all)
  ))

  # On request by name: computed on demand, agreeing with loo()
  fm <- fitMeasures(fit, c("elpd_loo", "se_loo", "p_loo", "looic"))
  expect_equal(unname(fm["elpd_loo"]), res$elpd_2, tolerance = 1e-10)
  expect_equal(unname(fm["looic"]), -2 * res$elpd_2, tolerance = 1e-10)
  expect_equal(unname(fm["se_loo"]), 2 * res$se_2, tolerance = 1e-10)
  expect_equal(unname(fm["p_loo"]), res$p_loo_2, tolerance = 1e-10)

  # Stored: included in "all" for free
  fit2 <- add_loo(fit)
  fm2 <- fitMeasures(fit2)
  expect_true(all(c("elpd_loo", "se_loo", "p_loo", "looic") %in% names(fm2)))
  expect_equal(unname(fm2["elpd_loo"]), res$elpd_2, tolerance = 1e-10)
})

test_that("waic() sanity and agreement with loo()", {
  set.seed(123)
  # A few units genuinely exceed the p_waic reliability threshold here
  expect_warning(w <- waic(fit, nsamp = 1000), "p_waic")
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, 301L)
  expect_equal(w$type, "loso")
  expect_true(all(is.finite(w$per_unit$lpd)))
  expect_true(all(w$per_unit$p_waic > 0))
  expect_output(print(w), "WAIC")

  # WAIC and LOO estimate the same quantity; loose agreement on this model
  expect_equal(
    unname(w$estimates["elpd_waic", "Estimate"]),
    res$elpd_2,
    tolerance = 0.005
  )
  expect_equal(
    unname(w$estimates["waic", "Estimate"]),
    -2 * unname(w$estimates["elpd_waic", "Estimate"])
  )

  # fitMeasures computes WAIC on request by name only (the test fit has
  # nsamp = 3, so suppress the small-sample p_waic reliability warning)
  expect_false("waic" %in% names(fitMeasures(fit)))
  suppressWarnings(
    fm <- fitMeasures(fit, c("waic", "p_waic", "se_waic"))
  )
  expect_true(all(c("waic", "p_waic", "se_waic") %in% names(fm)))
})

test_that("single-level FIML is supported (see test-loo-missing.R)", {
  d_miss <- lavaan::HolzingerSwineford1939
  d_miss[1, "x1"] <- NA
  fit_miss <- acfa(
    HS_model,
    d_miss,
    meanstructure = TRUE,
    missing = "ml",
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  res_miss <- loo(fit_miss)
  expect_s3_class(res_miss, "inlavaan_loo")
  expect_equal(res_miss$flavour, "joint")
})

test_that("test = 'standard' stores LOO and WAIC when supported and cheap", {
  fit_std <- acfa(
    HS_model,
    lavaan::HolzingerSwineford1939,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 150,
    test = "standard",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  int <- get_inlavaan_internal(fit_std)

  expect_s3_class(int$loo, "inlavaan_loo")
  expect_equal(int$loo$n_units, 301L)
  expect_equal(int$loo$elpd_2, res$elpd_2, tolerance = 1e-10)
  expect_identical(loo(fit_std), int$loo)

  expect_s3_class(int$waic, "inlavaan_waic")
  expect_equal(int$waic$nsamp, 150L)
  expect_identical(waic(fit_std), int$waic)
  # non-default arguments still trigger a fresh computation
  set.seed(1)
  w2 <- suppressWarnings(waic(fit_std, nsamp = 120))
  expect_equal(w2$nsamp, 120L)

  # stored results appear in fitMeasures' "all" for free
  fm <- fitMeasures(fit_std)
  expect_true(all(
    c("elpd_loo", "looic", "waic", "p_waic", "se_waic") %in% names(fm)
  ))
  expect_true(all(c("ppp", "dic", "p_dic") %in% names(fm)))
})

test_that("the fit-time budget gate aborts with its own condition class", {
  int <- get_inlavaan_internal(fit)
  expect_error(
    INLAvaan:::inlav_loo(int, max_seconds = 1e-9),
    class = "inlavaan_loo_budget"
  )
})

test_that("small nsamp skips fit-time WAIC but not LOO", {
  fit_s3 <- acfa(
    HS_model,
    lavaan::HolzingerSwineford1939,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "standard",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  int <- get_inlavaan_internal(fit_s3)
  expect_null(int$waic)
  expect_s3_class(int$loo, "inlavaan_loo")
})
