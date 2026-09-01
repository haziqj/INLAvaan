HS_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

# Small deterministic subset of HolzingerSwineford1939 shared by every acfa()
# call in this file, in place of the full 301-row dataset
set.seed(1)
dat <- lavaan::HolzingerSwineford1939[
  sample(nrow(lavaan::HolzingerSwineford1939), 40),
]

fit <- acfa(
  HS_model,
  dat,
  meanstructure = TRUE,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
res <- loo(fit)
# Shared across two test_that() blocks that both need a copy of `fit` with
# LOO stored via add_loo() -- historically each block called add_loo(fit)
# separately, but the call is deterministic so it is computed once here
fit_with_loo <- add_loo(fit)

test_that("LOSO matches reference values", {
  # Reference values computed with an independent implementation of the same
  # Taylor LOO formulas on this exact fit
  expect_equal(res$type, "loso")
  expect_equal(res$n_units, 40L)
  expect_equal(res$elpd_1, -488.6660523464, tolerance = 1e-4)
  expect_equal(res$elpd_2, -509.4810089914, tolerance = 1e-4)
  expect_equal(res$se_1, 14.9606326798, tolerance = 1e-4)
  expect_equal(res$se_2, 16.1424977191, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 28.1110170037, tolerance = 1e-4)
  # Every log CPO term exists here, but one unit has no second-order lpd, so
  # elpd_loo keeps its second order and that unit contributes its first-order
  # difference to p_loo
  expect_equal(res$n_ok, 40L)
  expect_equal(res$n_lpd_ok, 39L)
  expect_true(res$use_second)
  expect_equal(res$p_loo_2, 35.5963931926, tolerance = 1e-4)
  expect_equal(unname(res$estimates["elpd_loo", "Estimate"]), res$elpd_2)
  expect_equal(unname(res$estimates["p_loo", "Estimate"]), res$p_loo_2)
  # ... covering all 40 units, not the 39 with a second-order lpd
  pu_all <- res$per_unit
  has2 <- !is.na(pu_all$lpd_2)
  expect_equal(
    res$p_loo_2,
    sum((pu_all$lpd_2 - pu_all$log_cpo_2)[has2]) +
      sum((pu_all$lpd_1 - pu_all$log_cpo_1)[!has2])
  )

  pu <- res$per_unit[c(1L, 20L, 40L), ]
  expect_equal(
    pu$l_star,
    c(-10.1330813639, -10.7290547282, -12.8417007213),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-10.2877928268, -10.8713710491, -13.1844720013),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-10.6530882509, -11.2745912656, -13.5171746493),
    tolerance = 1e-4
  )
  expect_equal(
    pu$det_term,
    c(-0.3503871084, -0.3930981418, -0.2827972374),
    tolerance = 1e-3
  )
})

test_that("a substituted lpd unit is silent at the console", {
  # A missing lpd term is the ordinary state of an SEM fit, elpd_loo is
  # untouched, and the substituted first-order contribution is accurate to
  # within ~10% -- a smaller error than the second-order lpd's own bias on
  # the units that keep it, which is not announced either. Saying anything
  # here would misdirect and would drown the k_max >= 1 warning, which is the
  # rare and consequential one (test-loo-loco.R).
  expect_no_warning(loo(fit, cores = 1L))
  expect_no_message(loo(fit, cores = 1L))
  expect_no_warning(fitMeasures(fit_with_loo, "p_loo"))
  expect_no_message(fitMeasures(fit_with_loo, "p_loo"))
  # It is recorded where someone looking for it will find it
  expect_equal(res$n_lpd_ok, 39L)
  expect_output(print(res), "first-order contributions")
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
      "k_max",
      "k_min",
      "k_sum",
      "k_ssq",
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

  expect_output(print(res), "Leave-one-subject-out")
  expect_output(print(res), "elpd_loo")
})

test_that("sum of unit logliks equals the model loglik at the mode", {
  int <- get_inlavaan_internal(fit)
  x <- INLAvaan:::pars_to_x(int$theta_star, int$partable)
  lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
  opts <- fit@Options
  opts$estimator <- "ML"
  ll <- lavaan:::lav_model_loglik(
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

test_that("theta/Omega override scores arbitrary summaries", {
  int <- get_inlavaan_internal(fit)
  res_same <- loo(fit, theta = int$theta_star, Omega = int$Sigma_theta)
  expect_true(res_same$theta_overridden)
  expect_equal(res_same$elpd_2, res$elpd_2)

  res_pert <- loo(fit, theta = int$theta_star * 1.01)
  expect_false(isTRUE(all.equal(res_pert$elpd_2, res$elpd_2)))

  # Conditioning a parameter to zero gives a singular Omega; the active
  # block restriction handles it
  p <- 1L
  theta_c <- int$theta_star -
    int$Sigma_theta[, p] * (int$theta_star[p] / int$Sigma_theta[p, p])
  Omega_c <- int$Sigma_theta -
    tcrossprod(int$Sigma_theta[, p]) / int$Sigma_theta[p, p]
  res_cond <- loo(fit, theta = theta_c, Omega = Omega_c, units = 1:10)
  expect_true(all(is.finite(res_cond$per_unit$log_cpo_1)))

  expect_error(loo(fit, theta = 1:3), "length")
  expect_error(loo(fit, Omega = diag(3)), "covariance")
})

test_that("deprecated Sigma argument is honoured with a warning", {
  int <- get_inlavaan_internal(fit)
  expect_warning(
    res_dep <- loo(fit, Sigma = int$Sigma_theta, units = 1:10),
    class = "inlavaan_deprecated_sigma"
  )
  res_new <- loo(fit, Omega = int$Sigma_theta, units = 1:10)
  expect_equal(res_dep$estimates, res_new$estimates)

  expect_error(
    loo(fit, Omega = int$Sigma_theta, Sigma = int$Sigma_theta),
    "deprecated former name"
  )
})

test_that("type override and parallel agree with serial", {
  expect_error(loo(fit, type = "loco"), "two-level")

  res_ser <- loo(fit, units = 1:10)
  res_par <- loo(fit, units = 1:10, cores = 2L)
  # Parallel (fork-based mclapply) and serial paths are algorithmically
  # identical but not bit-reproducible: floating-point reduction order in the
  # per-unit inner numerics can differ, so agreement is checked to numerical
  # tolerance rather than exact equality (any real divergence is O(1)).
  expect_equal(
    res_par$per_unit$log_cpo_2,
    res_ser$per_unit$log_cpo_2,
    tolerance = 1e-3
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
    dat,
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
  ll <- lavaan:::lav_model_loglik(
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
    dat,
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
  fit2 <- fit_with_loo
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
  expect_equal(
    unname(fm["p_loo"]),
    unname(res$estimates["p_loo", "Estimate"]),
    tolerance = 1e-10
  )

  # Stored: included in "all" for free
  fit2 <- fit_with_loo
  fm2 <- fitMeasures(fit2)
  expect_true(all(c("elpd_loo", "se_loo", "p_loo", "looic") %in% names(fm2)))
  expect_equal(unname(fm2["elpd_loo"]), res$elpd_2, tolerance = 1e-10)
})

test_that("waic() sanity and structure", {
  # This fit has one unit whose second-order lpd does not exist (k_min <=
  # -1), which is the WAIC's only existence condition, so the whole result
  # falls to first order and warns
  expect_warning(w <- waic(fit), class = "inlavaan_waic_first_order")
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, 40L)
  expect_equal(w$type, "loso")
  expect_false(w$use_second)
  expect_equal(w$n_lpd_ok, 39L)
  expect_true(all(is.finite(w$per_unit$lpd)))
  expect_true(all(w$per_unit$p_waic > 0))
  expect_output(print(w), "first-order")

  # the fallback is exact, not merely lower-order: first-order WAIC IS the
  # first-order LOO score (lpd_1 - p_waic_1 = log_cpo_1 pointwise)
  expect_equal(
    unname(w$estimates["elpd_waic", "Estimate"]),
    res$elpd_1,
    tolerance = 1e-10
  )
  expect_equal(
    w$per_unit$elpd_waic,
    res$per_unit$log_cpo_1,
    tolerance = 1e-10
  )
  expect_equal(
    unname(w$estimates["waic", "Estimate"]),
    -2 * unname(w$estimates["elpd_waic", "Estimate"])
  )

  # requesting first order explicitly gives the same number, without a
  # warning: nothing had to be abandoned
  w1 <- waic(fit, second_order = FALSE)
  expect_equal(w1$estimates, w$estimates, tolerance = 1e-12)

  # deterministic: a recomputation reproduces the estimates exactly
  w_again <- suppressWarnings(waic(fit))
  expect_identical(w$estimates, w_again$estimates)

  # the deprecated draws argument is ignored, with a warning
  msgs <- testthat::capture_warnings(waic(fit, nsamp = 100))
  expect_true(any(grepl("nsamp", msgs)))

  # no p_waic threshold is applied any more: a large pointwise p_waic is
  # not itself a reason to warn
  expect_true(max(w$per_unit$p_waic) > 0.4)

  # fitMeasures computes WAIC on request by name only
  expect_false("waic" %in% names(fitMeasures(fit)))
  fm <- suppressWarnings(fitMeasures(fit, c("waic", "p_waic", "se_waic")))
  expect_true(all(c("waic", "p_waic", "se_waic") %in% names(fm)))
})

test_that("single-level FIML is supported (see test-loo-missing.R)", {
  d_miss <- dat
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
  # the stored WAIC falls to first order here (unit 5 has no second-order
  # lpd), which warns at fit time
  fit_std <- suppressWarnings(acfa(
    HS_model,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 100,
    test = "standard",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  ))
  int <- get_inlavaan_internal(fit_std)

  expect_s3_class(int$loo, "inlavaan_loo")
  expect_equal(int$loo$n_units, 40L)
  expect_equal(int$loo$elpd_2, res$elpd_2, tolerance = 1e-10)
  expect_identical(loo(fit_std), int$loo)

  expect_s3_class(int$waic, "inlavaan_waic")
  expect_identical(waic(fit_std), int$waic)
  # the stored WAIC is the fit-time LOO aggregated on the lpd side, at
  # whichever order every unit's lpd term supports
  pu <- int$loo$per_unit
  quad <- 2 * (pu$lpd_1 - pu$l_star)
  expected <- if (isTRUE(int$waic$use_second)) {
    sum(pu$lpd_2 - quad - 0.5 * pu$k_ssq)
  } else {
    sum(pu$lpd_1 - quad)
  }
  expect_equal(
    unname(int$waic$estimates["elpd_waic", "Estimate"]),
    expected,
    tolerance = 1e-10
  )
  # non-default arguments still trigger a fresh computation
  w2 <- suppressWarnings(waic(fit_std, units = 1:10))
  expect_equal(w2$n_units, 10L)

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

test_that("fit-time WAIC follows the fit-time LOO regardless of nsamp", {
  fit_s3 <- suppressWarnings(acfa(
    HS_model,
    dat,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "standard",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  ))
  int <- get_inlavaan_internal(fit_s3)
  # derived from the same Taylor pass, so no draws-based nsamp gate remains
  expect_s3_class(int$loo, "inlavaan_loo")
  expect_s3_class(int$waic, "inlavaan_waic")
})
