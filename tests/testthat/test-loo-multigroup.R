# Multigroup LOSO: groups are independent, so every single-group kernel
# applies blockwise with group-indexed moments and constants. Units are
# identified by case index, so they keep their identity across fits that
# stack groups differently. The data are shuffled so the group stack does
# not coincide with the data order, exercising the id bookkeeping.

HS_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
set.seed(421)
dat_mg <- lavaan::HolzingerSwineford1939[
  sample(nrow(lavaan::HolzingerSwineford1939)),
]
rownames(dat_mg) <- NULL

fit_args <- list(
  meanstructure = TRUE,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
fit_conf <- do.call(
  acfa,
  c(list(HS_model, dat_mg, group = "school"), fit_args)
)
res_conf <- loo(fit_conf)

# Total loglik of the fit at the INLAvaan mode (conditional under fixed.x)
ll_at_mode <- function(fit) {
  int <- get_inlavaan_internal(fit)
  K <- if (isTRUE(int$lavmodel@ceq.simple.only)) {
    int$lavmodel@ceq.simple.K
  } else {
    NULL
  }
  theta_unp <- if (is.null(K)) {
    int$theta_star
  } else {
    as.numeric(K %*% int$theta_star)
  }
  lm_x <- lavaan::lav_model_set_parameters(
    int$lavmodel,
    INLAvaan:::pars_to_x(theta_unp, int$partable)
  )
  opts <- fit@Options
  opts$estimator <- "ML"
  INLAvaan:::lavaan___lav_model_loglik(
    lavdata = int$lavdata,
    lavsamplestats = int$lavsamplestats,
    lavimplied = lavaan::lav_model_implied(lm_x),
    lavmodel = lm_x,
    lavoptions = opts
  )$loglik
}

test_that("multigroup LOSO structure: case ids, groups, and identities", {
  expect_s3_class(res_conf, "inlavaan_loo")
  expect_equal(res_conf$type, "loso")
  expect_equal(res_conf$n_units, 301L)
  expect_equal(res_conf$n_groups, 2L)
  expect_named(
    res_conf$per_unit,
    c(
      "unit",
      "group",
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
  expect_true(all(res_conf$per_unit$ok))

  # Units are the case indices of the group-stacked rows, with labels
  int <- get_inlavaan_internal(fit_conf)
  expect_equal(
    res_conf$per_unit$unit,
    unlist(int$lavdata@case.idx)
  )
  expect_equal(
    res_conf$per_unit$group,
    rep(
      int$lavdata@group.label,
      times = vapply(int$lavdata@X, nrow, integer(1))
    )
  )

  # Unit logliks sum to the model loglik at the mode
  expect_equal(
    sum(res_conf$per_unit$l_star),
    ll_at_mode(fit_conf),
    tolerance = 1e-6
  )

  expect_output(print(res_conf), "in 2 groups")
})

test_that("unit subsetting by case id preserves order across groups", {
  ids <- res_conf$per_unit$unit
  pick <- c(ids[200L], ids[3L], ids[157L])
  res_sub <- loo(fit_conf, units = pick)
  expect_equal(res_sub$per_unit$unit, pick)
  expect_equal(
    res_sub$per_unit$log_cpo_2,
    res_conf$per_unit$log_cpo_2[match(pick, ids)],
    tolerance = 1e-8
  )
  expect_error(loo(fit_conf, units = c(1L, 1L)), "distinct")
  expect_error(loo(fit_conf, units = 0L), "distinct")
})

test_that("group-2 unit scores match numerical derivatives", {
  int <- get_inlavaan_internal(fit_conf)
  Y2 <- int$lavdata@X[[2L]][7L, , drop = FALSE]
  s_an <- as.numeric(INLAvaan:::loso_scores_theta(
    int$theta_star,
    Y2,
    int$lavmodel,
    int$partable,
    group = 2L
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
      (INLAvaan:::loso_loglik_all(Y2, cp$mom[[2L]]) -
        INLAvaan:::loso_loglik_all(Y2, cm$mom[[2L]])) /
        (2 * h)
    },
    numeric(1)
  )
  expect_equal(s_an, g_num, tolerance = 1e-5)
})

test_that("configural fit equals independent per-group fits", {
  # With no cross-group constraints the posterior factorises over groups,
  # so the multigroup CPOs must reproduce those of separate fits on each
  # group's data (up to independent-optimisation numerics)
  res_sep <- lapply(unique(dat_mg$school), function(s) {
    fit_g <- do.call(
      acfa,
      c(list(HS_model, dat_mg[dat_mg$school == s, ]), fit_args)
    )
    list(ids = which(dat_mg$school == s), loo = loo(fit_g))
  })
  cpo_sep <- unlist(lapply(res_sep, function(r) r$loo$per_unit$log_cpo_2))
  ids_sep <- unlist(lapply(res_sep, function(r) r$ids))
  m <- match(res_conf$per_unit$unit, ids_sep)
  expect_equal(res_conf$per_unit$log_cpo_2, cpo_sep[m], tolerance = 1e-3)
  expect_equal(
    res_conf$elpd_2,
    sum(vapply(res_sep, function(r) r$loo$elpd_2, numeric(1))),
    tolerance = 1e-5
  )
})

test_that("marginalised means: per-group case-deletion conditionals", {
  fit0 <- do.call(
    acfa,
    c(
      list(HS_model, dat_mg, group = "school"),
      modifyList(fit_args, list(meanstructure = FALSE))
    )
  )
  res0 <- loo(fit0)
  expect_true(all(res0$per_unit$ok))

  # Closed form: l_i = log phi(y_i; ybar_{g,-i}, c_g * Sigma_g) with the
  # downdated mean of the unit's own group
  int <- get_inlavaan_internal(fit0)
  mom <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable
  )$mom
  ldmvn <- function(y, mu, S) {
    ch <- chol(S)
    d <- backsolve(ch, y - mu, transpose = TRUE)
    -0.5 * (length(y) * log(2 * pi) + 2 * sum(log(diag(ch))) + sum(d^2))
  }
  for (g in 1:2) {
    Y <- int$lavdata@X[[g]]
    n <- nrow(Y)
    cc <- n / (n - 1)
    ybar <- colMeans(Y)
    ids <- int$lavdata@case.idx[[g]]
    ii <- c(1L, n %/% 2L, n)
    l_closed <- vapply(
      ii,
      function(i) {
        ybar_m <- (n * ybar - Y[i, ]) / (n - 1)
        ldmvn(Y[i, ], ybar_m, cc * mom[[g]]$Sigma)
      },
      numeric(1)
    )
    expect_equal(
      res0$per_unit$l_star[match(ids[ii], res0$per_unit$unit)],
      l_closed,
      tolerance = 1e-10
    )
  }
})

test_that("cross-group equality constraints flow through the packed space", {
  fit_metr <- do.call(
    acfa,
    c(
      list(HS_model, dat_mg, group = "school", group.equal = "loadings"),
      fit_args
    )
  )
  int <- get_inlavaan_internal(fit_metr)
  expect_true(int$lavmodel@ceq.simple.only)

  res_metr <- loo(fit_metr)
  expect_true(all(res_metr$per_unit$ok))
  expect_equal(
    sum(res_metr$per_unit$l_star),
    ll_at_mode(fit_metr),
    tolerance = 1e-6
  )

  # The MI ladder comparison: identical units pair the fits pointwise
  cmp <- compare(fit_conf, fit_metr, loo = TRUE)
  expect_true(all(c("ELPD", "SE", "elpd_diff", "se_diff") %in% names(cmp)))
  expect_true(all(is.finite(cmp$se_diff)))
})

test_that("pooled and multigroup fits pair by unit id in compare()", {
  # The pooled fit scores units in data order, the multigroup fit in
  # group-stacked order; pairing must align them by case id
  fit_pool <- do.call(acfa, c(list(HS_model, dat_mg), fit_args))
  res_pool <- loo(fit_pool)
  expect_false(identical(res_pool$per_unit$unit, res_conf$per_unit$unit))
  expect_setequal(res_pool$per_unit$unit, res_conf$per_unit$unit)

  cmp <- compare(fit_pool, fit_conf, loo = TRUE)
  expect_true(all(is.finite(cmp$se_diff)))

  # Paired SE agrees with a manual alignment by case id
  d <- res_pool$per_unit$log_cpo_2[
    match(res_conf$per_unit$unit, res_pool$per_unit$unit)
  ] -
    res_conf$per_unit$log_cpo_2
  se_manual <- sqrt(length(d) * var(d))
  k <- which(cmp$se_diff > 0)
  expect_equal(cmp$se_diff[k], se_manual, tolerance = 1e-3)
})

test_that("conditional flavour scores fixed.x multigroup fits", {
  mod_x <- "
    visual  =~ x1 + x2 + x3
    visual ~ ageyr + grade
  "
  fit_x <- do.call(
    asem,
    c(list(mod_x, dat_mg, group = "school", fixed.x = TRUE), fit_args)
  )
  res_x <- loo(fit_x)
  expect_equal(res_x$flavour, "conditional")
  # a couple of influential rows fall back to first order by design
  expect_gte(res_x$n_ok, ceiling(0.9 * res_x$n_units))
  expect_true(all(is.finite(res_x$per_unit$log_cpo_1)))
  expect_true(all(is.finite(res_x$per_unit$log_cpo_2[res_x$per_unit$ok])))
  # the NA grade row is listwise-deleted, so its case id is not a unit
  expect_equal(res_x$n_units, 300L)
  expect_equal(
    sum(res_x$per_unit$l_star),
    ll_at_mode(fit_x),
    tolerance = 1e-6
  )
})

test_that("waic() supports multigroup fits and agrees with loo()", {
  set.seed(2)
  w <- suppressWarnings(waic(fit_conf, nsamp = 300))
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, 301L)
  expect_equal(w$n_groups, 2L)
  expect_true("group" %in% names(w$per_unit))
  expect_true(all(is.finite(w$per_unit$lpd)))
  expect_output(print(w), "in 2 groups")
  expect_equal(
    unname(w$estimates["elpd_waic", "Estimate"]),
    res_conf$elpd_2,
    tolerance = 0.005
  )
})
