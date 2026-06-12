# Conditional-flavour LOO for fixed.x fits: units are scored by the
# predictive density of their outcomes given their covariates, matching the
# (conditional) likelihood the model was fitted with.

HS_model_x <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
  visual ~ ageyr + grade
  textual ~ ageyr + grade
"
dat_x <- na.omit(
  lavaan::HolzingerSwineford1939[, c(paste0("x", 1:9), "ageyr", "grade")]
)

fit_c <- asem(
  HS_model_x,
  dat_x,
  fixed.x = TRUE,
  meanstructure = TRUE,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
res_c <- loo(fit_c)

# Total loglik of the fit at the INLAvaan mode (under fixed.x this is the
# conditional loglik)
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

test_that("conditional LOSO on a fixed.x fit matches reference values", {
  expect_equal(res_c$flavour, "conditional")
  expect_equal(res_c$type, "loso")
  expect_equal(res_c$n_units, 300L)
  expect_equal(res_c$elpd_1, -3726.3893510145, tolerance = 1e-4)
  expect_equal(res_c$elpd_2, -3748.7773908743, tolerance = 1e-4)
  expect_equal(res_c$se_1, 44.8288480134, tolerance = 1e-3)
  expect_equal(res_c$se_2, 45.1238552339, tolerance = 1e-3)
  expect_equal(res_c$p_loo_2, 46.3226952205, tolerance = 1e-2)

  pu <- res_c$per_unit[c(1L, 150L, 300L), ]
  expect_equal(
    pu$l_star,
    c(-16.9619393030, -13.0422181149, -11.7933556389),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-17.1947718781, -13.1690136822, -11.8872052364),
    tolerance = 1e-4
  )

  expect_output(print(res_c), "conditionally on the exogenous covariates")
})

test_that("conditional unit logliks sum to the fitted likelihood", {
  expect_equal(sum(res_c$per_unit$l_star), ll_at_mode(fit_c), tolerance = 1e-6)
})

test_that("joint and conditional scales differ by the covariate predictive", {
  fit_j <- asem(
    HS_model_x,
    dat_x,
    fixed.x = FALSE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  res_j <- loo(fit_j)
  expect_equal(res_j$flavour, "joint")

  # Closed form: the leave-one-out predictive of the saturated covariate
  # block is multivariate Student-t with downdated moments
  X <- as.matrix(dat_x[, c("ageyr", "grade")])
  n <- nrow(X)
  q <- ncol(X)
  lmvt <- function(x, mu, Lam, nu) {
    k <- length(x)
    ch <- chol(Lam)
    d2 <- sum(backsolve(ch, x - mu, transpose = TRUE)^2)
    lgamma((nu + k) / 2) -
      lgamma(nu / 2) -
      (k / 2) * log(nu * pi) -
      sum(log(diag(ch))) -
      ((nu + k) / 2) * log1p(d2 / nu)
  }
  xbar <- colMeans(X)
  A <- crossprod(sweep(X, 2L, xbar))
  lt <- vapply(
    seq_len(n),
    function(i) {
      m <- n - 1
      xb <- (n * xbar - X[i, ]) / m
      Am <- A - (n / m) * tcrossprod(X[i, ] - xbar)
      nu <- m - q
      lmvt(X[i, ], xb, Am * (m + 1) / (m * nu), nu)
    },
    numeric(1)
  )

  expect_equal(res_j$elpd_2 - res_c$elpd_2, sum(lt), tolerance = 2e-3)
})

twolevel_model_z <- "
  level: 1
    fw =~ y1 + y2 + y3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"
fit_2c <- asem(
  twolevel_model_z,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  fixed.x = TRUE,
  meanstructure = TRUE,
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)

test_that("conditional LOCO with cluster-level covariates matches reference values", {
  res_2c <- loo(fit_2c)
  expect_equal(res_2c$flavour, "conditional")
  expect_equal(res_2c$type, "loco")
  expect_equal(res_2c$n_units, 200L)
  expect_equal(res_2c$elpd_1, -12518.7018385497, tolerance = 1e-4)
  expect_equal(res_2c$elpd_2, -12527.7612324983, tolerance = 1e-4)
  expect_equal(res_2c$se_1, 400.7882650448, tolerance = 1e-3)
  expect_equal(res_2c$se_2, 400.9874715240, tolerance = 1e-3)

  pu <- res_2c$per_unit[c(1L, 100L, 200L), ]
  expect_equal(
    pu$l_star,
    c(-23.6884396577, -92.5933047338, -96.8530982807),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-23.6926283571, -92.7364588427, -96.9632184856),
    tolerance = 1e-4
  )

  # The subtracted covariate constant is pinned by the loglik identity
  expect_equal(
    sum(res_2c$per_unit$l_star),
    ll_at_mode(fit_2c),
    tolerance = 1e-6
  )

  # Row-deletion override runs on the conditional flavour (the covariate
  # constant cancels between the full and downdated cluster terms)
  expect_warning(
    res_row <- loo(fit_2c, type = "loso", units = 1:4),
    "per-row deletion"
  )
  expect_equal(res_row$flavour, "conditional")
  expect_true(all(is.finite(res_row$per_unit$log_cpo_1)))
})

test_that("conditional LOCO with within-level covariates matches reference values", {
  twolevel_model_wx <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1 + x2 + x3
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1 + w2
  "
  fit_wx <- asem(
    twolevel_model_wx,
    lavaan::Demo.twolevel,
    cluster = "cluster",
    fixed.x = TRUE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  res_wx <- loo(fit_wx)
  expect_equal(res_wx$flavour, "conditional")
  expect_equal(res_wx$n_units, 200L)
  expect_equal(res_wx$elpd_1, -12086.7629617682, tolerance = 1e-4)
  expect_equal(res_wx$elpd_2, -12097.3274537623, tolerance = 1e-4)
  expect_equal(res_wx$se_1, 387.9154865799, tolerance = 1e-3)
  expect_equal(res_wx$se_2, 388.1556448756, tolerance = 1e-3)

  pu <- res_wx$per_unit[c(1L, 100L, 200L), ]
  expect_equal(
    pu$l_star,
    c(-22.9475363793, -92.1020515691, -95.8874752171),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-22.9527716428, -92.2700952444, -96.0276662972),
    tolerance = 1e-4
  )

  # The general covariate constant is pinned by the loglik identity
  expect_equal(
    sum(res_wx$per_unit$l_star),
    ll_at_mode(fit_wx),
    tolerance = 1e-6
  )

  # Row-deletion override: the conditional row contribution equals the
  # conditional cluster loglik difference reconstructed from raw data
  expect_warning(
    res_row <- loo(fit_wx, type = "loso", units = c(2L, 50L)),
    "per-row deletion"
  )
  expect_equal(res_row$flavour, "conditional")
  int <- get_inlavaan_internal(fit_wx)
  css <- INLAvaan:::loco_suff_stats(int$lavdata)
  X <- int$lavdata@X[[1L]]
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  i <- 2L
  j <- css$cluster_idx[i]
  lcond <- function(cs) {
    INLAvaan:::loco_loglik_one(j, cs, cache$mom) -
      INLAvaan:::loo_fixedx_const_loco(int, cs, j, cache$mom)
  }
  rows <- setdiff(which(css$cluster_idx == j), i)
  Yj <- X[rows, , drop = FALSE]
  css_m <- css
  css_m$n_j[j] <- length(rows)
  css_m$S[[j]] <- crossprod(sweep(Yj, 2L, colMeans(Yj), "-"))
  css_m$ybar[j, ] <- colMeans(Yj)
  css_m$mean_d[[j]] <- colMeans(Yj)[css$zy_idx]
  expect_equal(
    res_row$per_unit$l_star[1L],
    lcond(css) - lcond(css_m),
    tolerance = 1e-8
  )

  # Within-only covariates (no cluster-level z) also run
  twolevel_model_w <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1 + x2
    level: 2
      fb =~ y1 + y2 + y3
  "
  fit_w <- asem(
    twolevel_model_w,
    lavaan::Demo.twolevel,
    cluster = "cluster",
    fixed.x = TRUE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  res_w <- loo(fit_w, units = 1:10)
  expect_equal(res_w$flavour, "conditional")
  expect_true(all(is.finite(res_w$per_unit$log_cpo_2)))
  expect_equal(
    sum(loo(fit_w)$per_unit$l_star),
    ll_at_mode(fit_w),
    tolerance = 1e-6
  )
})

test_that("waic scores fixed.x fits conditionally", {
  set.seed(1)
  w <- suppressWarnings(waic(fit_c, nsamp = 300))
  expect_equal(w$flavour, "conditional")
  expect_true(all(is.finite(w$per_unit$lpd)))
  expect_output(print(w), "conditionally on the exogenous covariates")
})

test_that("compare(loo = TRUE) enforces the flavour rules", {
  # Conditional fits may condition on different covariate sets (covariate
  # selection); the outcome variables match, so the comparison is valid
  HS_model_x1 <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    visual ~ ageyr
  "
  fit_c1 <- asem(
    HS_model_x1,
    dat_x,
    fixed.x = TRUE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  cmp <- compare(fit_c, fit_c1, loo = TRUE)
  expect_true(all(
    c("ELPD", "SE", "elpd_diff", "se_diff") %in% names(cmp)
  ))
  expect_true(all(is.finite(cmp$se_diff)))

  # Mixing joint and conditional scores is refused
  fit_j <- asem(
    HS_model_x,
    dat_x,
    fixed.x = FALSE,
    meanstructure = TRUE,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  )
  expect_error(
    compare(fit_c, fit_j, loo = TRUE),
    "mix joint and conditional"
  )
})

test_that("Conditional flavour works without a mean structure", {
  # Regression test: this combination (the default for SEM with exogenous
  # covariates) used to abort. The per-unit contribution is the difference
  # of two exchangeable case-deletion conditionals; the x-block term is a
  # theta-free constant evaluated at the frozen covariate moments.
  mod_x <- "
    visual  =~ x1 + x2 + x3
    visual  ~ grade
  "
  dat_x <- lavaan::HolzingerSwineford1939[, c("x1", "x2", "x3", "grade")]
  fit <- asem(mod_x, dat_x, verbose = FALSE, test = "none", nsamp = 3)
  expect_false(fit@Model@meanstructure)
  res <- loo(fit)
  int <- get_inlavaan_internal(fit)
  Y <- int$lavdata@X[[1L]]
  n <- nrow(Y)
  cc <- n / (n - 1)
  ybar <- colMeans(Y)
  x_idx <- int$lavsamplestats@x.idx[[1L]]
  Sg <- loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable
  )$mom[[1L]]$Sigma
  ldmvn <- function(y, mu, S) {
    ch <- chol(S)
    d <- backsolve(ch, y - mu, transpose = TRUE)
    -0.5 * (length(y) * log(2 * pi) + 2 * sum(log(diag(ch))) + sum(d^2))
  }
  ii <- c(1L, 150L, n)
  l_closed <- vapply(
    ii,
    function(i) {
      ybar_m <- (n * ybar - Y[i, ]) / (n - 1)
      ldmvn(Y[i, ], ybar_m, cc * Sg) -
        ldmvn(Y[i, x_idx], ybar_m[x_idx], cc * Sg[x_idx, x_idx, drop = FALSE])
    },
    numeric(1)
  )
  expect_equal(res$per_unit$l_star[ii], l_closed, tolerance = 1e-10)
})
