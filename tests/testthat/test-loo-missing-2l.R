# Extended LOO suite pinned to reference values. It runs in CI, and
# test-loo-loso.R covers the core LOO on CRAN.
skip_on_cran()

# Two-level FIML LOCO (leave-one-cluster-out under missing data). Each cluster
# is scored on its observed-data marginal likelihood via lavaan's raw-data
# missing kernels; since LOCO deletes a whole cluster there is no downdating.
# Fits without covariates are scored jointly, fixed.x fits conditionally.

twolevel_model <- "
  level: 1
    fw =~ y1 + y2 + y3
  level: 2
    fb =~ y1 + y2 + y3
"

# Keep the first 30 cluster ids (cluster sizes cycle 5, 10, 15, 20 every 4
# cluster ids in lavaan::Demo.twolevel, so every size is represented several
# times). MCAR holes in y1-y3; seed set immediately before the hole-punching
# loop so the dataset (and the pinned reference values) are reproducible. With
# ncl = 30, cluster 9 ends up with a row fully missing on y1-y3 -- needed by
# the two dedicated regression tests below.
make_miss <- function() {
  d <- lavaan::Demo.twolevel[, c("y1", "y2", "y3", "cluster")]
  d <- d[d$cluster <= 30, ]
  set.seed(20260613)
  for (v in c("y1", "y2", "y3")) {
    d[[v]][runif(nrow(d)) < 0.12] <- NA
  }
  d
}
d <- make_miss()

# a row is fully missing after punching holes; lavaan flags this with a
# (benign) note about the two-level FIML gradient, suppressed here so the
# fixture sets up cleanly (loo()/waic() handle these rows correctly)
fit <- suppressWarnings(asem(
  twolevel_model,
  d,
  cluster = "cluster",
  missing = "ml",
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
))
res <- loo(fit)

test_that("the test dataset has the expected missingness", {
  expect_equal(sum(is.na(d[, 1:3])), 129L)
  expect_equal(sum(!complete.cases(d[, 1:3])), 118L)
})

test_that("two-level FIML LOCO matches reference values", {
  # Reference values cross-checked against (i) lavaan's fitted two-level FIML
  # loglik, (ii) an independent dense marginal-covariance kernel, and (iii)
  # finite differences. This dataset has fully-missing-within cases, whose
  # gradient contribution lavaan < 0.7-1.2707 computed slightly inexactly
  # (fixed in lavaan PR #581); the values below are pinned at the mode found
  # with the corrected gradient, i.e. lavaan >= 0.7-1.2707 (required).
  expect_equal(res$type, "loco")
  expect_equal(res$flavour, "joint")
  expect_equal(res$n_units, 30L)
  expect_equal(res$elpd_1, -1622.0237377723, tolerance = 1e-4)
  expect_equal(res$elpd_2, -1631.5489111053, tolerance = 1e-4)
  expect_equal(res$se_1, 146.6170298875, tolerance = 1e-4)
  expect_equal(res$se_2, 147.4317813658, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 14.2293264754, tolerance = 1e-2)
  expect_equal(res$p_loo_2, 16.3265819302, tolerance = 1e-2)

  # first, middle, and last of the 30 clusters
  pu <- res$per_unit[c(1L, 15L, 30L), ]
  expect_equal(pu$nobs, c(5L, 15L, 10L))
  expect_equal(
    pu$l_star,
    c(-16.9286021186, -69.6123529442, -42.7294434596),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-16.9658686731, -69.7831061716, -42.8674693924),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-17.0288789994, -70.1463770505, -43.0327244061),
    tolerance = 1e-4
  )
})

test_that("per-cluster observed-data logliks sum to the fitted FIML loglik", {
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

test_that("analytic per-cluster scores match finite differences", {
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  int <- get_inlavaan_internal(fit)
  minfo <- INLAvaan:::loco_missing_info(int)
  js <- c(1L, 15L, 30L) # first, middle, and last of the 30 clusters
  s_an <- INLAvaan:::loco_missing_scores_theta(
    int$theta_star,
    minfo,
    int$lavmodel,
    int$partable,
    js
  )
  h <- 1e-6
  lj <- function(theta, j) {
    cache <- INLAvaan:::loo_grad_cache(
      theta,
      int$lavmodel,
      int$partable,
      two_level = TRUE
    )
    INLAvaan:::loco_missing_loglik_one(j, minfo, cache$mom)
  }
  # vapply stacks the per-parameter columns into a length(js) x m matrix,
  # matching the score matrix returned by loco_missing_scores_theta()
  s_fd <- vapply(
    seq_along(int$theta_star),
    function(k) {
      tp <- tm <- int$theta_star
      tp[k] <- tp[k] + h
      tm[k] <- tm[k] - h
      vapply(js, function(j) (lj(tp, j) - lj(tm, j)) / (2 * h), numeric(1))
    },
    numeric(length(js))
  )
  expect_equal(max(abs(s_an - s_fd)), 0, tolerance = 1e-5)
})

test_that("loo object structure and unit subsetting", {
  expect_s3_class(res, "inlavaan_loo")
  expect_true(all(res$per_unit$ok))
  expect_equal(
    res$per_unit$lpd_1 + res$per_unit$log_cpo_1,
    2 * res$per_unit$l_star
  )
  res5 <- loo(fit, units = 1:5)
  expect_equal(nrow(res5$per_unit), 5L)
  expect_equal(
    res5$per_unit$log_cpo_2,
    res$per_unit$log_cpo_2[1:5],
    tolerance = 1e-8
  )
})

test_that("waic() runs on a two-level FIML fit and agrees loosely with loo()", {
  w <- suppressWarnings(waic(fit))
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, 30L)
  expect_equal(w$type, "loco")
  expect_equal(w$flavour, "joint")
  expect_true(all(is.finite(w$per_unit$lpd)))
  expect_equal(
    unname(w$estimates["elpd_waic", "Estimate"]),
    res$elpd_2,
    tolerance = 0.01
  )
})

test_that("the per-row (leave-one-unit-out) override works under missing data", {
  # type = "loso" on a clustered fit warns (conditional vs marginal) then
  # scores the leave-one-unit-out conditional predictive per row
  expect_warning(
    res_row <- loo(fit, type = "loso", units = 1:10),
    "leave-one-unit-out"
  )
  expect_equal(res_row$type, "loso")
  expect_equal(nrow(res_row$per_unit), 10L)
  expect_true(all(res_row$per_unit$nobs == 1L))

  # analytic per-row scores agree with finite differences, including rows in
  # clusters that contain a fully-missing row. Skipped on CRAN: this agreement
  # is sensitive to BLAS/compiler differences across check flavours.
  skip_on_cran()
  int <- get_inlavaan_internal(fit)
  minfo <- INLAvaan:::loco_missing_info(int)
  rows <- c(1L, 5L, 200L, minfo$rows_by_cluster[[9L]])
  s_an <- INLAvaan:::loso2l_missing_scores_theta(
    int$theta_star,
    minfo,
    int$lavmodel,
    int$partable,
    rows
  )
  h <- 1e-6
  s_fd <- vapply(
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
      (INLAvaan:::loso2l_missing_loglik_all(rows, minfo, cp$mom) -
        INLAvaan:::loso2l_missing_loglik_all(rows, minfo, cm$mom)) /
        (2 * h)
    },
    numeric(length(rows))
  )
  expect_equal(max(abs(s_an - s_fd)), 0, tolerance = 1e-5)
})

test_that("LOCO scores are correct for clusters with a fully-missing row", {
  # regression guard for the fix: cluster 9 has a row with all within
  # variables missing; its score must match finite differences
  int <- get_inlavaan_internal(fit)
  minfo <- INLAvaan:::loco_missing_info(int)
  expect_true(any(minfo$n_obs < minfo$n_j)) # some cluster has a fully-missing row
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  s_an <- INLAvaan:::loco_missing_scores_theta(
    int$theta_star,
    minfo,
    int$lavmodel,
    int$partable,
    9L
  )
  h <- 1e-6
  s_fd <- vapply(
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
      (INLAvaan:::loco_missing_loglik_one(9L, minfo, cp$mom) -
        INLAvaan:::loco_missing_loglik_one(9L, minfo, cm$mom)) /
        (2 * h)
    },
    numeric(1)
  )
  expect_equal(max(abs(as.numeric(s_an) - s_fd)), 0, tolerance = 1e-5)
})

# Between-only variables under FIML, with MCAR holes in y1 and the between-only
# outcome w2 missing in clusters 3 and 7. fit_between also has a cluster-level
# covariate (w1), so those clusters keep a partial between-level pattern.
# fit_between_na drops w1, leaving them with no observed between-only values.
model_between <- "
  level: 1
    fw =~ y1 + y2 + y3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    w2 ~ fb
"
model_between_na <- "
  level: 1
    fw =~ y1 + y2 + y3
  level: 2
    fb =~ y1 + y2 + y3
    w2 ~ fb
"
d_between <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
set.seed(123)
d_between$y1[sample(nrow(d_between), 15)] <- NA
d_between$w2[d_between$cluster %in% c(3, 7)] <- NA
# Clusters 5 and 6 have w2 missing on their first row only, which lavaan reads
# as w2 missing for the whole cluster. The first row of cluster 5 is also
# missing on y1-y3, so it is an empty row in the cluster's patterns.
first_row <- !duplicated(d_between$cluster)
d_between$w2[first_row & d_between$cluster %in% c(5, 6)] <- NA
d_between[first_row & d_between$cluster == 5, c("y1", "y2", "y3")] <- NA

fit_2l_ml <- function(model, data) {
  suppressWarnings(asem(
    model,
    data,
    cluster = "cluster",
    missing = "ml",
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  ))
}
fits_between <- list(
  fit_between = fit_2l_ml(model_between, d_between),
  fit_between_na = fit_2l_ml(model_between_na, d_between)
)

test_that("LOCO handles between-only variables under missing data", {
  for (fit_b in fits_between) {
    res_b <- loo(fit_b)
    expect_equal(res_b$type, "loco")
    expect_equal(res_b$n_units, 30L)
    expect_true(all(res_b$per_unit$ok))

    # Per-cluster logliks sum to the fitted FIML loglik, which is conditional on
    # the covariate (w1) of fit_between as the scores are.
    int <- get_inlavaan_internal(fit_b)
    x <- INLAvaan:::pars_to_x(int$theta_star, int$partable)
    lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
    opts <- fit_b@Options
    opts$estimator <- "ML"
    ll <- lavaan:::lav_model_loglik(
      lavdata = int$lavdata,
      lavsamplestats = int$lavsamplestats,
      lavimplied = lavaan::lav_model_implied(lm_x),
      lavmodel = lm_x,
      lavoptions = opts
    )$loglik
    expect_equal(sum(res_b$per_unit$l_star), ll, tolerance = 1e-6)
  }
})

test_that("the per-row override handles between-only variables", {
  for (fit_b in fits_between) {
    minfo <- INLAvaan:::loco_missing_info(get_inlavaan_internal(fit_b))
    rows <- c(1L, minfo$rows_by_cluster[[3L]][1:2])
    expect_warning(
      res_row <- loo(fit_b, type = "loso", units = rows),
      "leave-one-unit-out"
    )
    expect_equal(nrow(res_row$per_unit), 3L)
    expect_true(all(res_row$per_unit$ok))
  }
})

test_that("between-only scores match finite differences", {
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  for (fit_b in fits_between) {
    int <- get_inlavaan_internal(fit_b)
    minfo <- INLAvaan:::loco_missing_info(int)
    # w2 missing in cluster 3, and on the first row only in clusters 5 and 6
    js <- c(1L, 3L, 5L, 6L)
    rows <- c(
      1L,
      minfo$rows_by_cluster[[3L]][1:2],
      minfo$rows_by_cluster[[6L]][1L]
    )
    s_an <- rbind(
      INLAvaan:::loco_missing_scores_theta(
        int$theta_star,
        minfo,
        int$lavmodel,
        int$partable,
        js
      ),
      INLAvaan:::loso2l_missing_scores_theta(
        int$theta_star,
        minfo,
        int$lavmodel,
        int$partable,
        rows
      )
    )
    ll_units <- function(theta) {
      cache <- INLAvaan:::loo_grad_cache(
        theta,
        int$lavmodel,
        int$partable,
        two_level = TRUE
      )
      c(
        vapply(
          js,
          function(j) INLAvaan:::loco_missing_loglik_one(j, minfo, cache$mom),
          numeric(1)
        ),
        INLAvaan:::loso2l_missing_loglik_all(rows, minfo, cache$mom)
      )
    }
    h <- 1e-6
    s_fd <- vapply(
      seq_along(int$theta_star),
      function(k) {
        tp <- tm <- int$theta_star
        tp[k] <- tp[k] + h
        tm[k] <- tm[k] - h
        (ll_units(tp) - ll_units(tm)) / (2 * h)
      },
      numeric(length(js) + length(rows))
    )
    expect_equal(max(abs(s_an - s_fd)), 0, tolerance = 1e-5)
  }
})

test_that("missing kernels match complete-data kernels on complete data", {
  d_full <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
  int <- get_inlavaan_internal(fit_2l_ml(model_between, d_full))
  css <- INLAvaan:::loco_suff_stats(int$lavdata)
  minfo <- INLAvaan:::loco_missing_info(int)
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  js <- seq_len(css$J)
  expect_equal(
    vapply(
      js,
      function(j) INLAvaan:::loco_missing_loglik_one(j, minfo, cache$mom),
      numeric(1)
    ),
    vapply(
      js,
      function(j) INLAvaan:::loco_loglik_one(j, css, cache$mom),
      numeric(1)
    ),
    tolerance = 1e-10
  )
  expect_equal(
    INLAvaan:::loco_missing_scores_theta(
      int$theta_star,
      minfo,
      int$lavmodel,
      int$partable,
      js,
      cache
    ),
    INLAvaan:::loco_scores_theta(
      int$theta_star,
      css,
      int$lavmodel,
      int$partable,
      js,
      cache
    ),
    tolerance = 1e-8
  )
})

# Cluster 1 keeps one responder, its other four rows fully missing on y1-y3
d_resp <- lavaan::Demo.twolevel[
  lavaan::Demo.twolevel$cluster <= 30,
  c("y1", "y2", "y3", "w1", "cluster")
]
rows_resp <- which(d_resp$cluster == 1)
d_resp[rows_resp[-1L], c("y1", "y2", "y3")] <- NA
fit_resp <- fit_2l_ml(
  "level: 1\n fw =~ y1 + y2 + y3\n level: 2\n y1 ~ w1",
  d_resp
)

test_that("the per-row override scores only a cluster's observed row", {
  expect_message(
    res_row <- suppressWarnings(loo(
      fit_resp,
      type = "loso",
      units = rows_resp
    )),
    "Not scoring 4 rows"
  )
  expect_equal(res_row$per_unit$unit, rows_resp[1L])
  # Deleting the only responder deletes the cluster
  res_cl <- loo(fit_resp, units = 1L)
  expect_equal(res_row$per_unit$l_star, res_cl$per_unit$l_star)

  # Fully missing rows contribute zero with a zero score
  int <- get_inlavaan_internal(fit_resp)
  minfo <- INLAvaan:::loco_missing_info(int)
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  ll <- INLAvaan:::loso2l_missing_loglik_all(rows_resp, minfo, cache$mom)
  expect_equal(ll[-1L], rep(0, 4L))
  s <- INLAvaan:::loso2l_missing_scores_theta(
    int$theta_star,
    minfo,
    int$lavmodel,
    int$partable,
    rows_resp,
    cache
  )
  expect_equal(unname(s[-1L, ]), matrix(0, 4L, ncol(s)))

  expect_error(
    suppressWarnings(loo(fit_resp, type = "loso", units = rows_resp[-1L])),
    "No units to score"
  )
})

# Small and empty clusters: cluster 2 cut to one row and cluster 3 to two,
# cluster 4 missing y1-y3 (only its between-level w2 observed) and cluster 5
# missing everything, plus MCAR holes in y2. The second model has exactly one
# variable (y1) at both levels.
d_small <- lavaan::Demo.twolevel[
  lavaan::Demo.twolevel$cluster <= 30,
  c("y1", "y2", "y3", "w2", "cluster")
]
pos <- ave(d_small$cluster, d_small$cluster, FUN = seq_along)
d_small <- d_small[
  !(d_small$cluster == 2 & pos > 1) & !(d_small$cluster == 3 & pos > 2),
]
d_small[d_small$cluster == 4, c("y1", "y2", "y3")] <- NA
d_small[d_small$cluster == 5, c("y1", "y2", "y3", "w2")] <- NA
set.seed(1)
d_small$y2[sample(nrow(d_small), 20)] <- NA
fits_small <- list(
  three_both = fit_2l_ml(
    "level: 1\n fw =~ y1 + y2 + y3\n level: 2\n fb =~ y1 + y2 + y3\n w2 ~ fb",
    d_small
  ),
  one_both = fit_2l_ml(
    "level: 1\n fw =~ y1 + y2 + y3\n level: 2\n y1 ~~ w2",
    d_small
  )
)

# Dense observed-data marginal of a set of rows from one cluster: the rows'
# level-1 values stacked, then the cluster's between-only values.
dense_marginal <- function(int, mom, rows) {
  X <- int$lavdata@X[[1L]]
  ovn <- int$lavdata@ov.names[[1L]]
  l1 <- int$lavdata@ov.names.l[[1L]][[1L]]
  l2 <- int$lavdata@ov.names.l[[1L]][[2L]]
  z <- setdiff(l2, l1)
  n <- length(rows)
  E <- matrix(0, length(l1), length(l2))
  b <- match(l1, l2)
  E[cbind(which(!is.na(b)), b[!is.na(b)])] <- 1
  Ez <- matrix(0, length(z), length(l2))
  Ez[cbind(seq_along(z), match(z, l2))] <- 1
  L <- rbind(E[rep(seq_along(l1), n), , drop = FALSE], Ez)
  W <- matrix(0, nrow(L), nrow(L))
  W[seq_len(n * length(l1)), seq_len(n * length(l1))] <-
    kronecker(diag(n), mom$Sigma_w)
  V <- W + L %*% mom$Sigma_b %*% t(L)
  m <- c(rep(mom$mu_w, n), numeric(length(z))) + as.numeric(L %*% mom$mu_b)
  v <- c(
    as.numeric(t(X[rows, match(l1, ovn), drop = FALSE])),
    X[rows[1L], match(z, ovn)]
  )
  o <- which(!is.na(v))
  INLAvaan:::mvn_loglik_rows(
    matrix(v[o], 1L),
    m[o],
    V[o, o, drop = FALSE]
  )
}

test_that("LOCO scores small clusters and drops empty ones", {
  for (fit_s in fits_small) {
    expect_message(res_s <- loo(fit_s), "Not scoring 1 cluster")
    expect_equal(res_s$per_unit$unit, setdiff(1:30, 5L))
    expect_true(all(is.finite(res_s$per_unit$log_cpo_2)))

    int <- get_inlavaan_internal(fit_s)
    minfo <- INLAvaan:::loco_missing_info(int)
    cache <- INLAvaan:::loo_grad_cache(
      int$theta_star,
      int$lavmodel,
      int$partable,
      two_level = TRUE
    )
    ll <- vapply(
      seq_len(minfo$J),
      function(j) INLAvaan:::loco_missing_loglik_one(j, minfo, cache$mom),
      numeric(1)
    )
    expect_equal(ll[5L], 0)
    expect_equal(res_s$per_unit$l_star, ll[-5L])

    # Per-cluster logliks sum to the fitted FIML loglik
    x <- INLAvaan:::pars_to_x(int$theta_star, int$partable)
    lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
    opts <- fit_s@Options
    opts$estimator <- "ML"
    ll_lav <- lavaan:::lav_model_loglik(
      lavdata = int$lavdata,
      lavsamplestats = int$lavsamplestats,
      lavimplied = lavaan::lav_model_implied(lm_x),
      lavmodel = lm_x,
      lavoptions = opts
    )$loglik
    expect_equal(sum(ll), ll_lav, tolerance = 1e-8)

    # The one-row, two-row and within-empty clusters match the dense marginal
    for (j in 2:4) {
      expect_equal(
        ll[j],
        dense_marginal(int, cache$mom, minfo$rows_by_cluster[[j]]),
        tolerance = 1e-10
      )
    }
  }
})

test_that("the per-row override scores one- and two-row clusters", {
  for (fit_s in fits_small) {
    int <- get_inlavaan_internal(fit_s)
    minfo <- INLAvaan:::loco_missing_info(int)
    r2 <- minfo$rows_by_cluster[[2L]]
    r3 <- minfo$rows_by_cluster[[3L]]
    expect_warning(
      res_row <- loo(fit_s, type = "loso", units = c(r2, r3)),
      "leave-one-unit-out"
    )
    expect_true(all(res_row$per_unit$ok))
    # The singleton's only row is the cluster
    expect_equal(
      res_row$per_unit$l_star[1L],
      loo(fit_s, units = 2L)$per_unit$l_star
    )
    # Each row of the two-row cluster, given the other
    cache <- INLAvaan:::loo_grad_cache(
      int$theta_star,
      int$lavmodel,
      int$partable,
      two_level = TRUE
    )
    l3 <- dense_marginal(int, cache$mom, r3)
    l_other <- c(
      dense_marginal(int, cache$mom, r3[2L]),
      dense_marginal(int, cache$mom, r3[1L])
    )
    expect_equal(res_row$per_unit$l_star[2:3], l3 - l_other, tolerance = 1e-10)
  }
})

test_that("scores of small clusters match finite differences", {
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  for (fit_s in fits_small) {
    int <- get_inlavaan_internal(fit_s)
    minfo <- INLAvaan:::loco_missing_info(int)
    js <- 2:4
    rows <- c(minfo$rows_by_cluster[[2L]], minfo$rows_by_cluster[[3L]])
    s_an <- rbind(
      INLAvaan:::loco_missing_scores_theta(
        int$theta_star,
        minfo,
        int$lavmodel,
        int$partable,
        js
      ),
      INLAvaan:::loso2l_missing_scores_theta(
        int$theta_star,
        minfo,
        int$lavmodel,
        int$partable,
        rows
      )
    )
    ll_units <- function(theta) {
      cache <- INLAvaan:::loo_grad_cache(
        theta,
        int$lavmodel,
        int$partable,
        two_level = TRUE
      )
      c(
        vapply(
          js,
          function(j) INLAvaan:::loco_missing_loglik_one(j, minfo, cache$mom),
          numeric(1)
        ),
        INLAvaan:::loso2l_missing_loglik_all(rows, minfo, cache$mom)
      )
    }
    h <- 1e-6
    s_fd <- vapply(
      seq_along(int$theta_star),
      function(k) {
        tp <- tm <- int$theta_star
        tp[k] <- tp[k] + h
        tm[k] <- tm[k] - h
        (ll_units(tp) - ll_units(tm)) / (2 * h)
      },
      numeric(length(js) + length(rows))
    )
    expect_equal(max(abs(s_an - s_fd)), 0, tolerance = 1e-5)
  }
})

test_that("missing kernels match complete-data kernels on small clusters", {
  d_one <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
  pos <- ave(d_one$cluster, d_one$cluster, FUN = seq_along)
  d_one <- d_one[
    !(d_one$cluster == 2 & pos > 1) & !(d_one$cluster == 3 & pos > 2),
  ]
  fit_one <- fit_2l_ml(
    "level: 1\n fw =~ y1 + y2 + y3\n level: 2\n fb =~ y1 + y2 + y3\n w2 ~ fb",
    d_one
  )
  int <- get_inlavaan_internal(fit_one)
  css <- INLAvaan:::loco_suff_stats(int$lavdata)
  minfo <- INLAvaan:::loco_missing_info(int)
  X <- int$lavdata@X[[1L]]
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  js <- 1:4
  expect_equal(
    vapply(
      js,
      function(j) INLAvaan:::loco_missing_loglik_one(j, minfo, cache$mom),
      numeric(1)
    ),
    vapply(
      js,
      function(j) INLAvaan:::loco_loglik_one(j, css, cache$mom),
      numeric(1)
    ),
    tolerance = 1e-10
  )
  rows <- c(minfo$rows_by_cluster[[2L]], minfo$rows_by_cluster[[3L]])
  expect_equal(
    INLAvaan:::loso2l_missing_loglik_all(rows, minfo, cache$mom),
    INLAvaan:::loso2l_loglik_all(rows, css, X, cache$mom),
    tolerance = 1e-10
  )
  expect_equal(
    INLAvaan:::loso2l_missing_scores_theta(
      int$theta_star,
      minfo,
      int$lavmodel,
      int$partable,
      rows,
      cache
    ),
    INLAvaan:::loso2l_scores_theta(
      int$theta_star,
      css,
      X,
      int$lavmodel,
      int$partable,
      rows,
      cache
    ),
    tolerance = 1e-8
  )
  expect_no_error(loo(fit_one, units = js))
})

# Fixed.x covariates: x1 within, w1 between. The FIML score is conditional on
# them, as the listwise (complete-data) score is.
model_x <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
"
d_x <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
fit_x <- function(data, missing = "ml") {
  suppressWarnings(asem(
    model_x,
    data,
    cluster = "cluster",
    missing = missing,
    verbose = FALSE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none"
  ))
}
fit_x_ml <- fit_x(d_x)
res_x_ml <- loo(fit_x_ml)
rows_x <- c(1:3, 6L, 100L, 200L)
loo_cols <- c("l_star", "log_cpo_1", "log_cpo_2", "lpd_2")

test_that("two-level FIML scores fixed.x fits as the listwise path does", {
  fit_lw <- fit_x(d_x, missing = "listwise")
  int_lw <- get_inlavaan_internal(fit_lw)
  expect_equal(res_x_ml$flavour, "conditional")
  # Same data and summary, so the two kernels must agree
  res_ml <- loo(
    fit_x_ml,
    theta = int_lw$theta_star,
    Omega = int_lw$Sigma_theta
  )
  expect_equal(
    res_ml$per_unit[loo_cols],
    loo(fit_lw)$per_unit[loo_cols],
    tolerance = 1e-8
  )
  res_ml_row <- suppressWarnings(loo(
    fit_x_ml,
    type = "loso",
    units = rows_x,
    theta = int_lw$theta_star,
    Omega = int_lw$Sigma_theta
  ))
  res_lw_row <- suppressWarnings(loo(fit_lw, type = "loso", units = rows_x))
  expect_equal(
    res_ml_row$per_unit[loo_cols],
    res_lw_row$per_unit[loo_cols],
    tolerance = 1e-8
  )

  # The units sum to lavaan's loglik, which is conditional under fixed.x
  int <- get_inlavaan_internal(fit_x_ml)
  x <- INLAvaan:::pars_to_x(int$theta_star, int$partable)
  lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
  opts <- fit_x_ml@Options
  opts$estimator <- "ML"
  ll <- lavaan:::lav_model_loglik(
    lavdata = int$lavdata,
    lavsamplestats = int$lavsamplestats,
    lavimplied = lavaan::lav_model_implied(lm_x),
    lavmodel = lm_x,
    lavoptions = opts
  )$loglik
  expect_equal(sum(res_x_ml$per_unit$l_star), ll, tolerance = 1e-8)
})

test_that("rescaling the covariates leaves the conditional score unchanged", {
  d_s <- d_x
  d_s$x1 <- 10 * d_s$x1
  d_s$w1 <- 10 * d_s$w1
  fit_s <- fit_x(d_s)
  # Map the summary onto the rescaled fit: the slopes shrink tenfold
  int <- get_inlavaan_internal(fit_x_ml)
  pt <- int$partable
  k <- pt$free[pt$op == "~" & pt$rhs %in% c("x1", "w1")]
  D <- rep(1, length(int$theta_star))
  D[k] <- 1 / 10
  res_s <- loo(
    fit_s,
    theta = int$theta_star * D,
    Omega = int$Sigma_theta * outer(D, D)
  )
  expect_equal(
    res_s$per_unit[loo_cols],
    res_x_ml$per_unit[loo_cols],
    tolerance = 1e-8
  )
  res_s_row <- suppressWarnings(loo(
    fit_s,
    type = "loso",
    units = rows_x,
    theta = int$theta_star * D,
    Omega = int$Sigma_theta * outer(D, D)
  ))
  res_row <- suppressWarnings(loo(fit_x_ml, type = "loso", units = rows_x))
  expect_equal(
    res_s_row$per_unit[loo_cols],
    res_row$per_unit[loo_cols],
    tolerance = 1e-8
  )
})

# missing = "ml.x" keeps rows with a missing covariate. Cluster 4 keeps only its
# covariates, so it has no outcome to score.
d_mlx <- d_x
set.seed(2)
d_mlx$x1[sample(nrow(d_mlx), 25)] <- NA
d_mlx$y1[sample(nrow(d_mlx), 25)] <- NA
d_mlx[d_mlx$cluster == 4, c("y1", "y2", "y3")] <- NA
fit_x_mlx <- fit_x(d_mlx, missing = "ml.x")

test_that("ml.x scores each cluster given its observed covariates", {
  expect_message(
    res_mlx <- loo(fit_x_mlx),
    "Not scoring 1 cluster with no observed outcome data"
  )
  expect_equal(res_mlx$per_unit$unit, setdiff(1:30, 4L))
  expect_true(all(is.finite(res_mlx$per_unit$log_cpo_2)))

  # Independent covariate term: x1 (within only) and w1 (between only) are
  # scored row by row and cluster by cluster on their observed values.
  int <- get_inlavaan_internal(fit_x_mlx)
  minfo <- INLAvaan:::loco_missing_info(int)
  cache <- INLAvaan:::loo_grad_cache(
    int$theta_star,
    int$lavmodel,
    int$partable,
    two_level = TRUE
  )
  mom <- cache$mom
  ovn <- int$lavdata@ov.names[[1L]]
  l1 <- int$lavdata@ov.names.l[[1L]][[1L]]
  l2 <- int$lavdata@ov.names.l[[1L]][[2L]]
  x1 <- minfo$X[, match("x1", ovn)]
  a <- match("x1", l1)
  l_x1 <- dnorm(x1, mom$mu_w[a], sqrt(mom$Sigma_w[a, a]), log = TRUE)
  w1 <- vapply(minfo$clusters, function(cj) cj$Y2[1L, match("w1", ovn)], 0)
  b <- match("w1", l2)
  l_w1 <- dnorm(w1, mom$mu_b[b], sqrt(mom$Sigma_b[b, b]), log = TRUE)
  c_j <- as.numeric(rowsum(l_x1, minfo$cl, na.rm = TRUE)) + l_w1
  ll_j <- vapply(
    seq_len(minfo$J),
    function(j) INLAvaan:::loco_missing_loglik_one(j, minfo, mom),
    numeric(1)
  )
  expect_equal(res_mlx$per_unit$l_star, (ll_j - c_j)[-4L], tolerance = 1e-10)
  # The covariate-only cluster has nothing left to score
  expect_equal(ll_j[4L], c_j[4L], tolerance = 1e-10)

  # The per-row override drops cluster 4's covariate-only rows
  rows <- c(minfo$rows_by_cluster[[1L]], minfo$rows_by_cluster[[4L]][1L])
  expect_message(
    res_row <- suppressWarnings(loo(fit_x_mlx, type = "loso", units = rows)),
    "Not scoring 1 row"
  )
  expect_equal(res_row$per_unit$unit, minfo$rows_by_cluster[[1L]])
  expect_true(all(is.finite(res_row$per_unit$log_cpo_2)))
})

test_that("waic() scores two-level FIML fixed.x fits conditionally", {
  w <- suppressMessages(waic(fit_x_mlx, second_order = FALSE))
  res1 <- suppressMessages(loo(fit_x_mlx, second_order = FALSE))
  expect_equal(w$flavour, "conditional")
  expect_equal(w$per_unit$unit, res1$per_unit$unit)
  # First-order WAIC is the first-order LOO score
  expect_equal(unname(w$estimates["elpd_waic", "Estimate"]), res1$elpd_1)
  w_ml <- waic(fit_x_ml)
  expect_equal(w_ml$flavour, "conditional")
  expect_equal(
    unname(w_ml$estimates["elpd_waic", "Estimate"]),
    res_x_ml$elpd_2,
    tolerance = 0.01
  )
})
