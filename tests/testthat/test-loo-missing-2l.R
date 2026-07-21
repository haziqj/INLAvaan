# Two-level FIML LOCO (leave-one-cluster-out under missing data). Each cluster
# is scored on its observed-data marginal likelihood via lavaan's raw-data
# missing kernels; since LOCO deletes a whole cluster there is no downdating.
# The flavour is joint (lavaan rejects two-level + conditional.x).

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
  # finite differences (lab graduated/2026-06-loo-missing/05-*.R). This
  # dataset has fully-missing-within cases, so the fitted mode (and the LOO
  # values pinned to it) shifts slightly between lavaan versions: lavaan <
  # 0.7-1.2707 optimises with the inexact gradient (lavaan PR #581), patched
  # lavaan with the correct one. The values below are pinned on the buggy
  # gradient; skip them on patched lavaan (the version-invariant identities
  # and finite-difference checks below still run there).
  skip_if(
    utils::packageVersion("lavaan") >= "0.7.1.2707",
    "fit mode differs once the lavaan gradient is patched"
  )
  expect_equal(res$type, "loco")
  expect_equal(res$flavour, "joint")
  expect_equal(res$n_units, 30L)
  expect_equal(res$elpd_1, -1622.0173425091, tolerance = 1e-4)
  expect_equal(res$elpd_2, -1631.5418961566, tolerance = 1e-4)
  expect_equal(res$se_1, 146.6123016390, tolerance = 1e-4)
  expect_equal(res$se_2, 147.4268836230, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 14.2027032692, tolerance = 1e-2)
  expect_equal(res$p_loo_2, 16.3011978533, tolerance = 1e-2)

  # first, middle, and last of the 30 clusters
  pu <- res$per_unit[c(1L, 15L, 30L), ]
  expect_equal(pu$nobs, c(5L, 15L, 10L))
  expect_equal(
    pu$l_star,
    c(-16.9345018087, -69.6199644963, -42.7357136336),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-16.9717923529, -69.7909182029, -42.8737272647),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-17.0345397512, -70.1542742739, -43.0387010435),
    tolerance = 1e-4
  )
})

test_that("per-cluster observed-data logliks sum to the fitted FIML loglik", {
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
  set.seed(1)
  w <- suppressWarnings(waic(fit, nsamp = 50))
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
  # clusters that contain a fully-missing row (lavaan's gradient kernel
  # mishandles zero-observed patterns; INLAvaan drops them before the kernel).
  # Skipped on CRAN: this agreement is sensitive to BLAS/compiler differences
  # across check flavours.
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
