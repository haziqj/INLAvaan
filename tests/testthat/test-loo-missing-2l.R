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

# MCAR holes in y1-y3; seed set immediately before so the dataset (and the
# pinned reference values) are reproducible
make_miss <- function() {
  d <- lavaan::Demo.twolevel[, c("y1", "y2", "y3", "cluster")]
  set.seed(20260613)
  for (v in c("y1", "y2", "y3")) {
    d[[v]][runif(nrow(d)) < 0.12] <- NA
  }
  d
}
d <- make_miss()

# a few rows are fully missing after punching holes; lavaan drops them with a
# (benign) note, suppressed here so the fixture sets up cleanly
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
  expect_equal(sum(is.na(d[, 1:3])), 904L)
  expect_equal(sum(!complete.cases(d[, 1:3])), 795L)
})

test_that("two-level FIML LOCO matches reference values", {
  # Reference values cross-checked against (i) lavaan's fitted two-level FIML
  # loglik, (ii) an independent dense marginal-covariance kernel, and (iii)
  # finite differences (lab graduated/2026-06-loo-missing/05-*.R)
  expect_equal(res$type, "loco")
  expect_equal(res$flavour, "joint")
  expect_equal(res$n_units, 200L)
  expect_equal(res$elpd_1, -11107.6378454856, tolerance = 1e-4)
  expect_equal(res$elpd_2, -11115.6523623652, tolerance = 1e-4)
  expect_equal(res$se_1, 355.1108972673, tolerance = 1e-4)
  expect_equal(res$se_2, 355.2910431850, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 15.3797379805, tolerance = 1e-2)
  expect_equal(res$p_loo_2, 16.4540289214, tolerance = 1e-2)

  pu <- res$per_unit[c(1L, 50L, 200L), ]
  expect_equal(pu$nobs, c(5L, 10L, 20L))
  expect_equal(
    pu$l_star,
    c(-19.2402315702, -49.1574789208, -84.6760346134),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-19.2447491860, -49.1945914624, -84.7073633870),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-19.2586816599, -49.2109180735, -84.7455888298),
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
  int <- get_inlavaan_internal(fit)
  minfo <- INLAvaan:::loco_missing_info(int)
  js <- c(1L, 50L, 200L)
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
  w <- suppressWarnings(waic(fit, nsamp = 500))
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, 200L)
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
    res_row <- loo(fit, type = "loso", units = 1:20),
    "leave-one-unit-out"
  )
  expect_equal(res_row$type, "loso")
  expect_equal(nrow(res_row$per_unit), 20L)
  expect_true(all(res_row$per_unit$nobs == 1L))

  # analytic per-row scores agree with finite differences, including rows in
  # clusters that contain a fully-missing row (lavaan's gradient kernel
  # mishandles zero-observed patterns; INLAvaan drops them before the kernel)
  int <- get_inlavaan_internal(fit)
  minfo <- INLAvaan:::loco_missing_info(int)
  rows <- c(1L, 5L, 200L, minfo$rows_by_cluster[[26L]])
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
  # regression guard for the fix: cluster 26 has a row with all within
  # variables missing; its score must match finite differences
  int <- get_inlavaan_internal(fit)
  minfo <- INLAvaan:::loco_missing_info(int)
  expect_true(any(minfo$n_obs < minfo$n_j)) # some cluster has a fully-missing row
  s_an <- INLAvaan:::loco_missing_scores_theta(
    int$theta_star,
    minfo,
    int$lavmodel,
    int$partable,
    26L
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
      (INLAvaan:::loco_missing_loglik_one(26L, minfo, cp$mom) -
        INLAvaan:::loco_missing_loglik_one(26L, minfo, cm$mom)) /
        (2 * h)
    },
    numeric(1)
  )
  expect_equal(max(abs(as.numeric(s_an) - s_fd)), 0, tolerance = 1e-5)
})
