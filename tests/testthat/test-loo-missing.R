# FIML LOO (single-level, missing data). Under FIML the fitted likelihood is
# the observed-data likelihood, so each unit is scored on the entries it
# actually has, l_i = log N(y_i,obs; mu[o_i], Sigma[o_i, o_i]); the Taylor
# machinery is unchanged save for pattern-aware casewise kernels.

HS_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

# A 70-row subsample (bigger than a plain complete-data fixture, since 12%
# MCAR holes across 6 columns still need to yield a genuine mix of complete,
# single-hole, and multi-hole rows plus several distinct FIML patterns).
# MCAR holes in x4-x9 (x1-x3 kept complete so every row has >= 3 observed
# entries); the seed is set immediately before the holes so the dataset is
# reproducible and the pinned reference values are stable
make_miss <- function() {
  set.seed(1)
  d <- lavaan::HolzingerSwineford1939[
    sample(nrow(lavaan::HolzingerSwineford1939), 70),
    paste0("x", 1:9)
  ]
  set.seed(20260613)
  for (v in paste0("x", 4:9)) {
    d[[v]][runif(nrow(d)) < 0.12] <- NA
  }
  d
}
dat <- make_miss()

fit <- acfa(
  HS_model,
  dat,
  meanstructure = TRUE,
  missing = "ml",
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
res <- loo(fit)

test_that("the test dataset has the expected missingness", {
  expect_equal(sum(is.na(dat)), 55L)
  expect_equal(sum(!complete.cases(dat)), 41L)
  X <- get_inlavaan_internal(fit)$lavdata@X[[1L]]
  expect_equal(nrow(X), 70L) # no rows dropped under FIML
  expect_true(anyNA(X)) # NAs retained, not listwise-deleted
  expect_length(INLAvaan:::fiml_patterns(X), 18L)
})

test_that("FIML LOSO matches reference values", {
  # Reference values from an independent prototype of the same observed-data
  # Taylor LOO formulas on this exact fit (lab 02-package-validation.R),
  # cross-checked below against lavaan's FIML loglik and finite differences
  expect_equal(res$type, "loso")
  expect_equal(res$flavour, "joint")
  expect_equal(res$n_units, 70L)
  expect_equal(res$elpd_1, -810.8096330551, tolerance = 1e-4)
  expect_equal(res$elpd_2, -829.4835098090, tolerance = 1e-4)
  expect_equal(res$se_1, 21.8869310500, tolerance = 1e-4)
  expect_equal(res$se_2, 22.5837971790, tolerance = 1e-4)
  expect_equal(res$p_loo_1, 27.7612891234, tolerance = 1e-2)
  expect_equal(res$p_loo_2, 31.1295585209, tolerance = 1e-2)

  # rows spanning complete (4), one hole (2), and three holes (11)
  pu <- res$per_unit[c(4L, 2L, 11L), ]
  expect_equal(pu$unit, c(4L, 2L, 11L))
  expect_equal(
    pu$l_star,
    c(-10.03278944439, -10.69608891448, -8.96709021099),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_1,
    c(-10.09565853301, -10.79854756016, -9.08394011848),
    tolerance = 1e-4
  )
  expect_equal(
    pu$log_cpo_2,
    c(-10.23563590292, -10.94860225615, -9.19913075203),
    tolerance = 1e-4
  )
  expect_equal(
    pu$det_term,
    c(-0.13882114800, -0.14444389100, -0.10621393414),
    tolerance = 1e-3
  )
})

test_that("casewise observed-data logliks sum to the fitted FIML loglik", {
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

test_that("pattern-scattered scores match finite differences on missing rows", {
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  int <- get_inlavaan_internal(fit)
  dv <- INLAvaan:::loso_data_view(
    int$lavmodel,
    int$lavdata,
    x_idx = int$lavsamplestats@x.idx
  )
  uv <- INLAvaan:::loso_resolve_units(int$lavdata, NULL)
  X <- int$lavdata@X[[1L]]
  miss_count <- rowSums(is.na(X))
  # complete, one-hole, and a maximally sparse row
  i_chk <- c(
    which(miss_count == 0L)[1L],
    which(miss_count == 1L)[1L],
    which(miss_count == max(miss_count))[1L]
  )
  s_an <- INLAvaan:::loso_scores_units(
    int$theta_star,
    uv,
    dv,
    int$lavmodel,
    int$partable
  )[i_chk, ]
  h <- 1e-6
  g_num <- vapply(
    seq_along(int$theta_star),
    function(k) {
      tp <- tm <- int$theta_star
      tp[k] <- tp[k] + h
      tm[k] <- tm[k] - h
      cp <- INLAvaan:::loo_grad_cache(tp, int$lavmodel, int$partable)
      cm <- INLAvaan:::loo_grad_cache(tm, int$lavmodel, int$partable)
      (INLAvaan:::loso_loglik_units(uv, dv, cp$mom)[i_chk] -
        INLAvaan:::loso_loglik_units(uv, dv, cm$mom)[i_chk]) /
        (2 * h)
    },
    numeric(length(i_chk))
  )
  expect_equal(max(abs(s_an - g_num)), 0, tolerance = 1e-6)
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
  expect_equal(
    res$per_unit$lpd_1 + res$per_unit$log_cpo_1,
    2 * res$per_unit$l_star
  )
  expect_equal(
    unname(res$estimates["elpd_loo", "Estimate"]),
    res$elpd_2
  )

  # unit subsetting addresses case numbers and agrees with the full run
  res10 <- loo(fit, units = 1:10)
  expect_equal(nrow(res10$per_unit), 10L)
  expect_equal(
    res10$per_unit$log_cpo_2,
    res$per_unit$log_cpo_2[1:10],
    tolerance = 1e-8
  )
})

test_that("waic() runs on a FIML fit and agrees loosely with loo()", {
  set.seed(1)
  w <- suppressWarnings(waic(fit, nsamp = 150))
  expect_s3_class(w, "inlavaan_waic")
  expect_equal(w$n_units, 70L)
  expect_equal(w$type, "loso")
  expect_equal(w$flavour, "joint")
  expect_true(all(is.finite(w$per_unit$lpd)))
  # WAIC and LOO estimate the same quantity; loose agreement on this model
  expect_equal(
    unname(w$estimates["elpd_waic", "Estimate"]),
    res$elpd_2,
    tolerance = 0.01
  )
})

# Two-level FIML (per-cluster LOCO) is supported; see test-loo-missing-2l.R
# for the reference-pinned coverage. Only the per-row deletion override
# remains gated under missing data.
