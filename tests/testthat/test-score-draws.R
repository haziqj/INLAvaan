hs <- lavaan::HolzingerSwineford1939
hs_mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

fit_quick <- function(model, data, ...) {
  acfa(
    model,
    data,
    silent = TRUE,
    nsamp = 20,
    vb_correction = FALSE,
    marginal_method = "gaussian",
    ...
  )
}

# The log-likelihood of each row of x_samp without the marginalised-means term,
# by the general route
general_loglik <- function(fit, x_samp) {
  int <- get_inlavaan_internal(fit)
  apply(x_samp, 1L, function(x) {
    inlav_model_loglik(
      x,
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata,
      reconstruct_lavoptions(fit),
      fit@Cache,
      marginalise_means = FALSE
    )
  })
}

score_fit <- function(fit, x_samp, ppp = FALSE) {
  int <- get_inlavaan_internal(fit)
  score_draws(
    x_samp,
    int$theta_star,
    int$partable,
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    reconstruct_lavoptions(fit),
    fit@Cache,
    fiml_setup(int$lavmodel, int$lavsamplestats, int$lavdata),
    multilevel_setup(int$lavmodel, int$lavsamplestats, int$lavdata, fit@Cache),
    ppp_plan = if (ppp) {
      ppp_setup(int$lavmodel, int$lavsamplestats, int$lavdata, int$partable)
    }
  )
}

test_that("The closed-form log-likelihood of complete data is lavaan's", {
  hs_x <- hs
  hs_x$sex <- hs_x$sex - 1
  fits <- list(
    plain = fit_quick(hs_mod, hs),
    means = fit_quick(hs_mod, hs, meanstructure = TRUE),
    groups = fit_quick(hs_mod, hs, group = "school"),
    covariate = fit_quick(paste(hs_mod, "visual ~ sex"), hs_x)
  )
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    int <- get_inlavaan_internal(fit)
    plan <- complete_loglik_setup(
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata,
      reconstruct_lavoptions(fit)
    )
    expect_false(is.null(plan), info = nm)
    x_samp <- sample_params_posterior(int, 5)$x_samp
    closed <- apply(x_samp, 1L, function(x) {
      complete_loglik(
        lavaan::lav_model_implied(
          lavaan::lav_model_set_parameters(int$lavmodel, x)
        ),
        plan
      )
    })
    expect_equal(closed, general_loglik(fit, x_samp), tolerance = 1e-10)
  }
})

test_that("The closed form does not apply to missing data", {
  hs_na <- hs
  hs_na$x1[1:20] <- NA
  fit <- fit_quick(hs_mod, hs_na, missing = "ml")
  int <- get_inlavaan_internal(fit)
  expect_null(complete_loglik_setup(
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    reconstruct_lavoptions(fit)
  ))
})

test_that("A non-positive-definite draw has a failed log-likelihood", {
  fit <- fit_quick(hs_mod, hs)
  int <- get_inlavaan_internal(fit)
  plan <- complete_loglik_setup(
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    reconstruct_lavoptions(fit)
  )
  implied <- lavaan::lav_model_implied(int$lavmodel)
  implied$cov[[1]][1, 2] <- implied$cov[[1]][2, 1] <- 1e3
  expect_identical(complete_loglik(implied, plan), -1e40)
})

test_that("The stored log-likelihoods give the chi-squares of the draws", {
  hs_na <- hs
  hs_na$x1[1:20] <- NA
  fits <- list(
    complete = fit_quick(hs_mod, hs),
    missing = fit_quick(hs_mod, hs_na, missing = "ml")
  )
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    int <- get_inlavaan_internal(fit)
    x_samp <- sample_params_posterior(int, 15)$x_samp
    scored <- score_fit(fit, x_samp)
    args <- list(
      fit,
      x_samp,
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata,
      reconstruct_lavoptions(fit),
      fit@Cache
    )
    expect_equal(
      do.call(
        compute_chisq_dev,
        c(args, list(loglik_samp = scored$dic$loglik_samp))
      ),
      do.call(compute_chisq_dev, args),
      tolerance = 1e-10,
      info = nm
    )
  }
})

test_that("The DIC of score_draws() averages the deviances of the draws", {
  fit <- fit_quick(hs_mod, hs)
  int <- get_inlavaan_internal(fit)
  x_samp <- sample_params_posterior(int, 15)$x_samp
  dic <- score_fit(fit, x_samp)$dic
  ll <- general_loglik(fit, x_samp)
  corr <- apply(x_samp, 1L, function(x) {
    marginalised_means_loglik_corr(
      lavaan::lav_model_implied(
        lavaan::lav_model_set_parameters(int$lavmodel, x)
      ),
      int$lavsamplestats
    )
  })
  expect_equal(dic$Dbar, mean(-2 * (ll + corr)), tolerance = 1e-10)
  expect_equal(dic$pD, dic$Dbar - dic$Dhat)
  expect_equal(dic$dic, dic$Dbar + dic$pD)
})

test_that("Scoring the DIC in the same pass leaves the PPP as it was", {
  fit <- fit_quick(hs_mod, hs)
  int <- get_inlavaan_internal(fit)
  x_samp <- sample_params_posterior(int, 30)$x_samp
  plan <- ppp_setup(int$lavmodel, int$lavsamplestats, int$lavdata)
  set.seed(7)
  ppp_alone <- mean(vapply(
    seq_len(nrow(x_samp)),
    function(i) {
      lavmodel_x <- lavaan::lav_model_set_parameters(int$lavmodel, x_samp[i, ])
      ppp_draw(
        x_samp[i, ],
        lavmodel_x,
        lavaan::lav_model_implied(lavmodel_x),
        int$lavmodel,
        plan
      )
    },
    numeric(1)
  ))
  set.seed(7)
  expect_identical(score_fit(fit, x_samp, ppp = TRUE)$ppp, ppp_alone)
})

test_that("The independence baseline is scored at once as by the general route", {
  hs_na <- hs
  hs_na$x1[1:20] <- NA
  hs_na$x5[30:45] <- NA
  base_mod <- paste0("x", 1:6, " ~~ x", 1:6, collapse = "\n")
  fits <- list(
    complete = fit_quick(base_mod, hs),
    means = fit_quick(base_mod, hs, meanstructure = TRUE),
    groups = fit_quick(base_mod, hs, group = "school", meanstructure = TRUE),
    missing = fit_quick(base_mod, hs_na, missing = "ml")
  )
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    int <- get_inlavaan_internal(fit)
    plan <- independence_loglik_setup(
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata,
      int$partable
    )
    expect_false(is.null(plan), info = nm)
    x_samp <- sample_params_posterior(int, 10)$x_samp
    expect_equal(
      independence_loglik(x_samp, plan)$raw,
      general_loglik(fit, x_samp),
      tolerance = 1e-10,
      info = nm
    )
  }
})

test_that("A model with a covariance is not scored as an independence model", {
  fit <- fit_quick(hs_mod, hs)
  int <- get_inlavaan_internal(fit)
  expect_null(independence_loglik_setup(
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    int$partable
  ))
})

test_that("The absolute fit indices use the draws of the fit, free of the seed", {
  fit <- fit_quick(hs_mod, hs)
  absolute <- c("BRMSEA", "BGammaHat", "adjBGammaHat", "BMc")
  set.seed(1)
  a <- bfit_indices(fit)
  set.seed(2)
  b <- bfit_indices(fit)
  expect_identical(a$indices[absolute], b$indices[absolute])
  expect_identical(a$details$chisq, b$details$chisq)
  expect_identical(a$details$nsamp, 20L)
  # Fewer draws than the fit has take the first of them
  set.seed(3)
  fewer <- bfit_indices(fit, nsamp = 10)
  expect_identical(fewer$details$nsamp, 10L)
  expect_identical(fewer$details$chisq, a$details$chisq[1:10])
  # More draws than the fit has draw anew
  set.seed(1)
  c1 <- bfit_indices(fit, nsamp = 30)
  set.seed(2)
  c2 <- bfit_indices(fit, nsamp = 30)
  expect_identical(c1$details$nsamp, 30L)
  expect_false(identical(c1$indices$BRMSEA, c2$indices$BRMSEA))
})

test_that("The independence baseline is fitted once and kept with the fit", {
  fit <- fit_quick(hs_mod, hs)
  n_fits <- 0L
  baseline_original <- fit_independence_baseline
  local_mocked_bindings(
    fit_independence_baseline = function(object, nsamp = NULL) {
      n_fits <<- n_fits + 1L
      baseline_original(object, nsamp)
    }
  )
  set.seed(1)
  a <- bfit_indices(fit)
  set.seed(2)
  b <- bfit_indices(fit)
  set.seed(3)
  fm <- fitMeasures(fit)
  expect_identical(n_fits, 1L)
  # So the incremental indices no longer depend on the seed either
  expect_identical(a$indices, b$indices)
  expect_equal(unname(fm["BCFI"]), mean(a$indices$BCFI))
  # Fewer draws use the kept baseline, more draws fit a new one
  fewer <- bfit_indices(fit, nsamp = 10)
  expect_identical(n_fits, 1L)
  expect_identical(fewer$indices$BCFI, a$indices$BCFI[1:10])
  bfit_indices(fit, nsamp = 30)
  expect_identical(n_fits, 2L)
  # The kept record is small: no data, no fit object
  rec <- fit@external$cache$independence
  expect_type(rec, "list")
  expect_lt(as.numeric(utils::object.size(rec)), 2e4)
})

test_that("compare() uses the baseline kept with its first model", {
  fit <- fit_quick(hs_mod, hs)
  fit_1f <- fit_quick("g =~ x1 + x2 + x3 + x4 + x5 + x6", hs)
  n_fits <- 0L
  baseline_original <- fit_independence_baseline
  local_mocked_bindings(
    fit_independence_baseline = function(object, nsamp = NULL) {
      n_fits <<- n_fits + 1L
      baseline_original(object, nsamp)
    }
  )
  fm <- fitMeasures(fit, c("BCFI", "BTLI"))
  cmp <- compare(fit, fit_1f, fit.measures = c("BCFI", "BTLI"))
  expect_identical(n_fits, 1L)
  expect_equal(cmp$BCFI[cmp$Model == "fit"], round(unname(fm[["BCFI"]]), 4))
  # The second model is scaled against the same kept baseline
  rec <- fit@external$cache$independence
  expect_s3_class(rec, "inlavaan_baseline_record")
  bf <- bfit_indices(fit_1f, baseline.model = rec)
  expect_equal(
    cmp$BCFI[cmp$Model == "fit_1f"],
    round(mean(bf$indices$BCFI), 4)
  )
  compare(fit, fit_1f, fit.measures = "BCFI")
  expect_identical(n_fits, 1L)
})
