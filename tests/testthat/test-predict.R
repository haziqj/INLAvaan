dat     <- lavaan::HolzingerSwineford1939
sem_dat <- lavaan::PoliticalDemocracy
NSAMP   <- 3

mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
sem_mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem60 ~ ind60
"

# Fit once; reused across all tests below (fast defaults)
fit_cfa <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP,
                vb_correction = FALSE, test = "none",
                marginal_method = "marggaus")
fit_mg  <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, group = "school",
                vb_correction = FALSE, test = "none",
                marginal_method = "marggaus")
fit_sem <- asem(sem_mod, sem_dat, verbose = FALSE, nsamp = NSAMP,
                vb_correction = FALSE, test = "none",
                marginal_method = "marggaus")

# ---- CFA: type = "lv" (default) -----------------------------------------

test_that("type = 'lv' returns correctly-shaped latent predictions", {
  prd  <- predict(fit_cfa, nsamp = NSAMP)
  summ <- summary(prd)
  expect_no_error(capture.output(print(prd)))
  expect_no_error(capture.output(print(summ)))
  expect_equal(length(prd),     NSAMP)
  expect_equal(nrow(summ$Mean), nrow(dat))
})

# ---- CFA: type = "yhat" / "ov" and "ypred" / "ydist" -------------------

test_that("type = 'yhat' and alias 'ov' return n x p fitted-mean matrices", {
  prd_yhat <- predict(fit_cfa, type = "yhat", nsamp = NSAMP)
  prd_ov   <- predict(fit_cfa, type = "ov",   nsamp = NSAMP)
  expect_equal(length(prd_yhat),    NSAMP)
  expect_equal(nrow(prd_yhat[[1]]), nrow(dat))
  expect_equal(ncol(prd_yhat[[1]]), 6L)
  expect_equal(dim(prd_ov[[1]]),    dim(prd_yhat[[1]]))
})

test_that("type = 'ypred' and alias 'ydist' return n x p predicted matrices", {
  prd_ypred <- predict(fit_cfa, type = "ypred", nsamp = NSAMP)
  prd_ydist <- predict(fit_cfa, type = "ydist", nsamp = NSAMP)
  expect_equal(length(prd_ypred),    NSAMP)
  expect_equal(nrow(prd_ypred[[1]]), nrow(dat))
  expect_equal(ncol(prd_ypred[[1]]), 6L)
  expect_equal(dim(prd_ydist[[1]]),  dim(prd_ypred[[1]]))
})

# ---- SEM (B-matrix path): lv, yhat, ypred --------------------------------

test_that("SEM predict covers lv, yhat, and ypred types", {
  prd_lv    <- predict(fit_sem, nsamp = NSAMP)
  prd_yhat  <- predict(fit_sem, type = "yhat",  nsamp = NSAMP)
  prd_ypred <- predict(fit_sem, type = "ypred", nsamp = NSAMP)
  n <- nrow(sem_dat)
  expect_equal(nrow(summary(prd_lv)$Mean), n)
  expect_equal(nrow(prd_yhat[[1]]),         n)
  expect_equal(ncol(prd_yhat[[1]]),         7L)
  expect_equal(nrow(prd_ypred[[1]]),        n)
})

# ---- newdata -------------------------------------------------------------

test_that("newdata works for lv, yhat, and ypred types", {
  newdat <- dat[1:5, ]
  expect_equal(nrow(predict(fit_cfa, type = "lv",    newdata = newdat, nsamp = NSAMP)[[1]]), 5L)
  expect_equal(nrow(predict(fit_cfa, type = "yhat",  newdata = newdat, nsamp = NSAMP)[[1]]), 5L)
  expect_equal(nrow(predict(fit_cfa, type = "ypred", newdata = newdat, nsamp = NSAMP)[[1]]), 5L)
})

test_that("type = 'ymis' with newdata throws an error", {
  expect_error(
    predict(fit_cfa, type = "ymis", newdata = dat[1:5, ], nsamp = NSAMP),
    "ymis.*newdata"
  )
})

# ---- Multigroup: lv, yhat, newdata ---------------------------------------

test_that("multigroup predict works for lv, yhat, and newdata", {
  prd      <- predict(fit_mg, nsamp = NSAMP)
  prd_yhat <- predict(fit_mg, type = "yhat", nsamp = NSAMP)
  newdat   <- rbind(
    dat[dat$school == "Pasteur",     ][1:3, ],
    dat[dat$school == "Grant-White", ][1:2, ]
  )
  prd_new  <- predict(fit_mg, type = "lv", newdata = newdat, nsamp = NSAMP)
  expect_equal(nrow(summary(prd)$Mean),      nrow(dat))
  expect_equal(nrow(summary(prd_yhat)$Mean), nrow(dat))
  expect_equal(nrow(summary(prd_new)$Mean),  5L)
})

# ---- summary = TRUE shortcut ---------------------------------------------

test_that("predict(summary = TRUE) matches summary(predict(...))", {
  skip_on_cran()
  set.seed(1)
  a <- predict(fit_cfa, type = "yhat", nsamp = NSAMP, summary = TRUE)
  set.seed(1)
  b <- summary(predict(fit_cfa, type = "yhat", nsamp = NSAMP))
  expect_s3_class(a, "summary.predict.inlavaan_internal")
  expect_equal(a, b)
})

test_that("predict() default (summary = FALSE) still returns raw draws", {
  prd <- predict(fit_cfa, type = "yhat", nsamp = NSAMP)
  expect_s3_class(prd, "predict.inlavaan_internal")
  expect_equal(length(prd), NSAMP)
})

test_that("Factor scores are centred on the implied/saturated means", {
  # Regression test: predict() used to condition on raw y instead of
  # y - mu_y, offsetting every factor score by Phi Lambda' Sigma^{-1} mu_y
  # (several sd on uncentred data), under both meanstructure settings.
  fit50 <- acfa(mod, dat, verbose = FALSE, nsamp = 50,
                vb_correction = FALSE, test = "none")
  draws <- unclass(predict(fit50, type = "lv"))
  fs <- Reduce(`+`, draws) / length(draws)
  fs_lav <- lavaan::lavPredict(lavaan::cfa(mod, dat))
  for (k in seq_len(ncol(fs_lav))) {
    expect_lt(max(abs(fs[, k] - fs_lav[, k])) / sd(fs_lav[, k]), 0.6)
  }
})

# ---- Regression: predict() draws exactly as the fit does -----------------

test_that("predict() passes R_star and honours the fit's samp_copula", {
  orig <- sample_params
  args <- NULL
  local_mocked_bindings(
    sample_params = function(...) {
      args <<- list(...)
      orig(...)
    }
  )

  fit_sn <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP,
                 vb_correction = FALSE, test = "none",
                 marginal_method = "skewnorm", samp_norta = TRUE)
  int_sn <- get_inlavaan_internal(fit_sn)
  expect_false(is.null(int_sn$R_star))
  expect_true(isTRUE(int_sn$samp_norta))

  args <- NULL
  predict(fit_sn, nsamp = NSAMP)
  expect_identical(args$method, "skewnorm")
  expect_identical(args$R_star, int_sn$R_star)

  # samp_copula = FALSE is recorded on the fit and inherited by predict()
  fit_nc <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP,
                 vb_correction = FALSE, test = "none",
                 marginal_method = "skewnorm", samp_copula = FALSE)
  args <- NULL
  predict(fit_nc, nsamp = NSAMP)
  expect_identical(args$method, "sampling")
})

# ---- Observed covariates and observed endogenous variables ----------------

# lavaan carries these as dummy latent variables, which are known given the
# data and so have zero conditional variance.
cov_mod <- "
  visual =~ x1 + x2 + x3
  visual ~ x4 + sex
"
xs <- c("x4", "sex")
fit_quick <- function(model, data = dat, ...) {
  asem(
    model,
    data,
    ...,
    verbose = FALSE,
    nsamp = NSAMP,
    vb_correction = FALSE,
    test = "none",
    marginal_method = "marggaus"
  )
}

# Average of the predict() draws with every posterior draw pinned at x
predict_at <- function(fit, x, type = "lv", nsamp = 200, ...) {
  local_mocked_bindings(
    sample_params_posterior = function(int, nsamp, ...) {
      list(x_samp = matrix(x, nsamp, length(x), byrow = TRUE))
    }
  )
  set.seed(1)
  Reduce(`+`, unclass(predict(fit, type = type, nsamp = nsamp, ...))) / nsamp
}
rel_err <- function(a, b) max(abs(a - b)) / sd(b)

test_that("predict() works for every type with observed covariates", {
  newdat <- dat[1:5, ]
  for (fx in c(TRUE, FALSE)) {
    fit <- fit_quick(cov_mod, fixed.x = fx)
    for (nd in list(NULL, newdat)) {
      x_data <- unname(as.matrix(if (is.null(nd)) dat[, xs] else nd[, xs]))
      lv <- predict(fit, type = "lv", newdata = nd, nsamp = NSAMP)
      set.seed(1)
      yhat <- predict(fit, type = "yhat", newdata = nd, nsamp = NSAMP)
      set.seed(1)
      ypred <- predict(fit, type = "ypred", newdata = nd, nsamp = NSAMP)
      expect_equal(colnames(lv[[1]]), c("visual", xs))
      expect_equal(colnames(yhat[[1]]), c("x1", "x2", "x3", xs))
      expect_equal(colnames(ypred[[1]]), c("x1", "x2", "x3", xs))
      for (prd in list(lv, yhat, ypred)) {
        expect_equal(unname(prd[[2]][, xs]), x_data)
      }
      expect_false(isTRUE(all.equal(lv[[1]][, 1], lv[[2]][, 1])))
      # Same seed, same factor scores: ypred adds noise to the indicators only
      noise <- ypred[[1]][, 1:3] - yhat[[1]][, 1:3]
      expect_true(all(abs(noise) > 0))
    }
  }
})

test_that("predict() works with observed covariates in several groups", {
  fit <- fit_quick(cov_mod, group = "school")
  newdat <- rbind(
    dat[dat$school == "Pasteur", ][1:3, ],
    dat[dat$school == "Grant-White", ][1:2, ]
  )
  # Rows come group by group
  by_group <- dat[order(match(dat$school, fit@Data@group.label)), xs]
  for (tp in c("lv", "yhat", "ypred")) {
    prd <- predict(fit, type = tp, nsamp = NSAMP)
    expect_equal(nrow(prd[[1]]), nrow(dat))
    expect_equal(unname(as.matrix(prd[[1]][, xs])), unname(as.matrix(by_group)))
    prd_new <- predict(fit, type = tp, newdata = newdat, nsamp = NSAMP)
    expect_equal(nrow(prd_new[[1]]), 5L)
  }
})

test_that("Predictions with observed covariates match lavaan", {
  for (ms in c(FALSE, TRUE)) {
    fit <- fit_quick(cov_mod, meanstructure = ms)
    fit_lav <- lavaan::sem(cov_mod, dat, meanstructure = ms)
    expect_equal(names(coef(fit)), names(coef(fit_lav)))
    x_lav <- lavaan::lav_model_get_parameters(fit_lav@Model)
    fs <- predict_at(fit, x_lav)
    yhat <- predict_at(fit, x_lav, "yhat")
    fs_lav <- lavaan::lavPredict(fit_lav)
    yhat_lav <- lavaan::lavPredict(fit_lav, type = "ov")
    expect_lt(rel_err(fs[, "visual"], fs_lav[, "visual"]), 0.25)
    for (v in c("x1", "x2", "x3")) {
      expect_lt(rel_err(yhat[, v], yhat_lav[, v]), 0.25)
    }
  }
})

test_that("predict() uses the covariates with conditional.x = TRUE", {
  # lavaan keeps their effects in Gamma rather than in dummy latent variables
  fit <- fit_quick(cov_mod, conditional.x = TRUE)
  fit_lav <- lavaan::sem(cov_mod, dat, conditional.x = TRUE)
  x_lav <- lavaan::lav_model_get_parameters(fit_lav@Model)
  newdat <- dat[1:20, ]
  for (nd in list(NULL, newdat)) {
    fs <- predict_at(fit, x_lav, newdata = nd)
    yhat <- predict_at(fit, x_lav, "yhat", newdata = nd)
    nd_lav <- if (is.null(nd)) dat else nd
    fs_lav <- lavaan::lavPredict(fit_lav, newdata = nd_lav)
    yhat_lav <- lavaan::lavPredict(fit_lav, type = "ov", newdata = nd_lav)
    expect_equal(colnames(fs), "visual")
    expect_lt(rel_err(fs[, "visual"], fs_lav[, "visual"]), 0.25)
    for (v in c("x1", "x2", "x3")) {
      expect_lt(rel_err(yhat[, v], yhat_lav[, v]), 0.25)
    }
  }
})

test_that("Observed endogenous variables are predicted from their regressors", {
  mods <- list(
    x4 = "visual =~ x1 + x2 + x3\n x4 ~ visual + ageyr",
    x1 = "visual =~ x1 + x2 + x3\n x1 ~ ageyr"
  )
  for (v in names(mods)) {
    fit <- fit_quick(mods[[v]], meanstructure = TRUE)
    fit_lav <- lavaan::sem(mods[[v]], dat, meanstructure = TRUE)
    x_lav <- lavaan::lav_model_get_parameters(fit_lav@Model)
    yhat <- predict_at(fit, x_lav, "yhat")
    est <- coef(fit_lav)
    fs_lav <- lavaan::lavPredict(fit_lav)[, "visual"]
    slope <- if (v == "x1") 1 else est[["x4~visual"]]
    y_hat <- est[[paste0(v, "~1")]] +
      slope * fs_lav +
      est[[paste0(v, "~ageyr")]] * dat$ageyr
    expect_lt(rel_err(yhat[, v], y_hat), 0.25)
    # Their residual variance is drawn in ypred
    set.seed(1)
    yhat <- predict(fit, type = "yhat", nsamp = NSAMP)[[1]][, v]
    set.seed(1)
    ypred <- predict(fit, type = "ypred", nsamp = NSAMP)[[1]][, v]
    expect_equal(
      sd(ypred - yhat),
      sqrt(est[[paste0(v, "~~", v)]]),
      tolerance = 0.25
    )
  }
  # No latent variables at all
  fit <- fit_quick("x4 ~ x1 + ageyr")
  for (tp in c("lv", "yhat", "ypred")) {
    expect_equal(nrow(predict(fit, type = tp, nsamp = NSAMP)[[1]]), nrow(dat))
  }
})

test_that("Fitted values of a latent regression match lavaan", {
  # Regression test: yhat applied (I - B)^{-1} to factor scores that already
  # carry the structural effects.
  fit_lav <- lavaan::sem(sem_mod, sem_dat)
  x_lav <- lavaan::lav_model_get_parameters(fit_lav@Model)
  yhat <- predict_at(fit_sem, x_lav, "yhat")
  yhat_lav <- lavaan::lavPredict(fit_lav, type = "ov")
  for (v in colnames(yhat_lav)) {
    expect_lt(rel_err(yhat[, v], yhat_lav[, v]), 0.25)
  }
})
