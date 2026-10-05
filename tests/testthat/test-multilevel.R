## ----- Shared fit (complete data) --------------------------------------------
mod_ml <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"
fit_ml <- asem(
  mod_ml,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)

test_that("Multilevel: fit and summary", {
  fit_lav <- lavaan::sem(mod_ml, lavaan::Demo.twolevel, cluster = "cluster")

  expect_s4_class(fit_ml, "INLAvaan")
  expect_no_error(capture.output(summary(fit_ml)))
  expect_equal(coef(fit_ml), coef(fit_lav), tolerance = 0.1)
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(fit_ml@optim$dx, rep(0, length(coef(fit_ml))), tolerance = 1e-2)
})

test_that("Multilevel predict lv", {
  nsamp <- 5

  # Level 1
  pred1 <- predict(fit_ml, type = "lv", level = 1L, nsamp = nsamp)
  expect_length(pred1, nsamp)
  m1 <- pred1[[1]]
  expect_equal(nrow(m1), 2500)
  expect_true("fw" %in% colnames(m1))
  expect_true(ncol(m1) >= 1)

  # Level 2
  pred2 <- predict(fit_ml, type = "lv", level = 2L, nsamp = nsamp)
  expect_length(pred2, nsamp)
  m2 <- pred2[[1]]
  expect_equal(nrow(m2), 200)
  expect_true("fb" %in% colnames(m2))
  expect_true(ncol(m2) >= 1)
})

test_that("Multilevel predict yhat and ypred", {
  nsamp <- 5

  # yhat
  pred_yhat <- predict(fit_ml, type = "yhat", nsamp = nsamp)
  expect_length(pred_yhat, nsamp)
  m <- pred_yhat[[1]]
  expect_equal(nrow(m), 2500)
  expect_equal(ncol(m), 8)
  expect_true(all(
    c("y1", "y2", "y3", "x1", "x2", "x3", "w1", "w2") %in% colnames(m)
  ))
  expect_false(any(is.na(m)))

  # ypred
  pred_ypred <- predict(fit_ml, type = "ypred", nsamp = nsamp)
  expect_length(pred_ypred, nsamp)
  m2 <- pred_ypred[[1]]
  expect_equal(nrow(m2), 2500)
  expect_equal(ncol(m2), 8)
  expect_false(any(is.na(m2)))
})

test_that("Multilevel predict errors for unsupported options", {
  expect_error(
    predict(fit_ml, type = "lv", newdata = lavaan::Demo.twolevel, nsamp = 3),
    "not supported for multilevel"
  )
})

## ----- Missing data (FIML) fit -----------------------------------------------
test_that("Multilevel predict ymis works", {
  dat <- lavaan::Demo.twolevel
  dat <- dat[dat$cluster <= 10, ]
  set.seed(123)
  dat$y1[sample(nrow(dat), 10)] <- NA

  fit_miss <- asem(
    mod_ml,
    dat,
    cluster = "cluster",
    missing = "ML",
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    verbose = FALSE,
    nsamp = 3
  )

  nsamp <- 5

  # Full imputed dataset (default)
  pred <- predict(fit_miss, type = "ymis", nsamp = nsamp)
  expect_length(pred, nsamp)
  expect_false(any(is.na(pred[[1]])))
  expect_equal(ncol(pred[[1]]), 8L) # 8 model variables (y1-y3, x1-x3, w1-w2)

  # ymis_only: named vector of just the imputed cells
  pred_only <- predict(fit_miss, type = "ymis", nsamp = nsamp, ymis_only = TRUE)
  expect_length(pred_only, nsamp)
  v <- pred_only[[1]]
  expect_true(is.numeric(v))
  expect_length(v, 10L) # exactly the 10 NAs we injected
  expect_true(all(grepl("^y1\\[", names(v)))) # all from y1
})

## ----- Named levels ----------------------------------------------------------
test_that("Named levels give the same results as numbered levels", {
  mod_num <- "
    level: 1
      f =~ y1 + a*y2 + y3
      f ~ x1
    level: 2
      f =~ y1 + b*y2 + y3
      d := a - b
  "
  mod_named <- sub("level: 1", "level: within", mod_num)
  mod_named <- sub("level: 2", "level: between", mod_named)
  dat <- subset(lavaan::Demo.twolevel, cluster <= 40)
  fit_2l <- function(model) {
    set.seed(1)
    asem(
      model,
      dat,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      nsamp = 20
    )
  }
  fit_num <- fit_2l(mod_num)
  fit_named <- fit_2l(mod_named)
  to_num <- function(x) sub(".lbetween", ".l2", x, fixed = TRUE)

  # Names follow coef(), which suffixes later-level names with the level name
  fit_lav <- lavaan::sem(mod_named, dat, cluster = "cluster", do.fit = FALSE)
  expect_identical(names(coef(fit_named)), names(lavaan::coef(fit_lav)))
  expect_identical(to_num(names(coef(fit_named))), names(coef(fit_num)))
  expect_equal(unname(coef(fit_named)), unname(coef(fit_num)))
  expect_equal(unname(vcov(fit_named)), unname(vcov(fit_num)))
  summ_named <- get_inlavaan_internal(fit_named)$summary
  summ_num <- get_inlavaan_internal(fit_num)$summary
  expect_identical(to_num(rownames(summ_named)), rownames(summ_num))
  expect_equal(summ_named, summ_num, ignore_attr = TRUE)

  expect_no_error(capture.output(summary(fit_named)))
  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(plot(fit_named, params = c("d", "f~~f.lbetween")))

  # Each method draws from the same seed for both fits
  same_draws <- function(f, seed) {
    set.seed(seed)
    a <- f(fit_named)
    set.seed(seed)
    b <- f(fit_num)
    list(named = a, num = b)
  }
  std <- same_draws(function(x) standardisedsolution(x, nsamp = 10), 2)
  expect_equal(std$named$est.std, std$num$est.std)
  lat <- same_draws(function(x) sampling(x, type = "latent", nsamp = 3), 3)
  expect_true(all(c("f", "f.lbetween") %in% colnames(lat$named)))
  expect_identical(to_num(colnames(lat$named)), colnames(lat$num))
  expect_equal(unname(lat$named), unname(lat$num))
  pred <- same_draws(function(x) predict(x, level = 2L, nsamp = 2), 4)
  expect_equal(pred$named, pred$num)
  loos <- same_draws(function(x) loo(x, type = "loco")$estimates, 5)
  expect_equal(loos$named, loos$num)
  fms <- same_draws(function(x) fitmeasures(x), 6)
  expect_equal(unclass(fms$named), unclass(fms$num))
})

## ----- Zero residual variances -----------------------------------------------
test_that("Multilevel ypred works with residual variances fixed to zero", {
  mod <- "
    level: 1
      fw =~ y1 + y2 + y3
      y1 ~~ 0*y1
    level: 2
      fb =~ y1 + y2 + y3
      y1 ~~ 0*y1
  "
  fit <- asem(
    mod,
    subset(lavaan::Demo.twolevel, cluster <= 40),
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  # Same seed, same fitted values: y1 gets no noise at either level
  set.seed(1)
  yhat <- predict(fit, type = "yhat", nsamp = 3)[[1]]
  set.seed(1)
  ypred <- predict(fit, type = "ypred", nsamp = 3)[[1]]
  expect_equal(ypred[, "y1"], yhat[, "y1"])
  expect_true(all(ypred[, c("y2", "y3")] != yhat[, c("y2", "y3")]))
})

## ----- Residual noise in ypred -----------------------------------------------
# Draws of predict() with every posterior draw pinned at the fit's parameters
predict_pinned <- function(fit, type, nsamp) {
  x <- lavaan::lav_model_get_parameters(fit@Model)
  local_mocked_bindings(
    sample_params_posterior = function(int, nsamp, ...) {
      list(x_samp = matrix(x, nsamp, length(x), byrow = TRUE))
    }
  )
  set.seed(1)
  unclass(predict(fit, type = type, nsamp = nsamp))
}

test_that("Two-level ypred adds the residual variances lavInspect() reports", {
  # At fixed parameters yhat is fixed, so ypred - yhat is the residual. Its
  # variance sums the within and between diagonals of lavInspect(fit, "theta"),
  # which hold the residual variances of observed outcomes too.
  dat <- subset(lavaan::Demo.twolevel, cluster <= 30)
  dat$y4 <- dat$y1 + dat$x2
  dat$y5 <- 0.5 * dat$y3 + dat$x3
  mods <- list(
    # Outcome at both levels (y4), chain (y5), between-level outcome (w1)
    outcomes = "
      level: 1
        fw =~ y1 + y2 + y3
        y4 ~ fw + x1
        y5 ~ y4
      level: 2
        fb =~ y1 + y2 + y3
        y4 ~ fb
        w1 ~ fb + w2
    ",
    path = "
      level: 1
        y1 ~ x1 + x2
        y2 ~ y1
      level: 2
        y1 ~ w1
        y2 ~ y1
    ",
    # Indicator with a residual covariance with an observed outcome
    rescov = "
      level: 1
        fw =~ y1 + y2 + y3
        y4 ~ x1
        y1 ~~ y4
      level: 2
        fb =~ y1 + y2 + y3
    "
  )
  for (nm in names(mods)) {
    fit <- asem(
      mods[[nm]],
      dat,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      vb_correction = FALSE,
      marginal_method = "marggaus",
      nsamp = 3
    )
    yhat <- predict_pinned(fit, "yhat", 1)[[1]]
    eps <- simplify2array(lapply(predict_pinned(fit, "ypred", 200), `-`, yhat))
    v_emp <- colMeans(apply(eps, c(1, 2), var))
    v_theta <- v_emp * 0
    theta <- lavaan::lavInspect(fit, "theta")
    for (th in theta) {
      v_theta[rownames(th)] <- v_theta[rownames(th)] + diag(th)
    }
    ov_x <- unlist(lavaan::lavNames(fit, "ov.x", block = 1:2))
    expect_true(all(eps[, ov_x, ] == 0))
    for (v in setdiff(colnames(yhat), ov_x)) {
      expect_equal(
        v_emp[[v]],
        v_theta[[v]],
        tolerance = 0.1,
        label = paste(nm, v)
      )
    }
    if (nm == "outcomes") {
      # A between-level residual is shared by the rows of a cluster
      first <- match(dat$cluster, dat$cluster)
      expect_equal(eps[, "w1", ], eps[first, "w1", ])
    }
    if (nm == "rescov") {
      c_emp <- mean(eps[, "y1", ] * eps[, "y4", ])
      expect_equal(c_emp, theta$within["y1", "y4"], tolerance = 0.1)
    }
  }
})
