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

predict_pinned_level2 <- function(fit, nsamp) {
  x <- lavaan::lav_model_get_parameters(fit@Model)
  local_mocked_bindings(
    sample_params_posterior = function(int, nsamp, ...) {
      list(x_samp = matrix(x, nsamp, length(x), byrow = TRUE))
    }
  )
  set.seed(1)
  unclass(predict(fit, type = "lv", level = 2L, nsamp = nsamp))
}

# Exact conditional moments, given theta, of the random quantities of cluster
# 1, by conditioning the cluster's full joint Gaussian on its observed data.
# Returns a function giving the mean and variance of a' X + c, where X stacks
# (eta_w, w) for each row and then (eta_b, u).
dense_cluster_1 <- function(fit, x) {
  int <- get_inlavaan_internal(fit)
  lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, x)
  imp <- lavaan::lav_model_implied(lm_x)
  mom <- ml_moments(lm_x, int$lavsamplestats)
  Lp <- int$lavdata@Lp[[1]]
  y <- int$lavdata@X[[1]]
  idx1 <- Lp$ov.idx[[1]]
  idx2 <- Lp$ov.idx[[2]]
  A <- matrix(0, length(idx1), length(idx2))
  sh <- match(idx1, idx2)
  A[cbind(which(!is.na(sh)), sh[!is.na(sh)])] <- 1
  zp <- which(!idx2 %in% idx1)
  rows <- which(Lp$cluster.idx[[2]] == 1)
  Lw <- mom$lambda[[1]]
  Vw <- mom$veta[[1]]
  Lb <- mom$lambda[[2]]
  Vb <- mom$veta[[2]]
  mw <- ncol(Vw)
  kw <- mw + length(idx1)
  K <- length(rows) * kw + ncol(Vb) + length(idx2)
  mu <- numeric(K)
  C <- matrix(0, K, K)
  for (i in seq_along(rows)) {
    s <- (i - 1) * kw + seq_len(kw)
    mu[s] <- c(mom$eeta[[1]], imp$mean[[1]])
    C[s, s] <- rbind(cbind(Vw, Vw %*% t(Lw)), cbind(Lw %*% Vw, imp$cov[[1]]))
  }
  sb <- length(rows) * kw + seq_len(ncol(Vb) + length(idx2))
  mu[sb] <- c(mom$eeta[[2]], imp$mean[[2]])
  C[sb, sb] <- rbind(cbind(Vb, Vb %*% t(Lb)), cbind(Lb %*% Vb, imp$cov[[2]]))
  ui <- length(rows) * kw + ncol(Vb) + seq_along(idx2)
  M <- NULL
  d <- NULL
  for (i in seq_along(rows)) {
    for (k in seq_along(idx1)) {
      if (!is.na(y[rows[i], idx1[k]])) {
        r <- numeric(K)
        r[(i - 1) * kw + mw + k] <- 1
        r[ui] <- A[k, ]
        M <- rbind(M, r)
        d <- c(d, y[rows[i], idx1[k]])
      }
    }
  }
  for (q in zp) {
    r <- numeric(K)
    r[ui[q]] <- 1
    M <- rbind(M, r)
    d <- c(d, y[rows[1], idx2[q]])
  }
  G <- C %*% t(M) %*% solve(M %*% C %*% t(M))
  m <- mu + G %*% (d - M %*% mu)
  V <- C - G %*% M %*% C
  list(
    rows = rows,
    kw = kw,
    mw = mw,
    ui = ui,
    A = A,
    idx1 = idx1,
    Lw = Lw,
    eeta_w = mom$eeta[[1]],
    mean_w = imp$mean[[1]],
    theta_w = imp$cov[[1]] - Lw %*% Vw %*% t(Lw),
    moments = function(a, const = 0) {
      c(mean = sum(a * m) + const, var = as.numeric(t(a) %*% V %*% a))
    },
    K = K
  )
}

test_that("Two-level predict() draws match the exact conditional distribution", {
  # Missing within values and a between-only indicator in cluster 1
  dat <- subset(lavaan::Demo.twolevel, cluster <= 10)
  dat$y1[c(2, 5)] <- NA
  dat$y3[3] <- NA
  fit <- asem(
    "level: 1\n fw =~ y1 + y2 + y3\n level: 2\n fb =~ y1 + y2 + y3 + w2",
    dat,
    cluster = "cluster",
    missing = "ML",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  )
  x <- lavaan::lav_model_get_parameters(fit@Model)
  ex <- dense_cluster_1(fit, x)
  ns <- 2000
  check <- function(draws, target) {
    expect_lt(
      abs(mean(draws) - target[["mean"]]),
      4 * sqrt(target[["var"]] / ns)
    )
    expect_equal(var(draws), target[["var"]], tolerance = 0.15)
  }
  ov <- get_inlavaan_internal(fit)$lavdata@ov.names[[1]]

  # Within factor of the first row, and the between factor of the cluster
  a <- numeric(ex$K)
  a[1] <- 1
  lv1 <- predict_pinned(fit, "lv", ns)
  check(sapply(lv1, function(z) z[1, 1]), ex$moments(a))
  a <- numeric(ex$K)
  a[ex$ui[1] - 1] <- 1
  lv2 <- predict_pinned_level2(fit, ns)
  check(sapply(lv2, function(z) z[1, 1]), ex$moments(a))

  # A missing value: y1 of the second row is its within part plus u
  a <- numeric(ex$K)
  a[ex$kw + ex$mw + 1] <- 1
  a[ex$ui] <- ex$A[1, ]
  ym <- predict_pinned(fit, "ymis", ns)
  check(sapply(ym, function(z) z[ex$rows[2], "y1"]), ex$moments(a))

  # ypred of y2 in the first row: its within prediction, a new within
  # residual, and the cluster's own between value
  k <- which(ov[ex$idx1] == "y2")
  a <- numeric(ex$K)
  a[seq_len(ex$mw)] <- ex$Lw[k, ]
  a[ex$ui] <- ex$A[k, ]
  target <- ex$moments(a, ex$mean_w[k] - sum(ex$Lw[k, ] * ex$eeta_w))
  target[["var"]] <- target[["var"]] + ex$theta_w[k, k]
  yp <- predict_pinned(fit, "ypred", ns)
  check(sapply(yp, function(z) z[ex$rows[1], "y2"]), target)
})

test_that("Two-level ypred keeps covariates and draws outcome residuals", {
  dat <- subset(lavaan::Demo.twolevel, cluster <= 30)
  fit <- asem(
    "
      level: 1
        fw =~ y1 + y2 + y3
        y4 ~ fw + x1
      level: 2
        fb =~ y1 + y2 + y3
        y4 ~ fb
        w1 ~ fb + w2
    ",
    dat,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  )
  yhat <- predict_pinned(fit, "yhat", 200)
  ypred <- predict_pinned(fit, "ypred", 200)
  for (v in c("x1", "w2")) {
    for (z in ypred[1:3]) {
      expect_equal(z[, v], dat[[v]], tolerance = 1e-10)
    }
  }
  # A cluster-level outcome takes one value per cluster
  first <- match(dat$cluster, dat$cluster)
  expect_equal(ypred[[1]][, "w1"], ypred[[1]][first, "w1"])
  # The within residual of the observed outcome y4 widens ypred over yhat
  spread <- function(draws, v) {
    mean(apply(sapply(draws, function(z) z[, v]), 1, var))
  }
  theta_w <- lavaan::lavInspect(fit, "theta")$within["y4", "y4"]
  expect_gt(spread(ypred, "y4") - spread(yhat, "y4"), 0.8 * theta_w)
})

test_that("The two-level PPP holds for a covariate with little between variance", {
  # x1 in Demo.twolevel varies almost only within clusters. As a covariate at
  # both levels it took the old Wishart-based PPP to 0 for a model that fits.
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
  set.seed(4)
  fit <- asem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ x1
    ",
    dat,
    cluster = "cluster",
    nsamp = 200,
    test = "ppp",
    verbose = FALSE
  )
  expect_gt(get_inlavaan_internal(fit, "ppp"), 0.05)
})

test_that("The two-level PPP replicate keeps the covariates and the design", {
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
  dat$y2[c(3, 40, 77)] <- NA
  fit0 <- suppressWarnings(lavaan::sem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
    ",
    dat,
    cluster = "cluster",
    missing = "ml"
  ))
  lavdata <- fit0@Data
  set.seed(1)
  rep <- ppp2l_draw(lavdata, lavaan::lav_model_implied(fit0@Model))[[1]]
  X <- lavdata@X[[1]]
  ov <- lavdata@ov.names[[1]]
  # Fixed covariates at their observed values, missing cells where they were
  expect_equal(rep[, ov == "x1"], X[, ov == "x1"])
  expect_equal(rep[, ov == "w1"], X[, ov == "w1"])
  expect_identical(is.na(rep), is.na(X))
  # A between-only covariate is constant within each cluster
  cl <- lavdata@Lp[[1]]$cluster.idx[[2]]
  expect_true(all(tapply(rep[, ov == "w1"], cl, stats::sd) == 0))
  expect_false(isTRUE(all.equal(rep[, ov == "y1"], X[, ov == "y1"])))
})

test_that("The two-level PPP warns about draws that give no replicate", {
  int <- get_inlavaan_internal(fit_ml)
  x <- lavaan::lav_model_get_parameters(int$lavmodel)
  bad <- matrix(NA_real_, 3, length(x))
  expect_warning(
    ppp <- get_ppp_twolevel(bad, int$lavmodel, int$lavsamplestats, int$lavdata),
    "No posterior draw could be scored"
  )
  expect_identical(ppp, NA_real_)
  set.seed(2)
  some <- rbind(bad[1, ], x, x)
  expect_warning(
    ppp <- get_ppp_twolevel(
      some,
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata
    ),
    "1 of 3 posterior draws could not be scored"
  )
  expect_true(ppp %in% c(0, 0.5, 1))
})

## ----- One-step two-level PPP ------------------------------------------------

test_that("The one-step information equals lavaan's expected information", {
  info_lav <- getFromNamespace("lav_mvn_cl_info_expected", "lavaan")
  kdh_lav <- getFromNamespace("lav_mvn_kron_dup_half", "lavaan")
  A <- crossprod(matrix(stats::rnorm(36), 6))
  expect_equal(ppp2l_kron_dup_half(A), kdh_lav(A), tolerance = 1e-12)
  int <- get_inlavaan_internal(fit_ml)
  lp <- int$lavdata@Lp[[1]]
  x_idx <- int$lavsamplestats@x.idx[[1]]
  imp <- ppp2l_group_moments(lavaan::lav_model_implied(fit_ml@Model), 1L)
  I0 <- info_lav(
    lp = lp,
    mu_w = imp$mean[[1]],
    sigma_w = imp$cov[[1]],
    mu_b = imp$mean[[2]],
    sigma_b = imp$cov[[2]],
    x_idx = x_idx
  )
  keep <- which(diag(I0) != 0)
  info <- ppp2l_info(imp, lp, x_idx)
  expect_identical(info$keep, keep)
  expect_equal(
    info$inv,
    solve(lp$nclusters[[2]] * I0[keep, keep]),
    tolerance = 1e-8
  )
})

test_that("One Fisher-scoring step reproduces the EM likelihood ratio", {
  int <- get_inlavaan_internal(fit_ml)
  lavdata <- int$lavdata
  lp <- lavdata@Lp[[1]]
  imp_all <- lavaan::lav_model_implied(fit_ml@Model)
  imp <- ppp2l_group_moments(imp_all, 1L)
  info <- ppp2l_info(imp, lp, int$lavsamplestats@x.idx[[1]])
  set.seed(8)
  out <- t(vapply(
    1:20,
    function(r) {
      X <- ppp2l_draw(lavdata, imp_all)[[1]]
      ylp <- ppp2l_cluster_stats(X, lp)
      ll <- ppp2l_loglik(X, 1L, lavdata, imp_all, FALSE, ppp2l_em, ylp = ylp)
      c(-2 * (ll[["fit"]] - ll[["sat"]]), ppp2l_onestep(ylp, imp, lp, info))
    },
    numeric(2)
  ))
  expect_gt(stats::cor(out[, 1], out[, 2]), 0.99)
  expect_lt(max(abs(out[, 1] - out[, 2]) / out[, 1]), 0.1)
})

test_that("The one-step and EM PPPs agree, and ppp_nsamp sets the draws", {
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 40, ]
  fit_with <- function(...) {
    asem(
      "
      level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1
      level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1
      ",
      dat,
      cluster = "cluster",
      verbose = FALSE,
      marginal_correction = "none",
      vb_correction = FALSE,
      ...
    )
  }
  set.seed(5)
  fit_one <- fit_with(test = "ppp", nsamp = 200, ppp_nsamp = 200)
  set.seed(5)
  fit_em <- fit_with(
    test = "ppp",
    nsamp = 200,
    ppp_nsamp = 200,
    ppp_method = "em"
  )
  p_one <- get_inlavaan_internal(fit_one, "ppp")
  p_em <- get_inlavaan_internal(fit_em, "ppp")
  expect_lt(abs(p_one - p_em), 0.06)
  # Seven draws give a PPP in sevenths
  p7 <- get_inlavaan_internal(
    fit_with(test = "ppp", nsamp = 50, ppp_nsamp = 7),
    "ppp"
  )
  expect_equal(p7 * 7, round(p7 * 7))
  # The two-level PPP is in the default test
  rec <- get_inlavaan_internal(fit_with(nsamp = 20, ppp_nsamp = 20), "test")
  expect_true("ppp" %in% rec$computed)
  expect_error(fit_with(ppp_nsamp = 0), "ppp_nsamp")
  # Inf takes every draw
  p_inf <- get_inlavaan_internal(
    fit_with(test = "ppp", nsamp = 9, ppp_nsamp = Inf),
    "ppp"
  )
  expect_equal(p_inf * 9, round(p_inf * 9))
})

test_that("Incomplete two-level data use the EM PPP", {
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
  dat$y2[c(3, 40, 77)] <- NA
  fit <- suppressWarnings(asem(
    "
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
    ",
    dat,
    cluster = "cluster",
    missing = "ml",
    test = "ppp",
    verbose = FALSE,
    nsamp = 30,
    ppp_nsamp = 30
  ))
  ppp <- get_inlavaan_internal(fit, "ppp")
  expect_true(ppp >= 0 && ppp <= 1)
})

test_that("The one-step PPP frees a covariate the replicates draw", {
  # x1 at both levels is drawn in the replicates, so its moments must be free
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 60, ]
  fit <- suppressWarnings(asem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ x1
    ",
    dat,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  ))
  int <- get_inlavaan_internal(fit)
  set.seed(7)
  xs <- sample_params_posterior(int, 100, TRUE)$x_samp
  ppp_with <- function(method) {
    set.seed(8)
    get_ppp_twolevel(
      xs,
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata,
      method = method
    )
  }
  expect_lt(abs(ppp_with("onestep") - ppp_with("em")), 0.06)
})

test_that("The one-step PPP falls back to EM at the boundary", {
  # A small between variance puts the saturated maximum on the boundary of
  # positive semi-definite between covariances
  set.seed(1)
  J <- 40
  dat <- do.call(
    rbind,
    lapply(seq_len(J), function(j) {
      yb <- c(1, 0.9, 0.8) *
        stats::rnorm(1, sd = 0.15) +
        stats::rnorm(3, sd = 0.05)
      fw <- stats::rnorm(8)
      y <- matrix(yb, 8, 3, byrow = TRUE) +
        outer(fw, c(1, 0.9, 0.8)) +
        matrix(stats::rnorm(24, sd = 0.7), 8)
      data.frame(cluster = j, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])
    })
  )
  fit <- suppressWarnings(asem(
    "
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
    ",
    dat,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  ))
  int <- get_inlavaan_internal(fit)
  lp <- int$lavdata@Lp[[1]]
  set.seed(2)
  xs <- sample_params_posterior(int, 60, TRUE)$x_samp
  # Some draws step outside the region and are scored by EM instead
  n_out <- sum(vapply(
    seq_len(nrow(xs)),
    function(s) {
      imp_all <- lavaan::lav_model_implied(lavaan::lav_model_set_parameters(
        int$lavmodel,
        xs[s, ]
      ))
      imp <- ppp2l_group_moments(imp_all, 1L)
      X <- ppp2l_draw(int$lavdata, imp_all)[[1]]
      info <- ppp2l_info(imp, lp, integer(0))
      is.na(ppp2l_onestep(ppp2l_cluster_stats(X, lp), imp, lp, info))
    },
    logical(1)
  ))
  expect_gt(n_out, 0)
  ppp_with <- function(method) {
    set.seed(3)
    get_ppp_twolevel(
      xs,
      int$lavmodel,
      int$lavsamplestats,
      int$lavdata,
      method = method
    )
  }
  expect_lt(abs(ppp_with("onestep") - ppp_with("em")), 0.06)
})
