## ----- Shared fixture ---------------------------------------------------------
d_rs <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
mod_rs <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~ w1
"
fit_rs <- asem(
  mod_rs,
  d_rs,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)

## ----- Generator ------------------------------------------------------------------

test_that("The generator draws each cluster from its own moments", {
  skip_on_cran()
  spec <- rs_spec(get_inlavaan_internal(fit_rs))
  info <- spec$rs$info
  imp <- rs_implied_pieces(fit_rs@Model, info)
  cl <- rs_cluster_data(fit_rs@Data, spec$rs)[[3L]]
  mom <- rs_cluster_moments(imp, info, cl$X, cl$exo_b)
  rows <- which(fit_rs@Data@Lp[[1]]$cluster.idx[[2]] == 3L)
  n_rep <- 4000
  set.seed(11)
  sim <- t(vapply(
    seq_len(n_rep),
    function(r) {
      X <- rs_draw_outcomes(fit_rs@Model, spec$rs, fit_rs@Data)
      as.numeric(t(X[rows, info$y.data.idx]))
    },
    numeric(length(mom$mean))
  ))
  z_mean <- (colMeans(sim) - mom$mean) / sqrt(diag(mom$cov) / n_rep)
  se_cov <- sqrt((mom$cov^2 + tcrossprod(diag(mom$cov))) / n_rep)
  z_cov <- ((stats::cov(sim) - mom$cov) / se_cov)[upper.tri(mom$cov, TRUE)]
  expect_lt(max(abs(z_mean)), 4.5)
  expect_lt(max(abs(z_cov)), 4.5)
  expect_lt(mean(abs(z_cov) > 2), 0.1)
})

test_that("simulate() keeps the covariates and the clusters", {
  sims <- simulate(fit_rs, nsim = 2, seed = 1)
  expect_length(sims, 2L)
  expect_equal(sims[[1]]$x1, d_rs$x1)
  expect_equal(sims[[1]]$w1, d_rs$w1)
  expect_equal(
    as.numeric(table(sims[[1]]$cluster)),
    as.numeric(table(d_rs$cluster))
  )
  expect_false(isTRUE(all.equal(sims[[1]]$y1, d_rs$y1)))
  expect_named(attr(sims[[1]], "truth"), names(attr(sims[[2]], "truth")))
  expect_length(simulate(fit_rs, nsim = 1, seed = 2, prior = TRUE), 1L)
  # The clusters keep the labels of the data
  d <- d_rs
  d$cluster <- d$cluster * 10 + 3
  fit <- asem(
    mod_rs,
    d,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  expect_equal(simulate(fit, nsim = 1, seed = 1)[[1]]$cluster, d$cluster)
  expect_error(
    simulate(fit_rs, sample.nobs = 100),
    class = "inlavaan_rs_simulate"
  )
})

## ----- sampling() -----------------------------------------------------------------

test_that("sampling() gives implied, latent and observed draws", {
  set.seed(3)
  im <- sampling(fit_rs, type = "implied", nsamp = 2)
  expect_named(im[[1]], c("within", "cluster"))
  # The implied draws are the averaged moments at each draw
  set.seed(3)
  x <- sampling(fit_rs, type = "lavaan", nsamp = 2)
  info <- rs_spec(get_inlavaan_internal(fit_rs))$rs$info
  avg <- rs_avg_implied(
    lavaan::lav_model_set_parameters(fit_rs@Model, x[1, ]),
    info
  )
  expect_equal(unname(im[[1]]$within$cov), unname(avg$cov[[1]]))

  la <- sampling(fit_rs, type = "latent", nsamp = 3)
  expect_true(all(c("fw", "fb", "s1") %in% colnames(la)))
  ob <- sampling(fit_rs, type = "observed", nsamp = 3)
  expect_equal(colnames(ob), c("y1", "y2", "y3", "x1", "w1"))
  expect_named(
    sampling(fit_rs, type = "all", nsamp = 2),
    c("lavaan", "theta", "latent", "observed", "implied")
  )
  expect_equal(
    dim(sampling(fit_rs, "observed", nsamp = 2, prior = TRUE)),
    c(2L, 5L)
  )
})

test_that("Observed draws carry the slope variance", {
  skip_on_cran()
  # Every parameter at the posterior mean, so the draws share one model, but
  # with a slope variance large enough for the test to see it
  int <- get_inlavaan_internal(fit_rs)
  x <- lavaan::lav_model_get_parameters(fit_rs@Model)
  pt <- lavaan::parTable(fit_rs)
  k <- pt$free[pt$lhs == "s1" & pt$op == "~~" & pt$rhs == "s1"]
  x[k] <- 1
  total_at <- function(x) {
    avg <- rs_avg_implied(
      lavaan::lav_model_set_parameters(fit_rs@Model, x),
      rs_info_of(int)
    )
    avg$cov[[1]][1:3, 1:3] + avg$cov[[2]][1:3, 1:3]
  }
  total <- total_at(x)
  x0 <- x
  x0[k] <- 0
  expect_gt(max(abs(total - total_at(x0))), 0.5)
  set.seed(5)
  ob <- t(vapply(
    1:6000,
    function(i) {
      sample_generative_ml(
        x,
        int$lavmodel,
        int$lavdata,
        rs_paths = rs_info_of(int)$path.tab
      )$observed
    },
    numeric(5)
  ))
  expect_lt(max(abs(stats::cov(ob)[1:3, 1:3] - total)), 0.15)
})

## ----- Casewise values ------------------------------------------------------------

test_that("Casewise fitted values are the outcomes' means given the covariates", {
  f <- fitted(fit_rs, type = "casewise")
  expect_equal(colnames(f), c("y1", "y2", "y3", "x1"))
  expect_equal(unname(f[, "x1"]), d_rs$x1)
  # The same means from the stacked moments of each cluster
  spec <- rs_spec(get_inlavaan_internal(fit_rs))
  imp <- rs_implied_pieces(fit_rs@Model, spec$rs$info)
  stacked <- unlist(lapply(rs_cluster_data(fit_rs@Data, spec$rs), function(cl) {
    rs_cluster_moments(imp, spec$rs$info, cl$X, cl$exo_b)$mean
  }))
  expect_equal(as.numeric(t(f[, 1:3])), stacked)
  r <- residuals(fit_rs, type = "casewise")
  expect_equal(r[, "y1"], d_rs$y1 - f[, "y1"])
  expect_equal(unname(r[, "x1"]), rep(0, nrow(d_rs)))
  expect_equal(unname(fitted(fit_rs, type = "ov")), unname(f))
  expect_error(
    fitted(fit_rs, type = "casewise", per_cluster = TRUE),
    class = "inlavaan_per_cluster"
  )
  expect_error(
    residuals(fit_rs, type = "casewise", per_cluster = TRUE),
    class = "inlavaan_per_cluster"
  )
})

test_that("predict() gives cluster-specific yhat and ypred", {
  set.seed(6)
  yhat <- predict(fit_rs, type = "yhat", nsamp = 40)
  expect_length(yhat, 40L)
  expect_equal(colnames(yhat[[1]]), c("y1", "y2", "y3", "x1", "w1"))
  expect_equal(unname(yhat[[1]][, "w1"]), d_rs$w1)
  # The cluster's own random effects make yhat much closer to y than the
  # population-average fitted values
  m <- Reduce(`+`, yhat) / length(yhat)
  f <- fitted(fit_rs, type = "casewise")
  expect_gt(cor(m[, "y1"], d_rs$y1), cor(f[, "y1"], d_rs$y1) + 0.3)
  # ypred adds the level-1 residual
  set.seed(6)
  ypred <- predict(fit_rs, type = "ypred", nsamp = 40)
  spread <- function(draws, v) {
    mean(apply(sapply(draws, function(z) z[, v]), 1, stats::var))
  }
  theta_w <- lavaan::lavInspect(fit_rs, "theta")$within["y1", "y1"]
  expect_gt(spread(ypred, "y1") - spread(yhat, "y1"), 0.6 * theta_w)
  expect_error(predict(fit_rs, type = "ymis"), class = "inlavaan_rs_predict")
})

test_that("ypred draws the within residual given the latent values", {
  int <- get_inlavaan_internal(fit_rs)
  info <- rs_spec(int)$rs$info
  imp <- rs_implied_pieces(fit_rs@Model, info)
  w <- rs_glist_block(fit_rs@Model, fit_rs@Model@GLIST, 1L)$mats
  # With no latent values, the residual is the whole within covariance
  parts <- rs_within_parts(w, info, character(0))
  expect_equal(parts$H, matrix(0, 3, 3))
  # With the factor's values, the residual covariance shrinks by the part
  # that runs through the factor, Lambda psi Lambda'
  parts <- rs_within_parts(w, info, "fw")
  lam <- w$lambda[info$y.names, "fw"]
  expect_equal(
    unname(parts$H),
    unname(outer(lam, lam) * w$psi["fw", "fw"])
  )
  Y <- fit_rs@Data@X[[1]][, info$y.data.idx]
  Y[1:150, 2] <- NA
  set.seed(3)
  E <- do.call(
    rbind,
    lapply(1:20, function(k) {
      rs_draw_within(Y, imp$sigma_w, parts$H)
    })
  )
  rows <- rep(seq_len(nrow(Y)), 20) > 150
  R <- imp$sigma_w - parts$H %*% solve(imp$sigma_w, parts$H)
  expect_equal(unname(cov(E[rows, ])), unname(R), tolerance = 0.1)
  o <- c(1L, 3L)
  R_o <- imp$sigma_w -
    parts$H[, o] %*% solve(imp$sigma_w[o, o], parts$H[o, ])
  expect_equal(unname(cov(E[!rows, ])), unname(R_o), tolerance = 0.1)
})

test_that("ypred adds the residual of an observed outcome", {
  skip_on_cran()
  # lavaan keeps an observed outcome's residual in Psi, with zero Theta
  fit <- asem(
    "level: 1
       y1 ~ rv('s1')*x1
     level: 2
       y1 ~ w1
       s1 ~ w1",
    d_rs,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  )
  int <- get_inlavaan_internal(fit)
  x <- lavaan::lav_model_get_parameters(fit@Model)
  xs <- matrix(x, 300, length(x), byrow = TRUE)
  set.seed(4)
  yhat <- rs_predict_y(int, int$lavmodel, int$lavdata, xs, "yhat")
  ypred <- rs_predict_y(int, int$lavmodel, int$lavdata, xs, "ypred")
  spread <- function(draws) {
    mean(apply(sapply(draws, function(z) z[, "y1"]), 1, stats::var))
  }
  psi <- lavaan::lavInspect(fit, "est")$within$psi["y1", "y1"]
  expect_equal(spread(ypred) - spread(yhat), psi, tolerance = 0.1)
})

## ----- B-indices ------------------------------------------------------------------

test_that("The B-index reference starts at the fitted model", {
  # The fixture's posterior means, whose variances are all positive
  fit <- fit_rs
  info <- rs_spec(get_inlavaan_internal(fit))$rs$info
  syn <- rs_baseline_syntax(fit@Model, info)
  b0 <- suppressWarnings(lavaan::sem(
    syn,
    d_rs,
    cluster = "cluster",
    do.fit = FALSE
  ))
  pt <- rs_baseline_start(fit@Model, info, lavaan::parTable(b0))
  # Keep the start exactly at the nested point for this check
  free <- pt$free > 0L
  x <- pt$start[free][order(pt$free[free])]
  ll_ref <- lavaan___lav_mvn_cl_rs_m2ll(
    lavmodel = lavaan::lav_model_set_parameters(b0@Model, x),
    rs = b0@Cache[[1L]]$rs,
    log2pi = TRUE,
    minus_two = FALSE
  )
  ll_model <- lavaan___lav_mvn_cl_rs_m2ll(
    lavmodel = fit@Model,
    rs = rs_spec(get_inlavaan_internal(fit))$rs,
    log2pi = TRUE,
    minus_two = FALSE
  )
  expect_equal(as.numeric(ll_ref), as.numeric(ll_model), tolerance = 1e-6)
})

test_that("The B-indices of a random-slope fit", {
  skip_on_cran()
  fit <- asem(
    mod_rs,
    d_rs,
    cluster = "cluster",
    verbose = FALSE,
    test = "dic",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  ref <- rs_baseline_fit(fit)
  expect_gt(ref$npar, fit@Fit@npar)
  set.seed(9)
  bf <- bfit_indices(fit, nsamp = 40)
  expect_true(all(
    c("BRMSEA", "BGammaHat", "BMc", "BCFI") %in% names(bf$indices)
  ))
  expect_equal(bf$details$df, ref$npar - bf$details$pD)
  expect_true(all(bf$indices$BRMSEA >= 0))
  expect_true(all(bf$indices$BMc <= 1))
  # The reference fits at least as well as every draw of the model
  expect_true(all(bf$details$chisq > 0))
})

test_that("The B-indices refuse a between-only outcome with between covariates", {
  fit <- asem(
    "level: 1
       fw =~ y1 + y2 + y3
       fw ~ rv('s1')*x1
     level: 2
       fb =~ y1 + y2 + y3 + w2
       fb ~ w1",
    d_rs,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  expect_error(rs_baseline_fit(fit), class = "inlavaan_rs_bfit")
})
