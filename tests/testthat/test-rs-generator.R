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
  # Every parameter at the posterior mean, so the draws share one model
  int <- get_inlavaan_internal(fit_rs)
  x <- lavaan::lav_model_get_parameters(fit_rs@Model)
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
  avg <- rs_avg_implied(fit_rs@Model, rs_info_of(int))
  total <- avg$cov[[1]][1:3, 1:3] + avg$cov[[2]][1:3, 1:3]
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
