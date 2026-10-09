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
