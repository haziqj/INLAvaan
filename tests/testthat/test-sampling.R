dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

# Fit once, reuse (fast defaults)
fit <- acfa(
  mod,
  dat,
  verbose = FALSE,
  nsamp = 5,
  vb_correction = FALSE,
  test = "none",
  marginal_method = "marggaus"
)

test_that("sampling() returns matrix for type = 'lavaan'", {
  s <- sampling(fit, type = "lavaan", nsamp = 10)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
  expect_true(all(!is.na(colnames(s))))
})

test_that("sampling() returns matrix for type = 'theta'", {
  s <- sampling(fit, type = "theta", nsamp = 10)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
  expect_equal(ncol(s), ncol(sampling(fit, type = "lavaan", nsamp = 10)))
})

test_that("sampling() with samp_copula = FALSE returns matrix", {
  s <- sampling(fit, type = "lavaan", nsamp = 10, samp_copula = FALSE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
})

test_that("sampling() type = 'latent' returns nsamp x nlv matrix", {
  s <- sampling(fit, type = "latent", nsamp = 8)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 2) # visual, textual
  expect_equal(colnames(s), c("visual", "textual"))
})

test_that("sampling() type = 'observed' returns nsamp x nobs_vars matrix", {
  s <- sampling(fit, type = "observed", nsamp = 8)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 6) # x1..x6
  expect_equal(colnames(s), paste0("x", 1:6))
})

test_that("sampling() type = 'all' returns named list of matrices", {
  s <- sampling(fit, type = "all", nsamp = 8)
  expect_true(is.list(s))
  expect_named(s, c("lavaan", "theta", "latent", "observed", "implied"))
  expect_equal(nrow(s$lavaan), 8)
  expect_equal(nrow(s$theta), 8)
  expect_equal(nrow(s$latent), 8)
  expect_equal(nrow(s$observed), 8)
  expect_equal(ncol(s$latent), 2)
  expect_equal(ncol(s$observed), 6)
  expect_length(s$implied, 8)
})

test_that("sampling() with prior = TRUE draws from priors", {
  s <- sampling(fit, type = "lavaan", nsamp = 10, prior = TRUE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
  sp <- sampling(fit, type = "lavaan", nsamp = 10)
  expect_equal(colnames(s), colnames(sp))
})

test_that("sampling() prior = TRUE with type = 'latent' works", {
  s <- sampling(fit, type = "latent", nsamp = 8, prior = TRUE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 2)
})

test_that("sampling() prior = TRUE with type = 'observed' works", {
  s <- sampling(fit, type = "observed", nsamp = 8, prior = TRUE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 6)
})

test_that("sampling() prior = TRUE with type = 'all' works", {
  s <- sampling(fit, type = "all", nsamp = 8, prior = TRUE)
  expect_true(is.list(s))
  expect_named(s, c("lavaan", "theta", "latent", "observed", "implied"))
  expect_equal(nrow(s$lavaan), 8)
  expect_equal(nrow(s$theta), 8)
  expect_equal(nrow(s$latent), 8)
  expect_equal(nrow(s$observed), 8)
  expect_length(s$implied, 8)
})

test_that("sampling() type = 'implied' returns list of covariance matrices", {
  s <- sampling(fit, type = "implied", nsamp = 8)
  expect_true(is.list(s))
  expect_length(s, 8)
  expect_true(is.matrix(s[[1]]$cov))
  expect_equal(nrow(s[[1]]$cov), 6)
  expect_equal(ncol(s[[1]]$cov), 6)
  expect_true(isSymmetric(s[[1]]$cov))
  expect_null(s[[1]]$mean)
})

test_that("sampling() type = 'implied' prior = TRUE works", {
  s <- sampling(fit, type = "implied", nsamp = 8, prior = TRUE)
  expect_true(is.list(s))
  expect_length(s, 8)
  expect_true(is.matrix(s[[1]]$cov))
  expect_equal(nrow(s[[1]]$cov), 6)
})

test_that("sampling.inlavaan_internal S3 dispatch works", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  s <- INLAvaan:::sampling.inlavaan_internal(int, type = "lavaan", nsamp = 5)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 5)
})

test_that("sampling() works with a single latent variable (nlv = 1)", {
  # Regression test: with one latent variable the generative draws were built
  # with t(vapply(., numeric(1))), yielding a 1 x nsamp row matrix and a
  # dimnames crash. They must come back as nsamp x 1 / nsamp x nobs matrices.
  fit1 <- acfa(
    "visual =~ x1 + x2 + x3",
    dat,
    verbose = FALSE,
    nsamp = 5,
    vb_correction = FALSE,
    test = "none",
    marginal_method = "marggaus"
  )

  lat <- sampling(fit1, type = "latent", nsamp = 8)
  expect_equal(dim(lat), c(8L, 1L))
  expect_equal(colnames(lat), "visual")

  obs <- sampling(fit1, type = "observed", nsamp = 8, silent = TRUE)
  expect_equal(dim(obs), c(8L, 3L))
  expect_equal(colnames(obs), paste0("x", 1:3))

  all_s <- sampling(fit1, type = "all", nsamp = 8, silent = TRUE)
  expect_equal(ncol(all_s$latent), 1L)
  expect_equal(ncol(all_s$observed), 3L)
})

test_that("Observed posterior draws are centred without a mean structure", {
  # Regression test: with meanstructure = FALSE the generative draws were
  # centred at zero; they must live on the data scale (saturated means).
  mod_hs <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  hs <- lavaan::HolzingerSwineford1939
  fit <- acfa(
    mod_hs,
    hs,
    meanstructure = FALSE,
    verbose = FALSE,
    nsamp = 200,
    vb_correction = FALSE,
    test = "none"
  )
  yrep <- sampling(fit, type = "observed", nsamp = 200, silent = TRUE)
  ybar <- colMeans(hs[, colnames(yrep)])
  expect_lt(max(abs(colMeans(yrep) - ybar)), 0.5)
})

## ----- Two-level fits --------------------------------------------------------
mod_ml <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
"
dat_ml <- subset(lavaan::Demo.twolevel, cluster <= 24)
fit_ml <- asem(
  mod_ml,
  dat_ml,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  nsamp = 50,
  vb_correction = FALSE,
  marginal_correction = "none"
)

test_that("sampling() type = 'implied' returns the within/cluster pair", {
  s <- sampling(fit_ml, type = "implied", nsamp = 1)
  expect_length(s, 1)
  expect_named(s[[1]], c("within", "cluster"))

  # At the posterior mode the two blocks must reproduce lavaan's own
  # model-implied moments exactly.
  lavmodel_x <- lavaan::lav_model_set_parameters(fit_ml@Model, coef(fit_ml))
  implied <- lavaan::lav_model_implied(lavmodel_x)
  int <- INLAvaan:::get_inlavaan_internal(fit_ml)
  fixed <- INLAvaan:::compute_implied_moments_ml(
    coef(fit_ml),
    int$lavmodel,
    int$lavdata
  )

  expect_named(fixed, c("within", "cluster"))
  expect_equal(
    fixed$within$cov,
    implied$cov[[1]],
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(
    fixed$cluster$cov,
    implied$cov[[2]],
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(
    fixed$within$mean,
    as.numeric(implied$mean[[1]]),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(
    fixed$cluster$mean,
    as.numeric(implied$mean[[2]]),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(colnames(fixed$within$cov), c("y1", "y2", "y3", "x1"))
  expect_equal(colnames(fixed$cluster$cov), c("y1", "y2", "y3", "w1"))
})

test_that("sampling() type = 'latent' covers both levels", {
  s <- sampling(fit_ml, type = "latent", nsamp = 6)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 6)
  expect_true(all(c("fw", "fb") %in% colnames(s)))
  expect_true(all(is.finite(s)))
})

test_that("sampling() type = 'observed' covers both levels", {
  s <- sampling(fit_ml, type = "observed", nsamp = 6)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 6)
  expect_setequal(colnames(s), c("y1", "y2", "y3", "x1", "w1"))
  expect_true(all(is.finite(s)))
})

test_that("Two-level observed draws follow the two-level moments", {
  # Draws at a fixed parameter vector: the marginal covariance of a variable
  # present at both levels is the sum of the within and cluster blocks, and a
  # between-only variable carries the cluster block's variance alone.
  int <- INLAvaan:::get_inlavaan_internal(fit_ml)
  x <- coef(fit_ml)
  implied <- lavaan::lav_model_implied(
    lavaan::lav_model_set_parameters(fit_ml@Model, x)
  )

  set.seed(20240917)
  draws <- t(vapply(
    seq_len(2000),
    function(i) {
      INLAvaan:::sample_generative_ml(x, int$lavmodel, int$lavdata)$observed
    },
    numeric(5)
  ))

  S <- stats::cov(draws)
  target_y <- diag(implied$cov[[1]])[1:3] + diag(implied$cov[[2]])[1:3]
  expect_lt(max(abs(diag(S)[1:3] / target_y - 1)), 0.25)

  var_w1 <- implied$cov[[2]][4, 4]
  expect_lt(abs(S["w1", "w1"] / var_w1 - 1), 0.25)
})

test_that("sampling() type = 'all' works for two-level fits", {
  s <- sampling(fit_ml, type = "all", nsamp = 4)
  expect_named(s, c("lavaan", "theta", "latent", "observed", "implied"))
  expect_equal(nrow(s$lavaan), 4)
  expect_equal(nrow(s$theta), 4)
  expect_equal(nrow(s$latent), 4)
  expect_equal(nrow(s$observed), 4)
  expect_length(s$implied, 4)
  expect_named(s$implied[[1]], c("within", "cluster"))
})

test_that("sampling() prior = TRUE covers both levels", {
  s <- sampling(fit_ml, type = "latent", nsamp = 4, prior = TRUE, silent = TRUE)
  expect_equal(nrow(s), 4)
  expect_true(all(c("fw", "fb") %in% colnames(s)))

  im <- sampling(fit_ml, type = "implied", nsamp = 2, prior = TRUE)
  expect_length(im, 2)
  expect_named(im[[1]], c("within", "cluster"))
})

test_that("Single-level draws are unchanged by the two-level path", {
  lat <- sampling(fit, type = "latent", nsamp = 5)
  expect_equal(colnames(lat), c("visual", "textual"))
  obs <- sampling(fit, type = "observed", nsamp = 5, silent = TRUE)
  expect_equal(colnames(obs), paste0("x", 1:6))
  im <- sampling(fit, type = "implied", nsamp = 2)
  expect_equal(colnames(im[[1]]$cov), paste0("x", 1:6))
  expect_null(im[[1]]$mean)
})
