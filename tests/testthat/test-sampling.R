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
