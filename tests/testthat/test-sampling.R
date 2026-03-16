dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("sampling() returns matrix for type = 'lavaan'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "lavaan", nsamp = 10)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
  expect_true(all(!is.na(colnames(s))))
})

test_that("sampling() returns matrix for type = 'theta'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "theta", nsamp = 10)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
  expect_equal(ncol(s), ncol(sampling(fit, type = "lavaan", nsamp = 10)))
})

test_that("sampling() with samp_copula = FALSE returns matrix", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "lavaan", nsamp = 10, samp_copula = FALSE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
})

test_that("sampling() type = 'latent' returns nsamp x nlv matrix", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "latent", nsamp = 8)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 2)  # visual, textual
  expect_equal(colnames(s), c("visual", "textual"))
})

test_that("sampling() type = 'observed' returns nsamp x nobs_vars matrix", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "observed", nsamp = 8)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 6)  # x1..x6
  expect_equal(colnames(s), paste0("x", 1:6))
})

test_that("sampling() type = 'all' returns named list of matrices", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
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
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "lavaan", nsamp = 10, prior = TRUE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 10)
  # Columns named the same as posterior samples
  sp <- sampling(fit, type = "lavaan", nsamp = 10)
  expect_equal(colnames(s), colnames(sp))
})

test_that("sampling() prior = TRUE with type = 'latent' works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "latent", nsamp = 8, prior = TRUE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 2)
})

test_that("sampling() prior = TRUE with type = 'observed' works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "observed", nsamp = 8, prior = TRUE)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 8)
  expect_equal(ncol(s), 6)
})

test_that("sampling() prior = TRUE with type = 'all' works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
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
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "implied", nsamp = 8)
  expect_true(is.list(s))
  expect_length(s, 8)
  # Each element has a cov matrix
  expect_true(is.matrix(s[[1]]$cov))
  expect_equal(nrow(s[[1]]$cov), 6)  # x1..x6
  expect_equal(ncol(s[[1]]$cov), 6)
  expect_true(isSymmetric(s[[1]]$cov))
  # No mean vector (meanstructure = FALSE by default)
  expect_null(s[[1]]$mean)
})

test_that("sampling() type = 'implied' prior = TRUE works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s <- sampling(fit, type = "implied", nsamp = 8, prior = TRUE)
  expect_true(is.list(s))
  expect_length(s, 8)
  expect_true(is.matrix(s[[1]]$cov))
  expect_equal(nrow(s[[1]]$cov), 6)
})

test_that("sampling.inlavaan_internal S3 dispatch works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  int <- INLAvaan:::get_inlavaan_internal(fit)
  s <- INLAvaan:::sampling.inlavaan_internal(int, type = "lavaan", nsamp = 5)
  expect_true(is.matrix(s))
  expect_equal(nrow(s), 5)
})
