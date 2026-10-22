test_that("vb_nodes centres the QMC node set exactly", {
  set.seed(123)
  m <- 12
  A <- matrix(rnorm(m * m), m)
  L <- t(chol(crossprod(A) / m + diag(m)))

  zs <- vb_nodes(40, L)

  expect_equal(nrow(zs), 41)
  expect_equal(ncol(zs), m)
  expect_equal(zs[1, ], rep(0, m))
  expect_equal(colMeans(zs), rep(0, m))
})

test_that("vb_nodes reproduces the target covariance up to node error", {
  set.seed(123)
  m <- 5
  Sigma <- crossprod(matrix(rnorm(m * m), m)) / m + diag(m)
  L <- t(chol(Sigma))

  zs <- vb_nodes(100, L)

  expect_equal(crossprod(zs) / nrow(zs), Sigma, tolerance = 0.2)
})

test_that("sobol_owen beyond the stored table ignores the RNG state", {
  skip_if_not_installed("qrng")
  d <- ncol(SobolOwen) + 10L
  set.seed(1)
  a <- sobol_owen(n = 8, d = d)
  set.seed(99)
  b <- sobol_owen(n = 8, d = d)
  expect_identical(a, b)
  # and leaves the RNG stream where it was
  set.seed(1)
  invisible(sobol_owen(n = 8, d = d))
  r1 <- runif(1)
  set.seed(1)
  expect_identical(runif(1), r1)
})
