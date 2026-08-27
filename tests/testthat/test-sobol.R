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
