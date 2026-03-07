test_that("sobol_owen returns correct dimensions from stored table", {
  res <- sobol_owen(10, 5)
  expect_true(is.matrix(res))
  expect_equal(dim(res), c(10, 5))
})

test_that("sobol_owen returns values in [0, 1]", {
  res <- sobol_owen(50, 10)
  expect_true(all(res >= 0 & res <= 1))
})

test_that("sobol_owen with d = 1 returns a matrix (not a vector)", {
  res <- sobol_owen(5, 1)
  expect_true(is.matrix(res))
  expect_equal(ncol(res), 1)
})

test_that("sobol_owen boundary: full stored table dimensions work", {
  res <- sobol_owen(100, 300)
  expect_equal(dim(res), c(100, 300))
})

# test_that("sobol_owen errors when exceeding stored table and qrng unavailable", {
#   skip_if(requireNamespace("qrng", quietly = TRUE),
#           "qrng is installed; cannot test the error path")
#   expect_error(sobol_owen(200, 5))
# })
#
# test_that("sobol_owen falls back to qrng for large requests", {
#   skip_if_not_installed("qrng")
#   res <- sobol_owen(200, 5)
#   expect_true(is.matrix(res))
#   expect_equal(dim(res), c(200, 5))
# })
