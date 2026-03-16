test_that("norta_adjust_R returns R unchanged when all marginals near-Gaussian", {
  R <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  # all alphas near zero -> early return
  approx_data <- matrix(c(0, 0, 0, 0), nrow = 2,
                        dimnames = list(NULL, c("alpha", "other")))
  result <- INLAvaan:::norta_adjust_R(R, approx_data)
  expect_equal(result, R)
})

test_that("norta_adjust_R handles near-zero off-diagonal correlation", {
  R <- matrix(c(1, 1e-12, 1e-12, 1), 2, 2)
  approx_data <- matrix(c(2, -2, 0, 0), nrow = 2,
                        dimnames = list(NULL, c("alpha", "other")))
  result <- INLAvaan:::norta_adjust_R(R, approx_data)
  expect_equal(dim(result), c(2, 2))
  expect_equal(diag(result), c(1, 1))
})

test_that("norta_adjust_R with use_spline = FALSE produces a valid matrix", {
  R <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  approx_data <- matrix(c(1.5, -1.5, 0, 0), nrow = 2,
                        dimnames = list(NULL, c("alpha", "other")))
  result <- INLAvaan:::norta_adjust_R(R, approx_data, use_spline = FALSE)
  expect_equal(dim(result), c(2, 2))
  expect_equal(diag(result), c(1, 1))
  # symmetric
  expect_equal(result[1, 2], result[2, 1])
})

test_that("norta_adjust_R skips near-Gaussian marginal in one pair", {
  # j=1 has alpha=0 (near-Gaussian), k=2 has large alpha -> pair is skipped
  R <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
  approx_data <- matrix(c(0.005, 2.0, 0, 0), nrow = 2,
                        dimnames = list(NULL, c("alpha", "other")))
  result <- INLAvaan:::norta_adjust_R(R, approx_data)
  expect_equal(dim(result), c(2, 2))
})
