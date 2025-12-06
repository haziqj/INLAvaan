test_that("is_same_function compares function bodies correctly", {
  f1 <- function(x) {
    x + 1
  }
  f2 <- function(x) {
    x + 1
  }
  f3 <- function(x) {
    x + 2
  }

  # Identical bodies should return TRUE
  expect_true(is_same_function(f1, f2))

  # Different bodies should return FALSE
  expect_false(is_same_function(f1, f3))

  # Edge case: It checks body only, ignores argument names
  # (Documenting this behavior is important)
  g1 <- function(a) {
    return(1)
  }
  g2 <- function(b) {
    return(1)
  }
  expect_true(is_same_function(g1, g2))
})

test_that("as_fun_string flattens functions to single string", {
  f <- function(x) {
    x^2 + 1
  }

  res <- as_fun_string(f)

  expect_type(res, "character")
  expect_length(res, 1)
  # Check that newlines/spaces were normalized
  expect_match(res, "function \\(x\\)")
  expect_match(res, "x\\^2 \\+ 1")
})

test_that("check_mat identifies problematic matrices", {
  # 1. Positive Definite (Good) -> Should return FALSE
  mat_pd <- matrix(c(2, 0.5, 0.5, 2), 2, 2)
  expect_false(check_mat(mat_pd))

  # 2. Negative Definite (Bad) -> Should return TRUE
  mat_nd <- matrix(c(-2, 0.5, 0.5, -2), 2, 2)
  expect_true(check_mat(mat_nd))

  # 3. NA/Inf/NaN (Bad) -> Should return TRUE
  mat_na <- mat_pd
  mat_na[1, 1] <- NA
  expect_true(check_mat(mat_na))

  mat_inf <- mat_pd
  mat_inf[1, 1] <- Inf
  expect_true(check_mat(mat_inf))
})

test_that("force_pd corrects non-PD matrices", {
  # Case 1: Already PD -> Should remain unchanged
  mat_pd <- matrix(c(2, 1, 1, 2), 2, 2)
  expect_equal(force_pd(mat_pd), mat_pd)

  # Case 2: Non-PD -> Should become PD
  # Create a matrix with negative eigenvalues
  mat_bad <- matrix(c(1, 2, 2, 1), 2, 2) # Eigenvalues: 3, -1

  mat_fixed <- force_pd(mat_bad)

  # Check that it is now PD (all eigenvalues > 0)
  eigs <- eigen(mat_fixed)$values
  expect_true(all(eigs + 1e-9 > 0))

  # Check that it is still symmetric
  expect_true(isSymmetric(mat_fixed))
})
