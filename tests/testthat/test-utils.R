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

test_that("is_bad_cov identifies problematic matrices", {
  # 1. Positive Definite (Good) -> Should return FALSE
  mat_pd <- matrix(c(2, 0.5, 0.5, 2), 2, 2)
  expect_false(is_bad_cov(mat_pd))

  # 2. Negative Definite (Bad) -> Should return TRUE
  mat_nd <- matrix(c(-2, 0.5, 0.5, -2), 2, 2)
  expect_true(is_bad_cov(mat_nd))

  # 3. NA/Inf/NaN (Bad) -> Should return TRUE
  mat_na <- mat_pd
  mat_na[1, 1] <- NA
  expect_true(is_bad_cov(mat_na))

  mat_inf <- mat_pd
  mat_inf[1, 1] <- Inf
  expect_true(is_bad_cov(mat_inf))
})

test_that("make_pd corrects non-PD matrices", {
  # Case 1: Already PD -> Should remain unchanged
  mat_pd <- matrix(c(2, 1, 1, 2), 2, 2)
  expect_equal(make_pd(mat_pd), mat_pd)

  # Case 2: Non-PD -> Should become PD
  # Create a matrix with negative eigenvalues
  mat_bad <- matrix(c(1, 2, 2, 1), 2, 2) # Eigenvalues: 3, -1

  mat_fixed <- make_pd(mat_bad)

  # Check that it is now PD (all eigenvalues > 0)
  eigs <- eigen(mat_fixed)$values
  expect_true(all(eigs + 1e-9 > 0))

  # Check that it is still symmetric
  expect_true(isSymmetric(mat_fixed))
})

test_that("is_lavaan / is_blavaan identify objects correctly", {
  mod <- "visual =~ x1 + x2 + x3"
  dat <- lavaan::HolzingerSwineford1939
  lav_fit <- lavaan::cfa(mod, dat)

  expect_true(is_lavaan(lav_fit))
  expect_false(isTRUE(is_lavaan(list())))
  expect_false(is_blavaan(lav_fit))
})

test_that("dmode handles edge cases", {
  expect_true(is.na(dmode(numeric(0))))
  expect_equal(dmode(42), 42)
  # Constant vector: returns mean
  expect_equal(dmode(rep(5, 10)), 5)
})

test_that("get_inlavaan_internal errors for non-INLAvaan", {
  expect_error(get_inlavaan_internal(list()), "Object must be of class")
})

test_that("lavaan internals resolve to functions", {
  for (name in lavaan_internal_names) {
    expect_true(
      is.function(get(paste0("lavaan___", name))),
      info = name
    )
  }
  # The random-slope kernels are bound only from lavaan >= 0.7-2
  expect_true(is.function(lavaan___lav_mvn_cl_rs_m2ll))
  expect_true(is.function(lavaan___lav_mvn_cl_rs_scores))
  expect_true(is.function(lavaan___lav_mvn_cl_rs_eb))
})

# Set LC_COLLATE, returning FALSE if the locale is not available
set_collate <- function(locale) {
  suppressWarnings(Sys.setlocale("LC_COLLATE", locale)) != ""
}

test_that("canonical_order ignores the collation locale", {
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
  skip_if_not(set_collate("en_US.UTF-8"), "en_US.UTF-8 not available")

  x <- c("Visual=~B2", "a1~~a1", "B2~~B2", "textual=~F6", ".p1.")
  expected <- c(5L, 3L, 1L, 2L, 4L)
  expect_identical(canonical_order(x), expected)
  set_collate("C")
  expect_identical(canonical_order(x), expected)
})

test_that("fit results do not depend on the collation locale", {
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
  skip_if_not(set_collate("en_US.UTF-8"), "en_US.UTF-8 not available")

  dat <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  names(dat) <- c("a1", "B2", "c3", "D4", "e5", "F6", "g7", "H8", "i9")
  mod <- "
    Visual =~ a1 + B2 + c3
    textual =~ D4 + e5 + F6
    Speed =~ g7 + H8 + i9
  "
  fit_once <- function() {
    set.seed(1)
    fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
    c(get_inlavaan_internal(fit)$mloglik, coef(fit))
  }
  res_utf8 <- fit_once()
  set_collate("C")
  res_c <- fit_once()
  expect_equal(res_utf8, res_c, tolerance = 1e-10)
})
