# The semi-analytic unit Hessians of single-level LOO (R/loo-hessians.R)
# against the differences of the unit scores (the route of
# loo_batched_hessians(), option inlavaan.loo_hessian = "differences"), on
# every shape of single-level row: with and without a mean structure, missing
# data (patterns on both routes of the trace term), several groups with
# equality constraints, fixed covariates and composites.

hs <- lavaan::HolzingerSwineford1939
hs_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
quick <- function(fn, ...) {
  suppressWarnings(fn(..., silent = TRUE, nsamp = 3, test = "none"))
}
both_routes <- function(fit, ...) {
  int <- get_inlavaan_internal(fit)
  run <- function(route) {
    withr::with_options(
      list(inlavaan.loo_hessian = route),
      suppressWarnings(inlav_loo(int, ...))
    )
  }
  list(semi = run("auto"), diff = run("differences"))
}
expect_same_terms <- function(r, tol = 1e-7) {
  cols <- c(
    "log_cpo_2",
    "lpd_2",
    "det_term",
    "k_max",
    "k_min",
    "k_sum",
    "k_ssq"
  )
  for (col in cols) {
    expect_equal(
      r$semi$per_unit[[col]],
      r$diff$per_unit[[col]],
      tolerance = tol
    )
  }
  expect_identical(r$semi$per_unit$l_star, r$diff$per_unit$l_star)
  expect_equal(r$semi$elpd_2, r$diff$elpd_2, tolerance = tol)
  expect_equal(r$semi$p_loo_2, r$diff$p_loo_2, tolerance = tol)
}

test_that("vech_to_sym() rebuilds the symmetric matrix of a vech", {
  S <- crossprod(matrix(rnorm(16), 4))
  expect_identical(vech_to_sym(S[lower.tri(S, diag = TRUE)], 4), S)
})

test_that("complete data, with and without a mean structure", {
  expect_same_terms(both_routes(quick(
    acfa,
    hs_model,
    hs,
    meanstructure = TRUE
  )))
  expect_same_terms(both_routes(quick(acfa, hs_model, hs)))
})

test_that("missing data, on both routes of the trace term", {
  set.seed(42)
  dat <- hs
  for (v in paste0("x", 1:9)) {
    dat[sample(nrow(dat), 25), v] <- NA
  }
  # Rows that miss 6 of 9 variables take the direct route
  dat[1:6, paste0("x", c(1, 2, 4, 5, 7, 8))] <- NA
  fit <- quick(acfa, hs_model, dat, missing = "ml")
  pats <- fiml_patterns(as.matrix(dat[paste0("x", 1:9)]))
  k <- vapply(pats, function(pt) 9L - length(pt$o), integer(1))
  expect_true(any(k > 0L & 2L * k <= 9L))
  expect_true(any(2L * k > 9L))
  expect_same_terms(both_routes(fit))
})

test_that("several groups with equality constraints", {
  fit <- quick(
    acfa,
    hs_model,
    hs,
    group = "school",
    group.equal = c("loadings", "intercepts")
  )
  expect_same_terms(both_routes(fit))
  # A subset of the units, in another order
  expect_same_terms(both_routes(fit, units = c(300, 2, 150, 7)))
})

test_that("fixed covariates, conditional and joint", {
  model <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60 + x1
  "
  pd <- lavaan::PoliticalDemocracy
  expect_same_terms(both_routes(quick(asem, model, pd, fixed.x = TRUE)))
  expect_same_terms(both_routes(quick(asem, model, pd, fixed.x = FALSE)))
})

test_that("composites", {
  model <- "
    C <~ x1 + x2 + x3
    x4 ~ C
    x5 ~ C
    x4 ~~ x5
  "
  fit <- quick(asem, model, hs, composites.cov = "fixed")
  expect_same_terms(both_routes(fit))
})

test_that("the first-order pass and the option keep the differences", {
  fit <- quick(acfa, hs_model, hs, meanstructure = TRUE)
  int <- get_inlavaan_internal(fit)
  n <- 0L
  orig <- loo_batched_hessians
  local_mocked_bindings(loo_batched_hessians = function(...) {
    n <<- n + 1L
    orig(...)
  })
  invisible(suppressWarnings(inlav_loo(int)))
  expect_identical(n, 0L)
  withr::with_options(
    list(inlavaan.loo_hessian = "differences"),
    invisible(suppressWarnings(inlav_loo(int)))
  )
  expect_identical(n, 1L)
  expect_error(
    withr::with_options(
      list(inlavaan.loo_hessian = "other"),
      loo_hessian_route()
    )
  )
})

## ----- Clusters of two-level models -------------------------------------------

d2 <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
twolevel_model <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"

test_that("clusters, with between-only covariates, joint and conditional", {
  for (fx in c(FALSE, TRUE)) {
    fit <- quick(
      asem,
      twolevel_model,
      d2,
      cluster = "cluster",
      fixed.x = fx
    )
    r <- both_routes(fit)
    expect_identical(r$semi$type, "loco")
    expect_same_terms(r)
  }
})

test_that("clusters when the levels order the variables differently", {
  model <- "
    level: 1
      y4 ~ y1 + y2 + y3
    level: 2
      y1 ~~ y2 + y3 + y4
      y2 ~~ y3 + y4
      y3 ~~ y4
  "
  fit <- quick(asem, model, d2, cluster = "cluster", fixed.x = FALSE)
  expect_same_terms(both_routes(fit, units = c(3, 1, 20)))
})

test_that("clusters of a two-level model with two groups", {
  d2g <- d2
  d2g$g <- ifelse(d2g$cluster %% 2 == 0, "a", "b")
  model <- "
  group: a
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
  group: b
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
  "
  fit <- quick(acfa, model, d2g, cluster = "cluster", group = "g")
  r <- both_routes(fit)
  expect_identical(length(unique(r$semi$per_unit$group)), 2L)
  expect_same_terms(r)
})
