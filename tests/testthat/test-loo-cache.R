# The default LOO pass kept with a fit (R/loo-cache.R): waic() after loo(),
# or loo() after waic(), reuses it, with the same numbers and the same
# conditions as a fresh pass.

HS_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
set.seed(1)
dat <- lavaan::HolzingerSwineford1939[
  sample(nrow(lavaan::HolzingerSwineford1939), 40),
]
cache_fit <- function() {
  acfa(
    HS_model,
    dat,
    meanstructure = TRUE,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "gaussian"
  )
}

# Counts the calls of inlav_loo() made while `expr` runs
count_passes <- function(expr) {
  n <- 0L
  orig <- inlav_loo
  local_mocked_bindings(inlav_loo = function(...) {
    n <<- n + 1L
    orig(...)
  })
  force(expr)
  n
}

test_that("waic() after loo() reuses the pass and gives the same numbers", {
  fit <- cache_fit()
  int <- get_inlavaan_internal(fit)
  fresh_loo <- inlav_loo(int)
  fresh_waic <- suppressWarnings(inlav_waic(int))
  expect_identical(count_passes(res <- loo(fit)), 1L)
  expect_identical(res, fresh_loo)
  n <- count_passes(w <- suppressWarnings(waic(fit)))
  expect_identical(n, 0L)
  expect_identical(w, fresh_waic)
  # A second loo() reuses it too
  expect_identical(count_passes(res2 <- loo(fit)), 0L)
  expect_identical(res2, fresh_loo)
})

test_that("loo() after waic() reuses the pass", {
  fit <- cache_fit()
  int <- get_inlavaan_internal(fit)
  fresh_loo <- inlav_loo(int)
  expect_identical(count_passes(suppressWarnings(waic(fit))), 1L)
  expect_identical(count_passes(res <- loo(fit)), 0L)
  expect_identical(res, fresh_loo)
})

test_that("other arguments, or another posterior summary, run their own pass", {
  fit <- cache_fit()
  invisible(loo(fit))
  expect_identical(count_passes(loo(fit, second_order = FALSE)), 1L)
  expect_identical(count_passes(loo(fit, units = 1:5)), 1L)
  expect_identical(count_passes(suppressWarnings(waic(fit, units = 1:5))), 1L)
  # A copy whose theta_star differs shares the environment but not the pass
  int <- get_inlavaan_internal(fit)
  int$theta_star <- int$theta_star + 1e-3
  expect_identical(count_passes(loo(int)), 1L)
  expect_false(identical(loo(int), loo(fit)))
})

test_that("a fit without the cache computes the pass at every call", {
  fit <- cache_fit()
  int <- get_inlavaan_internal(fit)
  int$loo_cache <- NULL
  expect_identical(count_passes(loo(int)), 1L)
  expect_identical(count_passes(suppressWarnings(waic(int))), 1L)
})

test_that("a reused pass signals what a fresh pass would, for each caller", {
  first_order <- structure(
    class = c("inlavaan_loo_first_order", "warning", "condition"),
    list(message = "Reverting to first-order approximation.", call = NULL)
  )
  other <- simpleWarning("another warning")
  dropped <- structure(
    class = c("inlavaan_loo_dropped_units", "message", "condition"),
    list(message = "Not scoring 1 case.\n", call = NULL)
  )
  conds <- list(first_order, other, dropped)
  # loo() shows all three
  expect_warning(
    expect_warning(
      expect_message(loo_cache_replay(conds, "loo"), "Not scoring 1 case"),
      "another warning"
    ),
    class = "inlavaan_loo_first_order"
  )
  # waic() leaves out the reversion of the log CPO terms
  expect_no_warning(
    suppressMessages(loo_cache_replay(conds[-2], "waic"))
  )
  expect_warning(
    suppressMessages(loo_cache_replay(conds, "waic")),
    "another warning"
  )
  expect_message(loo_cache_replay(conds[3], "waic"), "Not scoring 1 case")
})
