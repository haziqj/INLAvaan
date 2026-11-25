test_that("cov_as_cor changes the report only, not the model after the fit", {
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  fit_with <- function(cov_as_cor) {
    set.seed(1)
    acfa(
      mod,
      lavaan::HolzingerSwineford1939,
      cov_as_cor = cov_as_cor,
      test = "none",
      nsamp = 3,
      silent = TRUE
    )
  }
  fit_cov <- fit_with(FALSE)
  fit_cor <- fit_with(TRUE)

  int <- get_inlavaan_internal(fit_cor)
  expect_true(int$cov_as_cor)
  expect_false(get_inlavaan_internal(fit_cov)$cov_as_cor)
  is_lv_cov <- int$partable$op == "~~" &
    int$partable$lhs != int$partable$rhs &
    int$partable$free > 0
  expect_true(all(int$partable$mat[is_lv_cov] == "psi_cov"))

  loo_cov <- suppressWarnings(loo(fit_cov))$estimates
  loo_cor <- suppressWarnings(loo(fit_cor))$estimates
  expect_equal(loo_cor, loo_cov, tolerance = 1e-8)
  waic_cov <- suppressWarnings(waic(fit_cov))$estimates
  waic_cor <- suppressWarnings(waic(fit_cor))$estimates
  expect_equal(waic_cor, waic_cov, tolerance = 1e-8)
})
