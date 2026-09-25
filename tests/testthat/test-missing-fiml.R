# Single-level FIML fits: the saturated-means fast path must stay off, so that
# the intercept block of the Hessian, the VB shift and the marginal scans are
# computed like every other parameter, and the PPP must use the saturated
# (EM) covariance rather than the incomplete sample covariance.

set.seed(5)
hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
hs$x1[sample(nrow(hs), 60)] <- NA
hs$x4[sample(nrow(hs), 60)] <- NA
mod_hs <- "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
"
fit_fiml <- acfa(mod_hs, hs, missing = "ml", verbose = FALSE, nsamp = 100)
ml_fiml <- lavaan::cfa(mod_hs, hs, missing = "ml")

test_that("the saturated-means fast path is off under FIML", {
  int <- get_inlavaan_internal(fit_fiml)
  expect_null(saturated_mean_idx(
    int$partable,
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    FALSE
  ))
})

test_that("FIML posterior summaries agree with lavaan's FIML estimates", {
  ld <- grep("=~", names(coef(ml_fiml)))
  expect_equal(
    unname(coef(fit_fiml)[ld]),
    unname(coef(ml_fiml)[ld]),
    tolerance = 0.1
  )
  sd_i <- sqrt(diag(vcov(fit_fiml)))
  se_l <- sqrt(diag(vcov(ml_fiml)))
  expect_equal(unname(sd_i[ld]), unname(se_l[ld]), tolerance = 0.25)
  # The skew-normal marginals sit inside the scanned window again
  expect_lt(diagnostics(fit_fiml)["scan_end_mass_max"], 0.05)
})

test_that("PPP under FIML tracks the complete-data PPP for the same data", {
  set.seed(11)
  n <- 500
  eta <- rnorm(n)
  lam <- c(0.8, 0.7, 0.6, 0.7, 0.8)
  Y <- sapply(lam, function(l) l * eta + rnorm(n, sd = sqrt(1 - l^2)))
  colnames(Y) <- paste0("y", 1:5)
  Y <- as.data.frame(Y)
  Ym <- Y
  Ym$y1[sample(n, 100)] <- NA
  Ym$y2[sample(n, 100)] <- NA
  mod <- "f =~ y1 + y2 + y3 + y4 + y5"
  fit_c <- acfa(mod, Y, verbose = FALSE, nsamp = 300,
                marginal_method = "marggaus", vb_correction = FALSE)
  fit_m <- acfa(mod, Ym, missing = "ml", verbose = FALSE, nsamp = 300,
                marginal_method = "marggaus", vb_correction = FALSE)
  ppp_c <- unname(fitmeasures(fit_c, "ppp"))
  ppp_m <- unname(fitmeasures(fit_m, "ppp"))
  # Before the fix the FIML value was 0.000 for every correct model
  expect_gt(ppp_m, 0.05)
  expect_lt(abs(ppp_m - ppp_c), 0.3)
})
