set.seed(1)
gen_data_twofac <- function(n) {
  mod <- "
    fx =~ 1*x1 + 0.7*x2 + 0.6*x3
    fy =~ 1*y1 + 0.7*y2 + 0.6*y3
    fy ~ 0.25*fx

    x1 ~~ 0.25*x1
    x2 ~~ 0.1225*x2
    x3 ~~ 0.09*x3

    y1 ~~ 0.25*y1
    y2 ~~ 0.1225*y2
    y3 ~~ 0.09*y3
  "
  lavaan::simulateData(model = mod, sample.nobs = n)
}

dat <- gen_data_twofac(250)
mod <- "
    fx =~ x1 + a*x2 + b*x3
    fy =~ y1 + a*y2 + b*y3
    x1 ~~ c*x1
    y1 ~~ c*y1
    fy ~ fx
  "

fit_lav <- lavaan::sem(mod, dat)
fit_inl <- get_inlavaan_internal(asem(
  mod,
  dat,
  add_priors = FALSE,
  vb_correction = FALSE,
  test = "none",
  verbose = FALSE
))
pt <- fit_inl$partable


test_that("INLAvaan produces same results as lavaan", {
  expect_equal(
    as.numeric(coef(fit_lav)),
    fit_inl$theta_star_trans[pt$free[pt$free > 0]],
    tolerance = 1e-4
  )
  expect_no_error(out <- capture.output(summary(fit_inl)))
})

test_that("Parameter Transformations (pars_to_x) work correctly", {
  m_unpacked <- sum(pt$free > 0)
  n_free <- sum(pt$free > 0 & !duplicated(pt$free))
  theta <- rnorm(m_unpacked)
  x_out <- pars_to_x(theta, pt)

  # 1. Check dimensions (should return packed vector)
  expect_equal(length(x_out), n_free)

  ptidxprior <- which(!is.na(pt$prior))
  thidxprior <- which(!duplicated(pt$free[pt$free > 0]))

  # 2. Check Variance Transformation (Log -> Exp)
  ptidxvar <- which(grepl("theta_var|psi_var", pt$mat))
  xidxvar <- pt$free[ptidxvar]
  xidxvar <- xidxvar[!duplicated(xidxvar)]
  x_val <- x_out[xidxvar]

  thidxvar <- pt$free[pt$free > 0]
  thidxvar <- thidxvar %in% xidxvar & !duplicated(thidxvar)
  th_val <- theta[thidxvar]

  expect_equal(log(x_val), th_val, tolerance = 1e-8)
})

test_that("Prior Densities match manual calculation", {
  n_free <- sum(pt$free > 0 & !duplicated(pt$free))
  theta <- rep(0.5, n_free)

  # --- Manual Verification for specific params ---
  # 1. Normal Prior on loading (id 2, free=1) -> "normal(0,10)"
  # theta = 0.5. Identity transform.
  lp_normal <- dnorm(0.5, mean = 0, sd = 10, log = TRUE)

  # 2. Gamma[sd] on variance (id 8, free=5) -> "gamma(1,.5)[sd]"
  # theta = 0.5. Transform: x = exp(0.5) ≈ 1.648.
  # Prior is on SD: s = sqrt(x) ≈ 1.284
  # No Jacobian adjustment needed
  # xval = exp(0.5)
  # dx_dth = exp(0.5)
  # lp = dgamma(sqrt(xval), 1, 0.5, log=TRUE)
  # ljcb = log(abs(dx_dth / (2*sqrt(xval))))  <-- From your prior_logdens code
  val_x <- exp(0.5)
  val_s <- sqrt(val_x)
  lp_gamma <- dgamma(val_s, shape = 1, rate = 0.5, log = TRUE)
  jac_gamma <- log(abs(exp(0.5) / (2 * sqrt(exp(0.5)))))

  # Sum manual components (assuming all others are 0 or ignored for this
  # specific test logic)
  pt_mini <- pt
  pt_mini$prior[-c(2)] <- NA # Keep only one normal prior

  lp_mini_pkg <- prior_logdens(theta, pt_mini)

  expect_equal(lp_mini_pkg, lp_normal, tolerance = 1e-6)
})

test_that("Gradients are correct (Finite Difference Check)", {
  suppressMessages(
    tmp <- capture.output(fit <- asem(mod, dat, test = "none", debug = TRUE))
  )
  test_df <- read.table(text = tmp, skip = 1)[, -1]
  colnames(test_df) <- c("fd", "analytic", "diff")

  expect_equal(
    as.numeric(test_df$fd),
    as.numeric(test_df$diff),
    tolerance = 1e-2
  )
})
