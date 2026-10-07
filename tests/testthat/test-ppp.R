dat <- na.omit(lavaan::HolzingerSwineford1939)
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  visual + textual ~ ageyr + grade
"

test_that("The joint moments are rebuilt from conditional.x moments", {
  fit_cx <- lavaan::sem(mod, dat, conditional.x = TRUE)
  fit_jx <- lavaan::sem(mod, dat, meanstructure = TRUE)
  mom <- implied_joint_moments(
    lavaan::lav_model_implied(fit_cx@Model),
    1L,
    fit_cx@SampleStats@x.idx[[1L]]
  )
  implied_jx <- lavaan::lav_model_implied(fit_jx@Model)
  expect_equal(
    mom$cov,
    implied_jx$cov[[1L]],
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    mom$mean,
    as.numeric(implied_jx$mean[[1L]]),
    tolerance = 1e-4
  )
})

test_that("PPP agrees with and without conditional.x", {
  ppp <- vapply(
    c(FALSE, TRUE),
    function(cx) {
      set.seed(1)
      # Silence fit diagnostics on the intercepts, which do not bear on the PPP
      fit <- suppressWarnings(asem(
        mod,
        dat,
        conditional.x = cx,
        nsamp = 500,
        test = "ppp",
        verbose = FALSE
      ))
      get_inlavaan_internal(fit, "ppp")
    },
    numeric(1)
  )
  expect_true(all(ppp >= 0 & ppp <= 1))
  expect_lt(abs(ppp[1] - ppp[2]), 0.1)
})

# A saturated regression on six covariates has no misfit to find. Replicates
# that also vary the 21 covariate moments would push the PPP to about 1.
test_that("Fixed covariates add no misfit to the PPP", {
  set.seed(42)
  n <- 200
  z <- matrix(rnorm(n * 6), n, 6, dimnames = list(NULL, paste0("z", 1:6)))
  dat_sat <- data.frame(z, y = drop(z %*% rep(0.3, 6)) + rnorm(n))
  set.seed(1)
  fit <- asem(
    "y ~ z1 + z2 + z3 + z4 + z5 + z6",
    dat_sat,
    nsamp = 500,
    test = "ppp",
    verbose = FALSE
  )
  ppp <- get_inlavaan_internal(fit, "ppp")
  expect_gt(ppp, 0.2)
  expect_lt(ppp, 0.8)
})

## ----- Composites ------------------------------------------------------------

mod_comp <- "
  C <~ x1 + x2 + x3
  x4 ~ C
  x5 ~ C
  x4 ~~ x5
"

test_that("composite_fixed_t() finds each group's fixed indicator block", {
  fit_lav <- lavaan::sem(mod_comp, dat, group = "school", do.fit = FALSE)
  t_fixed <- composite_fixed_t(fit_lav@Model, fit_lav@ParTable, fit_lav@Data)
  expect_length(t_fixed, 2L)
  for (g in 1:2) {
    e <- t_fixed[[g]]
    # Three variances and three covariances, the covariances in both triangles
    expect_length(e$rows, 6L)
    expect_length(e$pos, 9L)
    expect_true(all(fit_lav@ParTable$group[e$rows] == g))
    expect_equal(
      fit_lav@Model@GLIST[[e$mm]][e$pos],
      fit_lav@SampleStats@cov[[g]][e$rc]
    )
  }

  # A covariance fixed by the user is a constraint, not a plug-in
  fit_user <- lavaan::sem(
    paste(mod_comp, "x1 ~~ 0*x2"),
    dat,
    do.fit = FALSE
  )
  t_user <- composite_fixed_t(fit_user@Model, fit_user@ParTable, fit_user@Data)
  expect_length(t_user[[1]]$rows, 5L)

  # Nothing to find without composites
  fit_cfa <- lavaan::cfa(mod, dat, do.fit = FALSE)
  expect_null(composite_fixed_t(fit_cfa@Model, fit_cfa@ParTable, fit_cfa@Data))
})

# lavaan fixes the covariances of the composite indicators at their sample
# values, so the observed data have no misfit there. Replicates that kept the
# observed values would carry misfit in that block and push the PPP to about
# 0.8. The phantom specification of the same model conditions on the indicators
# as fixed covariates instead, and its PPP is about 0.45.
test_that("Fixed composite indicator covariances add no misfit to the PPP", {
  mod_phantom <- "
    C =~ 0
    C ~ 1*x1 + x2 + x3
    C ~~ 0*C
    x4 ~ C
    x5 ~ C
    x4 ~~ x5
  "
  ppp <- vapply(
    c(mod_comp, mod_phantom),
    function(m) {
      set.seed(1)
      fit <- asem(m, dat, nsamp = 500, test = "ppp", verbose = FALSE)
      get_inlavaan_internal(fit, "ppp")
    },
    numeric(1)
  )
  expect_lt(ppp[[1]], 0.65)
  expect_lt(abs(ppp[[1]] - ppp[[2]]), 0.1)
})

# Two composites of three indicators each with corr(C1, C2) = 0.85, and two
# outcomes regressed on both. The rest of the data relate to the indicators only
# through C = w'x.
composite_sigma <- function(r = 0.85) {
  tm <- matrix(0.4, 3, 3)
  diag(tm) <- 1
  w <- c(1, 0.8, 0.6)
  v <- drop(crossprod(w, tm %*% w))
  wm <- cbind(c(w, 0, 0, 0), c(0, 0, 0, w))
  sxx <- kronecker(diag(2), tm)
  sxx[1:3, 4:6] <- tcrossprod(tm %*% w) * r / v
  sxx[4:6, 1:3] <- t(sxx[1:3, 4:6])
  b <- rbind(c(0.15, 0.1), c(0.05, 0.15))
  sxy <- sxx %*% wm %*% t(b)
  syy <- b %*% crossprod(wm, sxx %*% wm) %*% t(b) + diag(2)
  rbind(cbind(sxx, sxy), cbind(t(sxy), syy))
}
set.seed(2)
dat_two <- as.data.frame(
  matrix(rnorm(300 * 8), 300) %*% chol(composite_sigma())
)
names(dat_two) <- c(paste0("x", 1:6), "y1", "y2")
mod_two <- "
  C1 <~ x1 + x2 + x3
  C2 <~ x4 + x5 + x6
  y1 ~ C1 + C2
  y2 ~ C1 + C2
"

# A replicate's own indicator blocks move each w'Tw. Were the covariance of the
# composites kept at its raw value, their correlation would move with every
# replicate and add misfit that the observed data cannot have. The PPP of this
# correct model would then be about 0.87, where lavaan's p-value is 0.51.
test_that("PPP replicates keep the correlation of two composites", {
  set.seed(1)
  fit <- asem(mod_two, dat_two, nsamp = 500, test = "ppp", verbose = FALSE)
  ppp <- get_inlavaan_internal(fit, "ppp")
  expect_lt(ppp, 0.75)
  expect_gt(ppp, 0.25)
})

# Moments of a lavaan fit at its estimates in a replicate whose composite
# indicator blocks come from s_rep, with the correlations of the latent
# variables and the paths among them.
replicate_moments <- function(fit, s_rep) {
  lavmodel <- fit@Model
  e <- composite_fixed_t(lavmodel, fit@ParTable, fit@Data)[[1L]]
  lavmodel_rep <- lavmodel
  lavmodel_rep@GLIST[[e$mm]][e$pos] <- s_rep[e$rc]
  x <- lavaan::lav_model_get_parameters(lavmodel)
  plan <- composite_scale_plan(lavmodel, fit@ParTable)
  x_rows <- composite_rescale_x(
    x,
    lavaan::lav_model_set_parameters(lavmodel, x),
    lavaan::lav_model_set_parameters(lavmodel_rep, x),
    plan
  )
  m_rep <- composite_set_rows(lavmodel_rep, x_rows, plan)
  psi <- m_rep@GLIST$psi
  ib_inv <- solve(diag(nrow(psi)) - m_rep@GLIST$beta)
  list(
    cov = lavaan::lav_model_implied(m_rep)$cov[[1L]],
    lv_cor = cov2cor(ib_inv %*% psi %*% t(ib_inv)),
    beta = m_rep@GLIST$beta
  )
}

test_that("Replicates keep the standardised relations of composites", {
  fit_cov <- lavaan::sem(mod_two, dat_two)
  s_obs <- fit_cov@SampleStats@cov[[1L]]
  set.seed(3)
  s_rep <- stats::rWishart(1, 299, s_obs)[,, 1] / 299
  obs <- replicate_moments(fit_cov, s_obs)
  rep_cov <- replicate_moments(fit_cov, s_rep)
  # The correlation of the composites stays, and so do the paths out of them
  expect_equal(rep_cov$lv_cor[1:2, 1:2], obs$lv_cor[1:2, 1:2])
  expect_equal(rep_cov$beta, obs$beta)
  # C2 ~ C1 is the same model as C1 ~~ C2
  fit_reg <- lavaan::sem(sub("y1 ~", "C2 ~ C1\n  y1 ~", mod_two), dat_two)
  expect_equal(
    replicate_moments(fit_reg, s_rep)$cov,
    rep_cov$cov,
    tolerance = 1e-5
  )

  # A factor on composites, scaled by the loading on C1 or by std.lv
  mod_ho <- "
    C1 <~ x1 + x2 + x3
    C2 <~ x4 + x5 + x6
    C3 <~ x9 + x7 + x8
    F =~ C1 + C2 + C3
    ageyr ~ F
  "
  fit_mk <- lavaan::sem(mod_ho, dat)
  fit_sl <- lavaan::sem(mod_ho, dat, std.lv = TRUE)
  s_obs <- fit_mk@SampleStats@cov[[1L]]
  s_rep <- stats::rWishart(1, nrow(dat) - 1, s_obs)[,, 1] / (nrow(dat) - 1)
  rep_mk <- replicate_moments(fit_mk, s_rep)
  expect_equal(rep_mk$lv_cor, replicate_moments(fit_mk, s_obs)$lv_cor)
  expect_equal(
    replicate_moments(fit_sl, s_rep)$cov,
    rep_mk$cov,
    tolerance = 1e-5
  )
})
