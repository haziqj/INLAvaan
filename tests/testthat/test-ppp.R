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
