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
