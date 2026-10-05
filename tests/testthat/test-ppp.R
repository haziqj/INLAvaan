dat <- na.omit(lavaan::HolzingerSwineford1939)
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  visual + textual ~ ageyr + grade
"

test_that("The joint covariance is rebuilt from conditional.x moments", {
  fit_cx <- lavaan::sem(mod, dat, conditional.x = TRUE)
  fit_jx <- lavaan::sem(mod, dat)
  Sigma <- implied_joint_cov(
    lavaan::lav_model_implied(fit_cx@Model),
    1L,
    fit_cx@SampleStats@x.idx[[1L]]
  )
  expect_equal(
    Sigma,
    lavaan::lav_model_implied(fit_jx@Model)$cov[[1L]],
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("PPP agrees with and without conditional.x", {
  ppp <- vapply(
    c(FALSE, TRUE),
    function(cx) {
      set.seed(1)
      fit <- asem(
        mod,
        dat,
        conditional.x = cx,
        nsamp = 500,
        test = "ppp",
        verbose = FALSE
      )
      get_inlavaan_internal(fit, "ppp")
    },
    numeric(1)
  )
  expect_true(all(ppp >= 0 & ppp <= 1))
  expect_lt(abs(ppp[1] - ppp[2]), 0.1)
})
