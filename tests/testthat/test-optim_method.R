dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
NSAMP <- 3

test_that("Method: nlminb with numerical grad", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      numerical_grad = TRUE,
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: ucminf", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      optim_method = "ucminf",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: optim", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      optim_method = "optim",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-2)
})
