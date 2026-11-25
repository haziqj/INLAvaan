dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
NSAMP <- 3

test_that("Method: nlminb with numerical grad", {
  # Pins the optimisers of the old fit stages
  local_engine(FALSE)
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      numerical_grad = TRUE,
      silent = TRUE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: ucminf", {
  # Pins the optimisers of the old fit stages
  local_engine(FALSE)
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      optim_method = "ucminf",
      silent = TRUE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  # See note in "Method: nlminb with numerical grad" above.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: optim", {
  # Pins the optimisers of the old fit stages
  local_engine(FALSE)
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      optim_method = "optim",
      silent = TRUE,
      nsamp = NSAMP
    )
  })

  expect_s4_class(fit, "INLAvaan")
  # See note in "Method: nlminb with numerical grad" above.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-2)
})
