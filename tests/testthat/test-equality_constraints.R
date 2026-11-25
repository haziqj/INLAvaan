mod <- "
  # intercept with coefficients fixed to 1
  i =~  1*Day0 + 1*Day1 + 1*Day2 + 1*Day3 + 1*Day4 +
        1*Day5 + 1*Day6 + 1*Day7 + 1*Day8 + 1*Day9

  # slope with coefficients fixed to 0:9 (number of days)
  s =~  0*Day0 + 1*Day1 + 2*Day2 + 3*Day3 + 4*Day4 +
        5*Day5 + 6*Day6 + 7*Day7 + 8*Day8 + 9*Day9

  i ~~ i
  i ~ 1

  s ~~ s
  s ~ 1

  i ~~ s

  # fix intercepts
  Day0 ~ 0*1
  Day1 ~ 0*1
  Day2 ~ 0*1
  Day3 ~ 0*1
  Day4 ~ 0*1
  Day5 ~ 0*1
  Day6 ~ 0*1
  Day7 ~ 0*1
  Day8 ~ 0*1
  Day9 ~ 0*1

  # apply equality constraints
  Day0 ~~ v*Day0
  Day1 ~~ v*Day1
  Day2 ~~ v*Day2
  Day3 ~~ v*Day3
  Day4 ~~ v*Day4
  Day5 ~~ v*Day5
  Day6 ~~ v*Day6
  Day7 ~~ v*Day7
  Day8 ~~ v*Day8
  Day9 ~~ v*Day9
  "
# The sleep study data in wide form. lme4 is only suggested, so the tests that
# use these data skip without it.
sleep_wide <- function() {
  skip_if_not_installed("lme4")
  dat <- reshape(
    lme4::sleepstudy,
    timevar = "Days",
    idvar = "Subject",
    direction = "wide"
  )
  names(dat) <- sub("^Reaction\\.(.*)$", "Day\\1", names(dat))
  dat
}
NSAMP <- 3

test_that("Method: skewnorm", {
  dat <- sleep_wide()
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "skewnorm",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  gr_at_opt <- fit@optim$dx
  gt_at_opt <- as.numeric(fit@Model@ceq.simple.K %*% gr_at_opt)
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(gt_at_opt, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: asymgaus", {
  dat <- sleep_wide()
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "asymgaus",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: marggaus", {
  dat <- sleep_wide()
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "marggaus",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: sampling", {
  dat <- sleep_wide()
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "sampling",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Gradients are correct (Finite Difference Check)", {
  # The engine compares the analytic gradient at the mode with central
  # differences of the log-posterior, and grad_gap is the gap in posterior
  # SDs.
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  dat <- sleep_wide()
  fit <- agrowth(mod, dat, test = "none", silent = TRUE, nsamp = 3)
  gap <- get_inlavaan_internal(fit)$engine$diagnostics$marginals$grad_gap
  expect_true(all(is.finite(gap)))
  expect_lt(max(gap), 0.1)
})

test_that("Covariances held equal have the right gradient", {
  skip_on_cran()
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    visual ~~ c1*textual + c1*speed
  "
  dat <- lavaan::HolzingerSwineford1939
  expect_no_warning(
    fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, test = "none")
  )
  # The analytic gradient agrees with central differences of the
  # log-posterior at the mode
  expect_lt(diagnostics(fit)[["grad_gap_max"]], 0.1)
  int <- get_inlavaan_internal(fit)

  # The posterior mode sits at the ML estimate
  pt <- int$partable
  x_mode <- pars_to_x(
    as.numeric(fit@Model@ceq.simple.K %*% int$theta_star_novbc),
    pt
  )
  c1_mode <- x_mode[pt$free[pt$label == "c1"][1]]
  expect_equal(c1_mode, coef(lavaan::cfa(mod, dat))[["c1"]], tolerance = 0.05)
})

test_that("sampling() works with equality constraints", {
  skip_on_cran()
  fit <- acfa(
    "visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6",
    lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = "loadings",
    silent = TRUE,
    nsamp = NSAMP,
    test = "none"
  )
  npar <- fit@Model@nx.free
  expect_equal(ncol(sampling(fit, type = "lavaan", nsamp = 2)), npar)
  expect_equal(ncol(sampling(fit, type = "theta", nsamp = 2)), npar)
  expect_no_error(sampling(fit, type = "all", nsamp = 2))
})

test_that("Covariances held equal across groups fit", {
  skip_on_cran()
  fit <- acfa(
    "visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6",
    lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = c("loadings", "lv.covariances"),
    silent = TRUE,
    nsamp = NSAMP,
    test = "none"
  )
  int <- get_inlavaan_internal(fit)
  expect_length(int$coefficients, fit@Model@nx.free)
  expect_no_error(capture.output(summary(fit)))
})
