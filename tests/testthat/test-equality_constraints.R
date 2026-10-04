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
dat <- reshape(
  lme4::sleepstudy,
  timevar = "Days",
  idvar = "Subject",
  direction = "wide"
)
names(dat) <- sub("^Reaction\\.(.*)$", "Day\\1", names(dat))
fit_lav <- lavaan::growth(mod, dat)
NSAMP <- 3

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "skewnorm",
      verbose = FALSE,
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
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "asymgaus",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: marggaus", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: sampling", {
  expect_no_error({
    fit <- agrowth(
      mod,
      dat,
      marginal_method = "sampling",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Gradients are correct (Finite Difference Check)", {
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  suppressMessages(
    tmp <- capture.output(fit <- agrowth(mod, dat, test = "none", debug = TRUE))
  )
  test_df <- read.table(text = tmp, skip = 1)[, -1]
  colnames(test_df) <- c("fd", "analytic", "diff")

  expect_equal(
    as.numeric(test_df$fd),
    as.numeric(test_df$diff),
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(test_df$diff),
    rep(0, nrow(test_df)),
    tolerance = 1e-3
  )
})

test_that("Covariances held equal have the right gradient", {
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
  int <- get_inlavaan_internal(fit)
  expect_lt(max(abs(int$opt$dx_analytic - int$opt$dx)), 1e-4)

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
  fit <- acfa(
    "visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6",
    lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = "loadings",
    verbose = FALSE,
    nsamp = NSAMP,
    test = "none"
  )
  npar <- fit@Model@nx.free
  expect_equal(ncol(sampling(fit, type = "lavaan", nsamp = 2)), npar)
  expect_equal(ncol(sampling(fit, type = "theta", nsamp = 2)), npar)
  expect_no_error(sampling(fit, type = "all", nsamp = 2))
})
