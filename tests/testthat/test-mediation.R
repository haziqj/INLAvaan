test_that("Method: skewnorm", {
  set.seed(1234)
  X <- rnorm(100)
  M <- 0.5 * X + rnorm(100)
  Y <- 0.7 * M + rnorm(100)

  dat <- data.frame(X = X, Y = Y, M = M)
  mod <- "
    # Direct effect
    Y ~ c*X

    # Mediators
    M ~ a*X
    Y ~ b*M

    # Indirect effect (a*b)
    ab := a*b

    # Total effect
    total := c + (a*b)
  "

  fit_lav <- lavaan::sem(mod, dat)
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      verbose = FALSE
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Defined parameters can use other defined parameters", {
  set.seed(1234)
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    textual ~ a*visual
    speed   ~ b*textual + c*visual

    # total uses ind before it is defined, and twice uses total after it
    total := c + ind
    ind   := a*b
    twice := 2*total
  "
  dat <- lavaan::HolzingerSwineford1939
  fit_lav <- lavaan::sem(mod, dat)
  expect_no_error({
    fit <- asem(mod, dat, verbose = FALSE, nsamp = 100, test = "none")
  })

  # Compare with the ML values implied by lavaan's a, b and c, each on the
  # scale of its posterior SD
  est <- lavaan::coef(fit_lav)
  ind <- est[["a"]] * est[["b"]]
  summ <- get_inlavaan_internal(fit)$summary
  expected <- c(
    ind = ind,
    total = est[["c"]] + ind,
    twice = 2 * (est[["c"]] + ind)
  )
  for (nm in names(expected)) {
    expect_lt(abs(summ[nm, "Mean"] - expected[[nm]]), summ[nm, "SD"])
  }
  expect_equal(
    summ["twice", "Mean"],
    2 * summ["total", "Mean"],
    tolerance = 1e-3
  )

  # Standardised := rows sit on their own rows
  std <- standardisedsolution(fit, nsamp = 20)
  std_est <- function(lhs, op, rhs) {
    std$est.std[std$lhs == lhs & std$op == op & std$rhs == rhs]
  }
  expect_equal(
    std_est("total", ":=", "c+ind"),
    std_est("speed", "~", "visual") + std_est("ind", ":=", "a*b")
  )
  expect_no_error(out <- capture.output(summary(fit)))
})

test_that("summary() matches defined parameters under equality constraints", {
  set.seed(1234)
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    textual ~ b*visual
    d := 2*b
  "
  fit <- asem(
    mod,
    lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = c("loadings", "regressions"),
    verbose = FALSE,
    nsamp = 100,
    test = "none"
  )
  expect_no_warning(out <- capture.output(summary(fit)))

  summ <- get_inlavaan_internal(fit)$summary
  def_line <- grep("^\\s+d\\s", out, value = TRUE)
  expect_match(def_line, formatC(summ["d", "SD"], digits = 3, format = "f"))
  expect_match(
    def_line,
    formatC(summ["d", "97.5%"], digits = 3, format = "f")
  )
})

test_that("Defined parameters summarise the draws where they are defined", {
  set.seed(1234)
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    textual ~ a*visual
    speed   ~ b*textual + c*visual

    # The posterior of b crosses zero, so lb is undefined for some draws and
    # nb for all of them.
    lb := log(b)
    nb := log(-b^2)
    k  := 2
  "
  dat <- lavaan::HolzingerSwineford1939
  for (sn_fit_sample in c(TRUE, FALSE)) {
    expect_warning(
      fit <- asem(
        mod,
        dat,
        verbose = FALSE,
        nsamp = 200,
        test = "none",
        sn_fit_sample = sn_fit_sample
      ),
      "could not be computed"
    )
    int <- get_inlavaan_internal(fit)
    summ <- int$summary
    lb <- unlist(summ["lb", c("Mean", "SD", "2.5%", "97.5%")])
    expect_true(all(is.finite(lb)))
    expect_true(all(is.na(summ["nb", c("Mean", "SD")])))
    expect_equal(
      unlist(summ["k", c("Mean", "SD", "Mode")]),
      c(Mean = 2, SD = 0, Mode = 2)
    )
    expect_named(int$def_undefined, c("lb", "nb", "k"))
    expect_gt(int$def_undefined[["lb"]], 0)
    expect_equal(int$def_undefined[c("nb", "k")], c(nb = 1, k = 0))
    expect_false(any(c("nb", "k") %in% names(int$pdf_data)))
  }

  expect_no_warning(std <- standardisedsolution(fit, nsamp = 20))
  expect_true(is.finite(std$est.std[std$lhs == "lb"]))
  expect_true(is.na(std$est.std[std$lhs == "nb"]))
  out <- capture.output(summary(fit))
  expect_true(any(grepl("lb: undefined in", out)))
  expect_error(plot(fit, params = "nb"), "No posterior density")
  expect_error(
    asem(mod, dat, verbose = FALSE, nsamp = 1, test = "none"),
    "at least 2"
  )
})

test_that("standardisedsolution() warns about := undefined only when standardised", {
  set.seed(1234)
  # vs is about 0.4, but 1 on the standardised scale
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    speed ~~ vs*speed
    d := log(0.9 - vs)
  "
  expect_no_warning(
    fit <- acfa(
      mod,
      lavaan::HolzingerSwineford1939,
      verbose = FALSE,
      nsamp = 100,
      test = "none"
    )
  )
  expect_warning(std <- standardisedsolution(fit, nsamp = 20), "std.all")
  expect_true(is.na(std$est.std[std$lhs == "d"]))
})

test_that("Undefined-draw warnings format shares and escape labels", {
  expect_equal(
    format_share(c(0.0004, 0.163, 0.9996, 1)),
    c("<0.1% of draws", "16.3% of draws", ">99.9% of draws", "every draw")
  )
  expect_warning(
    warn_undefined_draws(c("a{b}" = 0.5), n_defined = 10),
    "`a{b}`: undefined in 50.0% of draws",
    fixed = TRUE
  )
})

test_that("muffle_nan_warnings() muffles only NaN warnings", {
  expect_no_warning(muffle_nan_warnings(log(-1)))
  expect_warning(muffle_nan_warnings(warning("something else")), "else")
})

test_that("A := undefined only at the start values fits without warnings", {
  set.seed(1234)
  # a starts at 0 but its posterior is near 0.5
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    textual ~ a*visual
    la := log(a - 0.1)
  "
  expect_no_warning(
    asem(
      mod,
      lavaan::HolzingerSwineford1939,
      verbose = FALSE,
      nsamp = 100,
      test = "none"
    )
  )
})
