dat <- lavaan::HolzingerSwineford1939
base <- "visual =~ x1 + x2 + x3\n speed =~ x7 + x8 + x9\n"
fit_with <- function(mod, ...) {
  set.seed(1)
  acfa(paste0(base, mod), dat, verbose = FALSE, nsamp = 3, test = "none", ...)
}

test_that("a == b gives the same fit as a shared label", {
  fit_eq <- fit_with("textual =~ x4 + a*x5 + b*x6\n a == b")
  fit_lab <- fit_with("textual =~ x4 + a*x5 + a*x6")
  expect_equal(coef(fit_eq)[["a"]], coef(fit_eq)[["b"]])
  expect_equal(coef(fit_eq)[["a"]], coef(fit_lab)[["a"]])
  expect_equal(fit_eq@Model@nx.free, fit_lab@Model@nx.free)
  expect_no_error(capture.output(summary(fit_eq)))
})

test_that("Constraints keep shared labels and group.equal in force", {
  mod <- "textual =~ x4 + x5 + x6\n x1 ~~ c(v1, v1b)*x1\n x2 ~~ c(v2, v2b)*x2"
  fit <- fit_with(
    paste0(mod, "\n v1 == v2"),
    group = "school",
    group.equal = "loadings"
  )
  # 60 free parameters, less 6 equal loadings and v1 == v2
  expect_equal(fit@Model@nx.free, 53)
  expect_equal(coef(fit)[["v1"]], coef(fit)[["v2"]])
  pt <- lavaan::parTable(fit)
  x2 <- pt$est[pt$lhs == "visual" & pt$op == "=~" & pt$rhs == "x2"]
  expect_equal(x2[1], x2[2])

  # A variance bound that the parameterisation already guarantees
  fit_bound <- fit_with(
    "textual =~ x4 + x5 + x6\n x1 ~~ v*x1\n v > 0",
    group = "school",
    group.equal = "loadings"
  )
  expect_equal(fit_bound@Model@nx.free, 53)
})

test_that("a == <value> fixes the parameter", {
  fit <- fit_with("textual =~ x4 + a*x5 + x6\n a == 0.5")
  pt <- lavaan::parTable(fit)
  i <- which(pt$label == "a")
  expect_equal(pt$free[i], 0)
  expect_equal(pt$est[i], 0.5)

  # Equal to the fixed marker loading
  fit <- fit_with("textual =~ a*x4 + b*x5 + x6\n b == a")
  pt <- lavaan::parTable(fit)
  i <- which(pt$label == "b")
  expect_equal(pt$free[i], 0)
  expect_equal(pt$est[i], 1)
})

test_that("Unsupported constraints give an error", {
  expect_error(
    fit_with("textual =~ x4 + a*x5 + b*x6\n a == 2*b"),
    "single parameter or a number"
  )
  expect_error(
    fit_with("textual =~ x4 + a*x5 + x6\n a > 1.5"),
    "inequality"
  )
  expect_error(
    fit_with("textual =~ x4 + a*x5 + b*x6\n a > b"),
    "inequality"
  )
  expect_error(
    fit_with("textual =~ x4 + a*x5 + b*x6\n d := a - b\n d == 0"),
    "defined parameters"
  )
  expect_error(
    fit_with("textual =~ x4 + a*x5 + x6\n x6 ~~ b*x6\n a == b"),
    "cannot hold these parameters equal"
  )
  expect_error(
    fit_with("textual =~ x4 + a*x5 + x6\n x6 ~~ a*x6"),
    "cannot hold these parameters equal"
  )
  expect_error(fit_with("", effect.coding = "loadings"), "effect.coding")
})

test_that("Two-level fits with a constraint keep working lavaan methods", {
  set.seed(1)
  fit <- asem(
    "level: 1\n fw =~ y1 + a*y2 + y3\n level: 2\n fb =~ y1 + b*y2 + y3\n a == b",
    lavaan::Demo.twolevel,
    cluster = "cluster",
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  expect_true(is.numeric(lavaan::lavInspect(fit, "est")$within$lambda))
  expect_true(is.numeric(unlist(lavaan::lavInspect(fit, "rsquare"))))
  expect_no_error(capture.output(summary(fit, rsquare = TRUE)))
})
