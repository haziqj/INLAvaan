test_that("split_modifiers() writes chained modifiers as separate terms", {
  expect_equal(
    split_modifiers('f =~ x1 + prior("normal(3,1)")*a*x2 + x3'),
    'f =~ x1 + prior("normal(3,1)")*x2 + a*x2 + x3'
  )
  expect_equal(
    split_modifiers("f =~ x1 + c(a1,a2)*c(1,NA)*x2"),
    "f =~ x1 + c(a1,a2)*x2 + c(1,NA)*x2"
  )
  # Continuation lines, comments and intercepts
  expect_equal(
    split_modifiers(
      'f =~ x1 + x2 +\n  start(1)*a*x3 # a*b*c\ny ~ prior("normal(0,1)")*b*1'
    ),
    'f =~ x1 + x2 + start(1)*x3 + a*x3\ny ~ prior("normal(0,1)")*1 + b*1'
  )
  # Products in := and constraints stay as they are
  expect_equal(
    split_modifiers("f =~ x1 + 0.5*a*x2\nab := a*b*c\na == 2*b*c"),
    "f =~ x1 + 0.5*x2 + a*x2\nab := a*b*c\na == 2*b*c"
  )
  # Nothing chained: the model comes back untouched, even with single-term
  # statements, several strings, or a `;` inside a comment
  for (mod in list(
    "f =~ x1 + a*x2 + x3 # note\n",
    "f =~ x1 + x2 + x3\n\ny ~ f # a*b*c\n",
    c("f =~ x1 + x2", "f ~~ 1*f"),
    "f =~ x1 + x2 # two items; see notes\n# tried: x1 ~~ x4; x2 ~~ x5\n"
  )) {
    expect_identical(split_modifiers(mod), mod)
  }
  # A `;` inside a comment does not become syntax
  expect_equal(
    split_modifiers("f =~ x1 + start(1)*a*x2 # note; x1 ~~ x4"),
    "f =~ x1 + start(1)*x2 + a*x2"
  )
})

test_that("Chained modifiers keep their prior and fixed value", {
  dat <- lavaan::HolzingerSwineford1939
  base <- "visual =~ x1 + x2 + x3\n textual =~ x4 + "
  fit_x5 <- function(rhs) {
    set.seed(1)
    # The tight prior pulls the loading away from the data, which the fit
    # diagnostics notice
    fit <- suppressWarnings(acfa(
      paste0(base, rhs, " + x6"),
      dat,
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    ))
    pt <- get_inlavaan_internal(fit)$partable
    i <- which(pt$lhs == "textual" & pt$op == "=~" & pt$rhs == "x5")
    list(fit = fit, pt = pt, i = i)
  }

  res <- fit_x5('prior("normal(3,0.01)")*a*x5')
  expect_equal(res$pt$label[res$i], "a")
  expect_equal(res$pt$prior[res$i], "normal(3,0.01)")
  expect_equal(coef(res$fit)[["a"]], 3, tolerance = 0.01)

  res <- fit_x5("0.5*a*x5")
  expect_equal(res$pt$free[res$i], 0)
  expect_equal(res$pt$est[res$i], 0.5)
})
