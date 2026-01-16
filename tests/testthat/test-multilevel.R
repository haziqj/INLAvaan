test_that("Method: skewnorm", {
  mod <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
'
  dat <- lavaan::Demo.twolevel
  fit_lav <- lavaan::cfa(mod, dat, cluster = "cluster")

  expect_no_error({
    fit <- asem(mod, dat, cluster = "cluster", verbose = FALSE, nsamp = 3)
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
  expect_equal(coef(fit), coef(fit_lav), tolerance = 0.1)
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-2)
})
