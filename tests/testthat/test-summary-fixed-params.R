test_that("Fixed marker loadings are reported with their fixed values", {
  pop_mod <- "
    eta1 =~ 1*y1 + 0.8*y2 + 0.6*y3
    eta2 =~ 1*y4 + 0.8*y5 + 0.6*y6
    eta2 ~ 0.3*eta1

    y1 ~~ 0.5*y1
    y2 ~~ 0.5*y2
    y3 ~~ 0.5*y3
    y4 ~~ 0.5*y4
    y5 ~~ 0.5*y5
    y6 ~~ 0.5*y6
    eta1 ~~ 1*eta1
    eta2 ~~ 1*eta2
  "
  mod <- "
    eta1 =~ y1 + y2 + y3
    eta2 =~ y4 + y5 + y6
    eta2 ~ eta1
  "

  set.seed(123)
  dat <- lavaan::simulateData(pop_mod, sample.nobs = 1000)
  fit <- asem(mod, dat, verbose = FALSE, test = "none")

  pe <- lavaan::parameterEstimates(fit, ci = FALSE, standardized = FALSE)
  idx_eta1 <- which(pe$lhs == "eta1" & pe$op == "=~" & pe$rhs == "y1")
  idx_eta2 <- which(pe$lhs == "eta2" & pe$op == "=~" & pe$rhs == "y4")

  expect_length(idx_eta1, 1L)
  expect_length(idx_eta2, 1L)
  expect_equal(pe$est[idx_eta1], 1)
  expect_equal(pe$est[idx_eta2], 1)
  expect_false(is.na(pe$est[idx_eta1]))
  expect_false(is.na(pe$est[idx_eta2]))
})
