test_that("CFA", {
  INLA::inla.setOption(num.threads = "1:1")

  mod <- "
    eta1 =~ y1 + y2 + y3
    eta2 =~ y4 + y5 + y6
  "
  dat <- lavaan::simulateData(mod, sample.nobs = 100)
  expect_no_error({
    fit <- icfa(
      model = mod,
      data = dat,
      meanstructure = FALSE,
      verbose = FALSE,
      npost = 10
    )
  })
})

test_that("SEM", {
  INLA::inla.setOption(num.threads = "1:1")

  mod <- "
    eta1 =~ y1 + y2 + y3
    eta2 =~ y4 + y5 + y6
    eta2 ~ 0.3*eta1
    y1 ~~ 0.5*y4
  "
  dat <- lavaan::simulateData(mod, sample.nobs = 100)
  expect_no_error({
    fit <- isem(
      model = mod,
      data = dat,
      meanstructure = FALSE,
      verbose = FALSE,
      npost = 10
    )
  })
})



