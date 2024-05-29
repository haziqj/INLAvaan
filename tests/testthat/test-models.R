test_that("Fake test", {
  require(INLA)


  n = 100; a = 1; b = 1; tau = 100
  z = rnorm(n)
  eta = a + b*z

  scale = exp(rnorm(n))
  prec = scale*tau
  y = rnorm(n, mean = eta, sd = 1/sqrt(prec))

  data = list(y=y, z=z)
  formula = y ~ 1+z

  expect_no_error({
    result = inla(formula, family = "gaussian", data = data)
  })


})

# test_that("CFA", {
#   INLA::inla.setOption(num.threads = "1:1")
#
#   mod <- "
#     eta1 =~ y1 + y2 + y3
#     eta2 =~ y4 + y5 + y6
#   "
#   dat <- lavaan::simulateData(mod, sample.nobs = 100)
#   expect_no_error({
#     fit <- icfa(
#       model = mod,
#       data = dat,
#       meanstructure = FALSE,
#       verbose = FALSE,
#       npost = 10
#     )
#   })
# })
#
# test_that("SEM", {
#   INLA::inla.setOption(num.threads = "1:1")
#
#   mod <- "
#     eta1 =~ y1 + y2 + y3
#     eta2 =~ y4 + y5 + y6
#     eta2 ~ 0.3*eta1
#     y1 ~~ 0.5*y4
#   "
#   dat <- lavaan::simulateData(mod, sample.nobs = 100)
#   expect_no_error({
#     fit <- isem(
#       model = mod,
#       data = dat,
#       meanstructure = FALSE,
#       verbose = FALSE,
#       npost = 10
#     )
#   })
# })



