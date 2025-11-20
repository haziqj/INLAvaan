# test_that("Fake test", {
#   require(INLA)
#
#   rgeneric.test = function(
#     cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
#     theta = NULL)
#   {
#     envir = parent.env(environment())
#
#     graph = function() {
#       return(matrix(1, n, n))
#     }
#
#     Q = function() {
#       R <- matrix(sin(1:n^2), n, n)
#       R <- R %*% t(R)
#       diag(R) <- diag(R)+1
#       Q <- exp(theta[1]) * R
#       return(Q)
#     }
#
#     mu = function() { return(numeric(0)) }
#
#     log.norm.const = function() { return(numeric(0)) }
#
#     log.prior = function() {
#       return (dgamma(exp(theta[1]), shape = 1, rate = 1, log=TRUE) + theta[1])
#     }
#
#     initial = function() { return(4) }
#
#     if (!length(theta)) theta = initial()
#     val = do.call(match.arg(cmd), args = list())
#     return (val)
#   }
#
#   n = 100
#   s = .1
#   Q <- rgeneric.test("Q", theta = 0)
#   library(mvtnorm)
#   S <- solve(as.matrix(Q))
#   S <- (S + t(S))/2
#   x <- drop(rmvnorm(1, sigma = S))
#   y <- x + rnorm(n, sd = s)
#   cont.family = list(hyper = list(prec = list(initial=log(1/s^2), fixed=TRUE)))
#   model2 = inla.rgeneric.define(rgeneric.test, n=n, optimize = FALSE)
#
#   expect_no_error({
#     r2 = inla(y ~ -1 + f(idx, model=model2),
#               data = data.frame(y = y, idx = 1:n), control.family = cont.family)
#   })
#
#
# })

# test_that("CFA", {
#   # INLA::inla.setOption(num.threads = "1:1")
#
#   mod <- "
#     eta1 =~ y1 + y2 + y3
#     eta2 =~ y4 + y5 + y6
#   "
#   dat <- lavaan::simulateData(mod, sample.nobs = 1000)
#   expect_no_error({
#     fit <- incfa(
#       model = mod,
#       data = dat,
#       meanstructure = FALSE,
#       verbose = FALSE,
#       npost = 100
#     )
#   })
# })

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



