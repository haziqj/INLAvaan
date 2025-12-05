dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
DP <- blavaan::dpriors(theta = "gamma(1,1)", psi = "gamma(1,1)")
NSAMP <- 3
fit <- asem(
  mod,
  dat,
  dp = DP,
  verbose = FALSE,
  nsamp = NSAMP
)

test_that("Setting dp argument", {
  pt <- lavaan::partable(fit)
  where_theta_var <- grep("theta_var", pt$mat)
  expect_true(all(pt$prior[where_theta_var] == "gamma(1,1)"))
  where_psi_var <- grep("psi_var", pt$mat)
  expect_true(all(pt$prior[where_psi_var] == "gamma(1,1)"))
})

test_that("Straight from textual model", {
  mod <- "
    visual  =~ x1 + prior('normal(1,2)')*x2 + x3
    textual =~ x4 + x5 + prior('normal(3,1.5)')*x6
    speed   =~ x7 + x8 + x9

    x1 ~~ prior('gamma(3,3)')*x1
  "

  expect_no_error({
    fit <- asem(
      mod,
      dat,
      verbose = FALSE,
      nsamp = NSAMP
    )
  })

  pt <- lavaan::partable(fit)
  expect_true(pt$prior[grep("visual=~x2", pt$names)] == "normal(1,2)")
  expect_true(pt$prior[grep("textual=~x6", pt$names)] == "normal(3,1.5)")
  expect_true(pt$prior[grep("x1~~x1", pt$names)] == "gamma(3,3)")

  fit_inlv <- get_inlavaan_internal(fit)
  pt <- fit_inlv$partable
  idx <- which(
    pt$names %in% c("visual=~x2", "textual=~x6", "x1~~x1", "textual~~textual")
  )
  res <- prior_logdens(
    c(2, 4, 1, 1),
    lapply(pt, function(x) x[idx]),
    debug = TRUE
  )

  # Check log densities
  expect_equal(
    as.numeric(res$lp),
    c(
      dnorm(2, 1, 2, log = TRUE),
      dnorm(4, 3, 1.5, log = TRUE),
      dgamma(exp(1), 3, 3, log = TRUE),
      dgamma(exp(1 / 2), 1, 0.5, log = TRUE)
    )
  )

  # Check jacobians
  expect_equal(
    as.numeric(res$ljcb),
    c(
      0, # log(abs(1))
      0, # log(abs(1))
      log(abs(exp(1))), # t = log x  ---> log(abs(d/dt e^t))
      log(abs(0.5 * exp(1 / 2))) # t = log(sqrt(x)) ---> log(abs(0.5 exp^{t/2}))
    )
  )
})
