test_that("vcov and fitMeasures", {
  testthat::skip() # FIXME

  data("PoliticalDemocracy", package = "lavaan")
  model <- "
    # Latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8

    # Latent regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

    # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
  "
  fit <- asem(
    model,
    PoliticalDemocracy,
    verbose = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none",
    test = "none",
    nsamp = 2
  )

  ## to get vcov, it seems we need to set @Options$se?
  # library(lavaan)
  # fit@Options$se <- "standard"
  expect_error(vcov(fit))

  ## would be nice, even if it just returns dic and ppp:
  expect_error(fitMeasures(fit))
})

test_that("Missing data", {
  data("PoliticalDemocracy", package = "lavaan")
  model <- "
    # Latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8

    # Latent regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

    # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
  "

  # ECM: Any possibility of a "full information" approach where you skip over
  # the NAs?
  set.seed(9619)
  mis <- matrix(
    rbinom(prod(dim(PoliticalDemocracy)), 1, .9),
    nrow(PoliticalDemocracy),
    ncol(PoliticalDemocracy)
  )
  pd <- PoliticalDemocracy * mis
  pd[pd == 0] <- NA

  fitm <- asem(
    model,
    pd,
    verbose = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none",
    test = "none",
    nsamp = 2
  )

  expect_equal(fitm@Data@missing, "listwise")
  expect_equal(fitm@Data@nobs[[1]], nrow(pd[complete.cases(pd), ]))
})

test_that("Approximate constraints", {
  testthat::skip() # FIXME

  # ECM: Approximate constraints could be interesting
  HS.model <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  expect_error({
    fit3 <- acfa(
      HS.model,
      data = lavaan::HolzingerSwineford1939,
      group = "school",
      group.equal = c("intercepts", "loadings"),
      wiggle = "intercepts",
      wiggle.sd = .1,
      verbose = FALSE
    )
  })
})

test_that("4-group model with a bunch of constraints and predict", {
  testthat::skip() # FIXME

  data(StereotypeThreat, package = "psychotools")
  StereotypeThreat <- transform(
    StereotypeThreat,
    group = interaction(ethnicity, condition)
  )
  StereotypeThreat <- StereotypeThreat[order(StereotypeThreat$group), ]

  mod <- "
    f1 =~ abstract + verbal + c(l1, l1, l1, l4)*numerical

    f1 ~  c(maj, min1, maj, min2)*1 + c(NA, 0, NA, 0)*1
    abstract ~ c(ar1, ar2, ar3, ar3)*1
    numerical ~ c(na1,na1,na1,na4)*1

    numerical ~~ c(e1, e1, e1, e4)*numerical
    f1 ~~ c(v1.maj, v1.min, v1.maj, v1.min)*f1
  "

  fit <- acfa(
    mod,
    StereotypeThreat,
    group = "group",
    group.equal = c("loadings", "residuals", "intercepts"),
    verbose = FALSE,
    # marginal_method = "marggaus",
    marginal_correction = "none",
    test = "none",
    nsamp = 2
  )
  prd <- predict(fit, nsamp = 3)
  expect_no_error(out <- capture.output(print(prd)))

  # lavPredict(fit) # ECM: I believe these are coming from lavaan
})


test_that("Constraint on correlations", {
  # ECM: This is a place where I think blavaan will constrain correlations
  # instead of covariances. #  that might not be the right thing to do, but
  # equality constraints on covariances are already weird.

  HS.modelc <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9

    x1 ~~ c(a, b)*x4
    x2 ~~ c(a, b)*x5
  "

  fit <- acfa(
    HS.modelc,
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    dp = blavaan::dpriors(lambda = "normal(1,.5)"),
    verbose = FALSE,
    marginal_method = "marggaus",
    marginal_correction = "none",
    test = "none",
    nsamp = 2
  )

  expect_no_error(out <- capture.output(summary(fit)))
  expect_s4_class(fit, "INLAvaan")

  # Check it runs more than 1 group
  expect_true(length(coef(fit)) > 30)
  expect_equal(fit@Data@ngroups, 2L)
})

test_that("Standardised LVs with equality constraints", {
  # ECM: MCMC can choke on sign indeterminacy
  mod <- "
    visual  =~ c(l1,l1)*x1 + c(l2,l2)*x2 + c(l3,l3)*x3
    textual =~ c(l4,l4)*x4 + c(l5,l5)*x5 + c(l6,l6)*x6
    visual ~~ c(1, NA)*visual
    textual ~~ c(1, NA)*textual
  "

  fit <- acfa(
    mod,
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    std.lv = TRUE,
    meanstructure = TRUE,
    verbose = FALSE,
    # marginal_method = "marggaus",
    marginal_correction = "none",
    test = "none",
    nsamp = 2
  )

  expect_no_error(out <- capture.output(summary(fit)))
  expect_s4_class(fit, "INLAvaan")
})

test_that("Growth models", {
  # ECM: Also see the examples at https://osf.io/4bpmq
  data("Demo.growth", package = "lavaan")
  colnames(Demo.growth)[1:4] <- c("X1", "X2", "X3", "X4")

  # Latent Change Score (LCS) specification with constant change
  constantX.syntax <- "
    # Latent variables
    lX1 =~ 1*X1
    lX2 =~ 1*X2
    lX3 =~ 1*X3
    lX4 =~ 1*X4

    # Autoregressions
    lX2 ~ 1*lX1
    lX3 ~ 1*lX2
    lX4 ~ 1*lX3

    # Change (constant)
    dX1 =~ 1*lX2
    dX2 =~ 1*lX3
    dX3 =~ 1*lX4

    # Random intercept and slope
    intX   =~ 1*lX1
    slopeX =~ 1*dX1 + 1*dX2 + 1*dX3

    # Residuals equal
    X1 ~~ rsdX*X1
    X2 ~~ rsdX*X2
    X3 ~~ rsdX*X3
    X4 ~~ rsdX*X4

    # Manifest means fixed to 0
    X1 ~ 0*1
    X2 ~ 0*1
    X3 ~ 0*1
    X4 ~ 0*1

    # Auto proportions
    dX1 ~ 0*lX1
    dX2 ~ 0*lX2
    dX3 ~ 0*lX3

    # Estimate slope and intercept means
    slopeX ~ 1
    intX ~ 1

    # Estimate latent variances and covariance
    slopeX ~~ slopeX
    intX ~~ intX
    slopeX ~~ intX

    # Means and variances of latent variables fixed to 0
    lX1 ~ 0*1
    lX2 ~ 0*1
    lX3 ~ 0*1
    lX4 ~ 0*1

    dX1 ~ 0*1
    dX2 ~ 0*1
    dX3 ~ 0*1

    lX1 ~~ 0*lX1
    lX2 ~~ 0*lX2
    lX3 ~~ 0*lX3
    lX4 ~~ 0*lX4

    dX1 ~~ 0*dX1
    dX2 ~~ 0*dX2
    dX3 ~~ 0*dX3
  "
  fit <- inlavaan(constantX.syntax, Demo.growth, verbose = FALSE)

  expect_no_error(out <- capture.output(summary(fit)))
  expect_s4_class(fit, "INLAvaan")
})

test_that("Non-recursive (cyclic) model", {
  set.seed(1234)
  pop_model <- "
    dv1 ~ 0.3*dv3
    dv2 ~ 0.5*dv1
    dv3 ~ 0.7*dv2
  "
  dat <- lavaan::simulateData(pop_model, sample.nobs = 500)
  fit <- asem(
    model = "dv1 ~ dv3; dv2 ~ dv1; dv3 ~ dv2",
    data = dat,
    dp = blavaan::dpriors(beta = "normal(.5,.5)"),
    verbose = FALSE
  )

  expect_no_error(out <- capture.output(summary(fit)))
  expect_s4_class(fit, "INLAvaan")
})
