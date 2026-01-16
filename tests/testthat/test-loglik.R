test_that("Standard MVN loglik", {
  mod <- "
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
  dat <- lavaan::PoliticalDemocracy
  fit <- lavaan::sem(mod, dat)

  target <- as.numeric(lavaan::logLik(fit))
  output <- inlav_model_loglik(
    coef(fit),
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options
  )
  expect_equal(output, target, tolerance = 1e-5)
})

test_that("Standard multigroup likelihood", {
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  dat <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa(mod, dat, group = "school")

  target <- as.numeric(lavaan::logLik(fit))
  output <- inlav_model_loglik(
    coef(fit),
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options
  )
  expect_equal(output, target, tolerance = 1e-5)
})

test_that("Multilevel no missing", {
  mod <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1 + x2 + x3
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1 + w2
  "
  dat <- lavaan::Demo.twolevel
  fit <- lavaan::sem(mod, dat, cluster = "cluster")

  target <- as.numeric(lavaan::logLik(fit))
  output <- inlav_model_loglik(
    coef(fit),
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options
  )
  expect_equal(output, target, tolerance = 1e-5)
})

test_that("Missing data", {
  mod <- "
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
  set.seed(9619)
  mis <- matrix(
    rbinom(prod(dim(lavaan::PoliticalDemocracy)), 1, .95),
    nrow(lavaan::PoliticalDemocracy),
    ncol(lavaan::PoliticalDemocracy)
  )
  dat <- lavaan::PoliticalDemocracy * mis
  dat[dat == 0] <- NA

  # Complete cases
  suppressWarnings(fit <- lavaan::sem(mod, dat))
  target <- as.numeric(lavaan::logLik(fit))
  output <- inlav_model_loglik(
    coef(fit),
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options
  )
  expect_equal(output, target, tolerance = 1e-5)

  # FIML
  suppressWarnings(fit <- lavaan::sem(mod, PoliticalDemocracy, missing = "ML"))
  target <- as.numeric(lavaan::logLik(fit))
  output <- inlav_model_loglik(
    coef(fit),
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options
  )
  expect_equal(output, target, tolerance = 1e-5)
})
