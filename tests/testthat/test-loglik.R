# The closed-form correction inlav_model_loglik() adds to lavaan's profiled
# loglik when the model has no mean structure (saturated means with flat
# priors, marginalised analytically), summed over groups
marg_corr <- function(fit) {
  imp <- lavaan::lav_model_implied(fit@Model)
  nobs <- unlist(fit@SampleStats@nobs)
  sum(vapply(seq_along(nobs), function(g) {
    S <- imp$cov[[g]]
    0.5 * as.numeric(determinant(S, logarithm = TRUE)$modulus) +
      0.5 * ncol(S) * log(2 * pi / nobs[g])
  }, numeric(1)))
}

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

  # no mean structure: lavaan profiles the saturated means, INLAvaan
  # marginalises them under flat priors -- the closed-form correction
  # separates the two
  target <- as.numeric(lavaan::logLik(fit)) + marg_corr(fit)
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
  target <- as.numeric(lavaan::logLik(fit)) + marg_corr(fit)
  output <- inlav_model_loglik(
    coef(fit),
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options
  )
  expect_equal(output, target, tolerance = 1e-5)

  # FIML
  suppressWarnings(fit <- lavaan::sem(mod, dat, missing = "ML"))
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

# Under fixed.x the evidence is that of y given x, so a covariate with a zero
# effect adds nothing to the marginal likelihood or the DIC
fixedx_evidence <- function(model, data, ...) {
  int <- get_inlavaan_internal(asem(
    model,
    data,
    nsamp = 3,
    test = "dic",
    verbose = FALSE,
    ...
  ))
  c(mloglik = int$mloglik, Dhat = int$DIC$Dhat)
}

test_that("A zero-effect covariate leaves the evidence unchanged", {
  dat <- na.omit(lavaan::HolzingerSwineford1939)
  expect_equal(
    fixedx_evidence("visual =~ x1 + x2 + x3; visual ~ ageyr + 0*grade", dat),
    fixedx_evidence("visual =~ x1 + x2 + x3; visual ~ ageyr", dat),
    tolerance = 1e-6
  )
})

test_that("A zero-effect covariate at both levels leaves the evidence unchanged", {
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster <= 30, ]
  without_x <- "
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
  "
  with_x <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ 0*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ 0*x1
  "
  expect_equal(
    fixedx_evidence(with_x, dat, cluster = "cluster"),
    fixedx_evidence(without_x, dat, cluster = "cluster"),
    tolerance = 1e-6
  )
})
