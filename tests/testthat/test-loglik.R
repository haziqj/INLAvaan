# The closed-form correction inlav_model_loglik() adds to lavaan's profiled
# loglik when the model has no mean structure (saturated means with flat
# priors, marginalised analytically), summed over groups
marg_corr <- function(fit) {
  imp <- lavaan::lav_model_implied(fit@Model)
  nobs <- unlist(fit@SampleStats@nobs)
  sum(vapply(
    seq_along(nobs),
    function(g) {
      S <- imp$cov[[g]]
      0.5 *
        as.numeric(determinant(S, logarithm = TRUE)$modulus) +
        0.5 * ncol(S) * log(2 * pi / nobs[g])
    },
    numeric(1)
  ))
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

test_that("Random slopes: value, dependence on x, FIML and gradient", {
  d_rs <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
  mod_rs <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
  "
  # The MLE has a couple of small negative variances on this subset; lavaan
  # says so and it does not affect what is checked here.
  suppressWarnings(
    fit <- lavaan::sem(mod_rs, d_rs, cluster = "cluster")
  )
  x <- coef(fit)
  ll_at <- function(z) {
    inlav_model_loglik(
      z,
      fit@Model,
      fit@SampleStats,
      fit@Data,
      fit@Options,
      fit@Cache
    )
  }

  target <- as.numeric(lavaan::fitMeasures(fit, "logl"))
  expect_equal(ll_at(x), target, tolerance = 1e-6)

  # The random-slope kernel reads the updated model, so the log-likelihood
  # moves with the parameters instead of being constant in `x`
  x_pert <- x + 0.05
  expect_false(isTRUE(all.equal(ll_at(x_pert), ll_at(x))))
  expect_true(ll_at(x) > ll_at(x_pert))

  # Analytic gradient against central finite differences, away from the mode
  # where the gradient is not numerically zero
  grad <- inlav_model_grad(
    x_pert,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Cache
  )
  h <- 1e-5
  fd <- vapply(
    seq_along(x_pert),
    function(j) {
      xp <- xm <- x_pert
      xp[j] <- xp[j] + h
      xm[j] <- xm[j] - h
      (ll_at(xp) - ll_at(xm)) / (2 * h)
    },
    numeric(1)
  )
  expect_equal(unname(grad), unname(fd), tolerance = 1e-4)

  # FIML: about 5% missing on the indicators only
  set.seed(9619)
  d_mis <- d_rs
  for (v in c("y1", "y2", "y3")) {
    d_mis[[v]][sample(nrow(d_mis), round(0.05 * nrow(d_mis)))] <- NA
  }
  suppressWarnings(
    fit_mis <- lavaan::sem(mod_rs, d_mis, cluster = "cluster", missing = "ml")
  )
  output <- inlav_model_loglik(
    coef(fit_mis),
    fit_mis@Model,
    fit_mis@SampleStats,
    fit_mis@Data,
    fit_mis@Options,
    fit_mis@Cache
  )
  expect_equal(
    output,
    as.numeric(lavaan::fitMeasures(fit_mis, "logl")),
    tolerance = 1e-6
  )
})

test_that("Passing the lavaan cache leaves non-random-slope values alone", {
  # Regression guard for the two extra arguments the random-slope fix sends
  # to lavaan: `lavmodel_x` in place of `lavmodel`, and `lavcache`.
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  fit <- lavaan::cfa(mod, lavaan::HolzingerSwineford1939)
  expect_equal(
    inlav_model_loglik(
      coef(fit),
      fit@Model,
      fit@SampleStats,
      fit@Data,
      fit@Options,
      fit@Cache
    ),
    as.numeric(lavaan::logLik(fit)) + marg_corr(fit),
    tolerance = 1e-5
  )

  mod_2l <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1 + x2 + x3
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1 + w2
  "
  fit_2l <- lavaan::sem(
    mod_2l,
    lavaan::Demo.twolevel,
    cluster = "cluster"
  )
  target_2l <- as.numeric(lavaan::logLik(fit_2l))
  expect_equal(
    inlav_model_loglik(
      coef(fit_2l),
      fit_2l@Model,
      fit_2l@SampleStats,
      fit_2l@Data,
      fit_2l@Options,
      fit_2l@Cache
    ),
    target_2l,
    tolerance = 1e-5
  )
  # ... and the five-argument call still works
  expect_equal(
    inlav_model_loglik(
      coef(fit_2l),
      fit_2l@Model,
      fit_2l@SampleStats,
      fit_2l@Data,
      fit_2l@Options
    ),
    target_2l,
    tolerance = 1e-5
  )
})
