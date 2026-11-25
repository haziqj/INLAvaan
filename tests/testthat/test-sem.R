mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
"
dat <- lavaan::PoliticalDemocracy
fit_lav <- lavaan::cfa(mod, dat)
NSAMP <- 3

test_that("Method: skewnorm", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "skewnorm",
      vb_correction = FALSE,
      test = "none",
      silent = TRUE,
      nsamp = NSAMP
    )
  })

  # Summary
  expect_no_error(
    out <- capture.output(summary(fit, postmedian = TRUE, postmode = TRUE))
  )
  expect_no_error(out <- capture.output(summary(fit, rsquare = TRUE)))
  expect_no_error({
    tmp <- get_inlavaan_internal(fit)
    out <- capture.output(print(tmp))
    out <- capture.output(print(summary(tmp)))
  })

  expect_s4_class(fit, "INLAvaan")
  # Convergence (dx ~ 0) depends on the optimiser path, which varies with the
  # platform's BLAS/compiler -- too fragile to assert on CRAN's check farm.
  skip_on_cran()
  expect_equal(fit@optim$dx, rep(0, length(coef(fit))), tolerance = 1e-3)
})

test_that("Method: gaussian", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "gaussian",
      vb_correction = FALSE,
      test = "none",
      silent = TRUE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: sampling is not available", {
  expect_error(
    asem(mod, dat, marginal_method = "sampling", silent = TRUE),
    class = "inlavaan_unavailable"
  )
})

test_that("cov_as_cor reports correlations without changing the fit", {
  skip_on_cran()
  fit_off <- asem(
    mod,
    dat,
    control = inlavaan_control(debug = TRUE),
    test = "none",
    silent = TRUE,
    nsamp = NSAMP,
    cov_as_cor = FALSE
  )
  fit_on <- asem(
    mod,
    dat,
    control = inlavaan_control(debug = TRUE),
    test = "none",
    silent = TRUE,
    nsamp = NSAMP,
    cov_as_cor = TRUE
  )

  # Estimation is unaffected: same posterior mode and Hessian either way.
  expect_equal(fit_off$theta_star, fit_on$theta_star)
  expect_equal(fit_off$Omega, fit_on$Omega)

  covpars <- grep("~~", rownames(fit_off$summary), value = TRUE)
  covpars <- covpars[
    vapply(
      strsplit(covpars, "~~", fixed = TRUE),
      function(p) p[1] != p[2],
      logical(1)
    )
  ]
  expect_true(length(covpars) > 0)

  # The classes stay covariances (methods after the fit read them) and the
  # setting is stored. The reporting scale switches to (-1, 1), and everything
  # else in the summary is untouched.
  pt_on <- fit_on$partable
  expect_true(all(pt_on$mat[match(covpars, pt_on$names)] == "theta_cov"))
  expect_true(fit_on$cov_as_cor)
  expect_true(all(abs(fit_on$summary[covpars, "Mean"]) <= 1))

  noncov <- setdiff(rownames(fit_off$summary), covpars)
  expect_equal(
    fit_off$summary[noncov, "Mean"],
    fit_on$summary[noncov, "Mean"]
  )
})

test_that("Gradients are correct (Finite Difference Check)", {
  # The engine compares the analytic gradient at the mode with central
  # differences of the log-posterior, and grad_gap is the gap in posterior
  # SDs.
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  fit <- asem(mod, dat, test = "none", silent = TRUE, nsamp = 3)
  gap <- get_inlavaan_internal(fit)$engine$diagnostics$marginals$grad_gap
  expect_true(all(is.finite(gap)))
  expect_lt(max(gap), 0.1)
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
    "dv1 ~ dv3; dv2 ~ dv1; dv3 ~ dv2",
    data = dat,
    dp = priors_for(beta = "normal(.5,.5)"),
    test = "none",
    nsamp = 3,
    silent = TRUE
  )
  expect_s4_class(fit, "INLAvaan")
  expect_no_error(capture.output(summary(fit)))
  beta <- coef(fit)[c("dv1~dv3", "dv2~dv1", "dv3~dv2")]
  expect_true(all(abs(beta - c(0.3, 0.5, 0.7)) < 0.25))
})
