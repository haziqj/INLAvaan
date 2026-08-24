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
      marginal_correction = "none",
      vb_correction = FALSE,
      test = "none",
      verbose = FALSE,
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

test_that("Method: asymgaus", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "asymgaus",
      marginal_correction = "none",
      vb_correction = FALSE,
      test = "none",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: marggaus", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "marggaus",
      vb_correction = FALSE,
      test = "none",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("Method: sampling", {
  expect_no_error({
    fit <- asem(
      mod,
      dat,
      marginal_method = "sampling",
      vb_correction = FALSE,
      test = "none",
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  expect_s4_class(fit, "INLAvaan")
})

test_that("cov_as_cor reports correlations without changing the fit", {
  fit_off <- asem(
    mod,
    dat,
    debug = TRUE,
    test = "none",
    verbose = FALSE,
    nsamp = NSAMP,
    cov_as_cor = FALSE
  )
  fit_on <- asem(
    mod,
    dat,
    debug = TRUE,
    test = "none",
    verbose = FALSE,
    nsamp = NSAMP,
    cov_as_cor = TRUE
  )

  # Estimation is unaffected: same posterior mode and Hessian either way.
  expect_equal(fit_off$theta_star, fit_on$theta_star)
  expect_equal(fit_off$Sigma_theta, fit_on$Sigma_theta)

  covpars <- grep("~~", rownames(fit_off$summary), value = TRUE)
  covpars <- covpars[
    vapply(
      strsplit(covpars, "~~", fixed = TRUE),
      function(p) p[1] != p[2],
      logical(1)
    )
  ]
  expect_true(length(covpars) > 0)

  # mat relabelled to _cor, reporting scale switches to (-1, 1); everything
  # else in the summary is untouched.
  pt_on <- fit_on$partable
  expect_true(all(pt_on$mat[match(covpars, pt_on$names)] == "theta_cor"))
  expect_true(all(abs(fit_on$summary[covpars, "Mean"]) <= 1))

  noncov <- setdiff(rownames(fit_off$summary), covpars)
  expect_equal(
    fit_off$summary[noncov, "Mean"],
    fit_on$summary[noncov, "Mean"]
  )
})

test_that("Gradients are correct (Finite Difference Check)", {
  # Analytic-vs-finite-difference agreement is sensitive to BLAS/compiler
  # differences across CRAN check flavours -- too fragile to assert there.
  skip_on_cran()
  suppressMessages(
    tmp <- capture.output(fit <- asem(mod, dat, test = "none", debug = TRUE))
  )
  test_df <- read.table(text = tmp, skip = 1)[, -1]
  colnames(test_df) <- c("fd", "analytic", "diff")

  expect_equal(
    as.numeric(test_df$fd),
    as.numeric(test_df$diff),
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(test_df$diff),
    rep(0, nrow(test_df)),
    tolerance = 1e-3
  )
})
