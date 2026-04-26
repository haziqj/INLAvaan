test_that("Missing-data vignette examples fit with finite estimates", {
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

  set.seed(221)
  mis <- matrix(rbinom(prod(dim(dat)), 1, 0.99), nrow(dat), ncol(dat))
  datmiss <- dat * mis
  datmiss[datmiss == 0] <- NA

  fit_listwise <- expect_no_error(
    INLAvaan::asem(mod, datmiss, meanstructure = TRUE, verbose = FALSE, test = "none")
  )
  fit_fiml <- expect_no_error(
    INLAvaan::asem(
      mod,
      datmiss,
      missing = "ML",
      meanstructure = TRUE,
      verbose = FALSE,
      test = "none"
    )
  )

  expect_true(all(is.finite(coef(fit_listwise))))
  expect_true(all(is.finite(coef(fit_fiml))))

  int <- INLAvaan:::get_inlavaan_internal(fit_fiml)
  expect_false(is.null(int$native_backend))
})
