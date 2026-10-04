dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("vcov() returns a matrix for INLAvaan objects", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  vc <- suppressMessages(vcov(fit))
  expect_true(is.matrix(vc))
  expect_equal(nrow(vc), length(coef(fit)))
  expect_equal(ncol(vc), length(coef(fit)))
})

test_that("vcov(type = 'theta') returns Laplace covariance", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  vt <- vcov(fit, type = "theta")
  expect_true(is.matrix(vt))
  expect_true(isSymmetric(vt))
})

test_that("vcov() lines up with coef() under equality constraints", {
  mod3 <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  fits <- list(
    group_equal = acfa(
      mod3,
      dat,
      group = "school",
      group.equal = "loadings",
      verbose = FALSE,
      nsamp = 10,
      test = "none"
    ),
    shared_label = acfa(
      "visual =~ x1 + a * x2 + a * x3\n textual =~ x4 + x5 + x6",
      dat,
      verbose = FALSE,
      nsamp = 10,
      test = "none"
    ),
    explicit_eq = acfa(
      "visual =~ x1 + a * x2 + b * x3\n textual =~ x4 + x5 + x6\n a == b",
      dat,
      verbose = FALSE,
      nsamp = 10,
      test = "none"
    )
  )
  for (fit in fits) {
    vc <- vcov(fit)
    expect_identical(rownames(vc), names(coef(fit)))
    expect_identical(colnames(vc), names(coef(fit)))

    # Rows held equal repeat the packed matrix kept internally
    K <- fit@Model@ceq.simple.K
    int <- get_inlavaan_internal(fit)
    expect_equal(unname(vc), unname(K %*% int$vcov_x %*% t(K)))

    # lavaan reads the same matrix
    lv <- lavaan::lavInspect(fit, "vcov")
    expect_equal(unclass(lv), vc, ignore_attr = TRUE)
    expect_identical(rownames(lv), names(coef(fit)))
  }
})
