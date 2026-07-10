dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("residuals() returns observed-minus-fitted moments for INLAvaan objects", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  rs <- residuals(fit)
  expect_type(rs, "list")
  expect_equal(rs$type, "raw")
  ov <- lavaan::lavNames(fit, "ov")
  expect_equal(dim(rs$cov), c(length(ov), length(ov)))
  expect_true(isSymmetric(unclass(rs$cov)))
})

test_that("residuals() matches observed minus fitted()", {
  fit <- acfa(mod, dat, meanstructure = TRUE, verbose = FALSE,
              nsamp = 3, test = "none")
  ov <- lavaan::lavNames(fit, "ov")
  obs_cov <- lavaan::lavInspect(fit, "sampstat")$cov
  implied <- fitted(fit)$cov
  expect_equal(unclass(residuals(fit)$cov), unclass(obs_cov) - unclass(implied),
               tolerance = 1e-8)
})

test_that("residuals() supports the lavaan 'type' options", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  # lavaan canonicalises the reported $type; "cor" -> "cor.bollen"
  expected <- c(raw = "raw", cor = "cor.bollen", cor.bollen = "cor.bollen",
                cor.bentler = "cor.bentler", normalized = "normalized",
                standardized = "standardized")
  for (ty in names(expected)) {
    rs <- residuals(fit, type = ty)
    expect_equal(rs$type, unname(expected[ty]))
  }
})

test_that("residuals(type = 'casewise') returns observed-minus-fitted per case", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  cw <- residuals(fit, type = "casewise")
  expect_true(is.matrix(cw))
  expect_equal(nrow(cw), nrow(dat))
  expect_equal(ncol(cw), length(lavaan::lavNames(fit, "ov")))
})

test_that("resid() matches residuals()", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  expect_equal(resid(fit), residuals(fit))
})
