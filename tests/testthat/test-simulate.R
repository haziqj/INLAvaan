dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

test_that("simulate() returns list of correct length", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 3)
  expect_true(is.list(sims))
  expect_length(sims, 3)
})

test_that("simulate() each element is a data frame with truth attr", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 2)
  for (s in sims) {
    expect_true(is.data.frame(s))
    truth <- attr(s, "truth")
    expect_true(is.numeric(truth))
    expect_false(is.null(names(truth)))
    truth_theta <- attr(s, "truth_theta")
    expect_true(is.numeric(truth_theta))
    expect_false(is.null(names(truth_theta)))
    expect_equal(length(truth_theta), length(truth))
  }
})

test_that("simulate() data has correct dimensions (default nobs)", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 1)
  expect_equal(nrow(sims[[1]]), nrow(dat))
  expect_equal(ncol(sims[[1]]), 6)  # x1..x6
})

test_that("simulate() data has correct dimensions (custom nobs)", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 1, sample.nobs = 50)
  expect_equal(nrow(sims[[1]]), 50)
})

test_that("simulate() truth attr names match coef()", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 1)
  expect_equal(names(attr(sims[[1]], "truth")), names(coef(fit)))
})

test_that("simulate() with prior = TRUE works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 3, prior = TRUE)
  expect_length(sims, 3)
  for (s in sims) {
    expect_true(is.data.frame(s))
    expect_equal(nrow(s), nrow(dat))
  }
})

test_that("simulate() with seed is reproducible", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  s1 <- simulate(fit, nsim = 1, seed = 42)
  s2 <- simulate(fit, nsim = 1, seed = 42)
  expect_equal(s1[[1]], s2[[1]])
  expect_equal(attr(s1[[1]], "truth"), attr(s2[[1]], "truth"))
  expect_equal(attr(s1[[1]], "truth_theta"), attr(s2[[1]], "truth_theta"))
})

test_that("simulate() nsim = 1 returns length-1 list", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 1)
  expect_length(sims, 1)
})

test_that("simulate() with samp_copula = FALSE works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 5)
  sims <- simulate(fit, nsim = 2, samp_copula = FALSE)
  expect_length(sims, 2)
  expect_true(is.data.frame(sims[[1]]))
})
