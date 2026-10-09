dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"

# Fit once, reuse (fast defaults)
fit <- acfa(
  mod,
  dat,
  verbose = FALSE,
  nsamp = 5,
  vb_correction = FALSE,
  test = "none",
  marginal_method = "marggaus"
)

test_that("simulate() returns list of correct length", {
  sims <- simulate(fit, nsim = 3)
  expect_true(is.list(sims))
  expect_length(sims, 3)
})

test_that("simulate() each element is a data frame with truth attr", {
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
  sims <- simulate(fit, nsim = 1)
  expect_equal(nrow(sims[[1]]), nrow(dat))
  expect_equal(ncol(sims[[1]]), 6) # x1..x6
})

test_that("simulate() data has correct dimensions (custom nobs)", {
  sims <- simulate(fit, nsim = 1, sample.nobs = 50)
  expect_equal(nrow(sims[[1]]), 50)
})

test_that("simulate() truth attr names match coef()", {
  sims <- simulate(fit, nsim = 1)
  expect_equal(names(attr(sims[[1]], "truth")), names(coef(fit)))
})

test_that("simulate() with prior = TRUE works", {
  sims <- simulate(fit, nsim = 3, prior = TRUE)
  expect_length(sims, 3)
  for (s in sims) {
    expect_true(is.data.frame(s))
    expect_equal(nrow(s), nrow(dat))
  }
})

test_that("simulate() with seed is reproducible", {
  s1 <- simulate(fit, nsim = 1, seed = 42)
  s2 <- simulate(fit, nsim = 1, seed = 42)
  expect_equal(s1[[1]], s2[[1]])
  expect_equal(attr(s1[[1]], "truth"), attr(s2[[1]], "truth"))
  expect_equal(attr(s1[[1]], "truth_theta"), attr(s2[[1]], "truth_theta"))
})

test_that("simulate() nsim = 1 returns length-1 list", {
  sims <- simulate(fit, nsim = 1)
  expect_length(sims, 1)
})

test_that("simulate() with samp_copula = FALSE works", {
  sims <- simulate(fit, nsim = 2, samp_copula = FALSE)
  expect_length(sims, 2)
  expect_true(is.data.frame(sims[[1]]))
})

# Fixtures for the mean-structure behaviour tests below
vars <- paste0("x", 1:6)
ybar <- colMeans(dat[, vars])
fit_ms <- acfa(
  mod,
  dat,
  meanstructure = FALSE,
  verbose = FALSE,
  nsamp = 20,
  vb_correction = FALSE,
  test = "none",
  marginal_method = "marggaus"
)
fit_true <- acfa(
  mod,
  dat,
  meanstructure = TRUE,
  verbose = FALSE,
  nsamp = 20,
  vb_correction = FALSE,
  test = "none",
  marginal_method = "marggaus"
)

sim_col_means <- function(sims) {
  rowMeans(vapply(sims, function(d) colMeans(d[, vars]), numeric(length(vars))))
}

test_that("simulate() replicates live on the data scale without a mean structure", {
  # Regression test: simulateData() centres columns at zero when there is no
  # mean structure; the saturated means (integrated out under a flat prior)
  # must be added back so replicates share the observed location.
  sims <- simulate(fit_ms, nsim = 40, silent = TRUE)
  expect_lt(max(abs(sim_col_means(sims) - ybar)), 0.5)
})

test_that("simulate() does not double-shift when a mean structure is present", {
  # Guards the !meanstructure gate: with an explicit mean structure the
  # intercepts already place the data on scale, so no extra shift is applied.
  sims <- simulate(fit_true, nsim = 40, silent = TRUE)
  expect_lt(max(abs(sim_col_means(sims) - ybar)), 0.5)
})

test_that("simulate() propagates saturated-mean uncertainty N(ybar, Sigma/n)", {
  # A large sample.nobs shrinks within-replicate sampling noise (~0.02) so the
  # between-replicate spread reflects the mean-uncertainty draw (~0.07); a point
  # ybar shift (no uncertainty term) would leave spread near 0.02.
  set.seed(22)
  sims <- simulate(fit_ms, nsim = 60, sample.nobs = 4000, silent = TRUE)
  x1_means <- vapply(sims, function(d) mean(d$x1), numeric(1))
  expect_gt(sd(x1_means), 0.03)
})

test_that("simulate() prior predictive replicates stay centred at zero", {
  # The mean shift is posterior-only (flat prior on the means is improper);
  # prior = TRUE draws must remain on the latent, zero-centred scale. A wrongly
  # applied shift would land near ybar (>= 2.19); prior sampling noise stays
  # well under 1 (seed pinned for determinism).
  set.seed(99)
  sims <- simulate(fit_ms, nsim = 40, prior = TRUE, silent = TRUE)
  expect_lt(max(abs(sim_col_means(sims))), 1)
})

test_that("simulate() works for multigroup models", {
  fit <- acfa(
    "visual =~ x1 + x2 + x3; textual =~ x4 + x5 + x6",
    lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = "loadings",
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  sims <- simulate(fit, nsim = 2)
  expect_length(sims, 2)
  expect_equal(
    as.numeric(table(sims[[1]]$group)),
    as.numeric(unlist(fit@SampleStats@nobs))
  )
})

test_that("simulate() works for conditional.x fits", {
  fit <- suppressWarnings(asem(
    "visual =~ x1 + x2 + x3; visual ~ ageyr + grade",
    na.omit(lavaan::HolzingerSwineford1939),
    conditional.x = TRUE,
    verbose = FALSE,
    nsamp = 20,
    test = "none"
  ))
  set.seed(1)
  sims <- simulate(fit, nsim = 2, silent = TRUE)
  expect_length(sims, 2)
  expect_setequal(names(sims[[1]]), c("x1", "x2", "x3", "ageyr", "grade"))
})

test_that("simulate() keeps the cluster design of a two-level fit", {
  d <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
  d$cluster <- d$cluster * 10 + 3
  fit <- asem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
    ",
    d,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  )
  expect_no_warning(sims <- simulate(fit, nsim = 2, seed = 1))
  expect_equal(sims[[1]]$cluster, d$cluster)
  # A given sample size still lets lavaan choose the clusters
  sims_n <- simulate(fit, nsim = 1, seed = 1, sample.nobs = 200)
  expect_equal(nrow(sims_n[[1]]), 200L)
})
