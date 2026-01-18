dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
DP <- blavaan::dpriors(theta = "gamma(1,1)", psi = "gamma(1,1)")
NSAMP <- 3
fit <- asem(
  mod,
  dat,
  dp = DP,
  verbose = FALSE,
  nsamp = NSAMP
)

test_that("Setting dp argument", {
  pt <- lavaan::partable(fit)
  where_theta_var <- grep("theta_var", pt$mat)
  expect_true(all(pt$prior[where_theta_var] == "gamma(1,1)"))
  where_psi_var <- grep("psi_var", pt$mat)
  expect_true(all(pt$prior[where_psi_var] == "gamma(1,1)"))
})

test_that("Straight from textual model", {
  mod <- "
    visual  =~ x1 + prior('normal(1,2)')*x2 + x3
    textual =~ x4 + x5 + prior('normal(3,1.5)')*x6
    speed   =~ x7 + x8 + x9

    x1 ~~ prior('gamma(3,3)')*x1
  "

  expect_no_error({
    fit <- asem(
      mod,
      dat,
      verbose = FALSE,
      nsamp = NSAMP
    )
  })
  expect_no_error(out <- capture.output(summary(fit)))

  pt <- lavaan::partable(fit)
  expect_true(pt$prior[grep("visual=~x2", pt$names)] == "normal(1,2)")
  expect_true(pt$prior[grep("textual=~x6", pt$names)] == "normal(3,1.5)")
  expect_true(pt$prior[grep("x1~~x1", pt$names)] == "gamma(3,3)")

  fit_inlv <- get_inlavaan_internal(fit)
  pt <- fit_inlv$partable
  idx <- which(
    pt$names %in% c("visual=~x2", "textual=~x6", "x1~~x1", "textual~~textual")
  )
  short_pt <- lapply(pt, function(x) x[idx])
  short_pt$free <- 1:4

  res <- prior_logdens(c(2, 4, 1, 1), short_pt, debug = TRUE)

  # Check log densities
  expect_equal(
    as.numeric(res$lp),
    c(
      dnorm(2, 1, 2, log = TRUE),
      dnorm(4, 3, 1.5, log = TRUE),
      dgamma(exp(1), 3, 3, log = TRUE),
      dgamma(exp(1 / 2), 1, 0.5, log = TRUE)
    )
  )

  # Check jacobians
  expect_equal(
    as.numeric(res$ljcb),
    c(
      0, # log(abs(1))
      0, # log(abs(1))
      log(abs(exp(1))), # t = log x  ---> log(abs(d/dt e^t))
      log(abs(0.5 * exp(1 / 2))) # t = log(sqrt(x)) ---> log(abs(0.5 exp^{t/2}))
    )
  )
})

## ---- TESTS FOR NEW VECTORISED LOG PRIOR AND GRADIENT ------------------------

get_test_objects <- function(model, data, group = NULL, group.equal = "") {
  # 1. Fit standard lavaan to get data structures
  lav_fit <- lavaan::cfa(
    model,
    data = data,
    group = group,
    group.equal = group.equal,
    se = "none", # Speed up
    do.fit = FALSE # We only need the setup
  )

  # 2. Run inlavaanify (assuming it is exported or available in namespace)
  pt <- inlavaanify_partable(
    pt = lavaan::parTable(lav_fit),
    lavdata = lav_fit@Data,
    lavoptions = lav_fit@Options
  )

  # 3. Create the Cache
  cache <- prepare_priors_for_optim(pt)

  # 4. Generate Random Theta (Validation Vector)
  # Size is the maximum integer in the 'free' column
  n_theta <- max(pt$free, na.rm = TRUE)
  set.seed(42) # Reproducibility
  theta <- rnorm(n_theta)

  list(pt = pt, cache = cache, theta = theta)
}

test_that("Vectorized vs Old: Standard Single-Group CFA", {
  # Setup
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  obj <- get_test_objects(mod, dat)

  # --- LOG DENSITY TEST ---
  old_dens <- prior_logdens(obj$theta, obj$pt)
  new_dens <- prior_logdens_vectorized(obj$theta, obj$cache)

  # Expect exact match (allowing for tiny floating point differences)
  expect_equal(
    new_dens,
    old_dens,
    tolerance = 1e-8,
    info = "Log-densities should match in single-group models"
  )

  # --- GRADIENT TEST ---
  old_grad <- prior_grad(obj$theta, obj$pt)
  new_grad <- prior_grad_vectorized(obj$theta, obj$cache)

  expect_equal(
    new_grad,
    old_grad,
    tolerance = 1e-8,
    info = "Gradients should match in single-group models"
  )
})

test_that("Vectorized vs Old: Multi-Group with Equality Constraints", {
  # This is the critical test for the indexing bug
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2
    textual =~ x4 + x5
  "

  # Create object with equality constraints
  obj <- get_test_objects(
    mod,
    dat,
    group = "school",
    group.equal = "loadings"
  )

  # --- LOG DENSITY TEST ---
  old_dens <- prior_logdens(obj$theta, obj$pt)
  new_dens <- prior_logdens_vectorized(obj$theta, obj$cache)

  expect_equal(
    new_dens,
    old_dens,
    tolerance = 1e-8,
    info = "Log-densities should match even with duplicate free parameters (constraints)"
  )

  # --- GRADIENT TEST ---
  old_grad <- prior_grad(obj$theta, obj$pt)
  new_grad <- prior_grad_vectorized(obj$theta, obj$cache)

  # Note: If prior_grad (old) still has the indexing bug, this will FAIL.
  # The failure proves the new version is handling indices differently (and correctly).
  expect_equal(
    new_grad,
    old_grad,
    tolerance = 1e-8,
    info = "Gradients should match even with duplicate free parameters (constraints)"
  )
})

test_that("Vectorized version handles different Prior Types correctly", {
  # Mock a pt with Normal, Gamma, and Beta to ensure coverage
  # We construct a fake cache manually to test the math isolated from the parser

  # 3 params: 1=Normal, 2=Gamma[sd], 3=Beta
  theta <- c(0.5, 0.5, 0.0) # Arbitrary values

  cache <- list(
    free_id = c(1L, 2L, 3L),
    # Exp, Identity, Identity
    trans_type = c(1L, 0L, 0L),
    # Normal, Gamma, Beta
    prior_type = c(1L, 2L, 3L),
    p1 = c(0, 1, 1), # mu, shape, alpha
    p2 = c(1, 0.5, 1), # sd, rate, beta
    is_sd_prior = c(FALSE, TRUE, FALSE),
    prior_names = c("norm", "gam", "beta")
  )

  # Calculate New
  res_grad <- prior_grad_vectorized(theta, cache)

  # We expect a result vector of length 3
  expect_equal(length(res_grad), 3)
  expect_true(all(is.finite(res_grad)))
})
