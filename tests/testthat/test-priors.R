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

  res <- prior_logdens_vectorized(c(2, 4, 1, 1), prepare_priors_for_optim(short_pt), debug = TRUE)

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

test_that("inline prior syntax is extracted before lavaan parsing", {
  mod <- "
    visual =~ x1 + prior('normal(1,2)')*x2 + x3
    textual =~ label*prior(\"normal(3,1.5)\")*x6
    x1 ~~ prior('gamma(3,3)')*x1
    x2 ~ prior('normal(0,.5)')*1
  "

  parsed <- INLAvaan:::extract_inline_priors(mod)
  expect_false(grepl("prior\\s*\\(", parsed$model))
  expect_equal(
    parsed$priors[, c("lhs", "op", "rhs", "prior")],
    data.frame(
      lhs = c("visual", "textual", "x1", "x2"),
      op = c("=~", "=~", "~~", "~1"),
      rhs = c("x2", "x6", "x1", ""),
      prior = c("normal(1,2)", "normal(3,1.5)", "gamma(3,3)", "normal(0,.5)")
    )
  )
})

test_that("political democracy example supports inline priors with GCP", {
  dat <- lavaan::PoliticalDemocracy
  mod <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3
    dem65 =~ y5 + y6 + y7 + y8

    dem60 ~ ind60
    dem65 ~ ind60 + dem60

    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

    dem60 =~ 1.5*y4

    ind60 ~~ prior('gamma(1, 1)')*ind60
    dem60 ~~ prior('gamma(2, 1)')*dem60
    dem65 ~~ prior('gamma(1,.5)')*dem65
  "

  fit <- expect_no_error(
    asem(
      model = mod,
      data = dat,
      use_gcp = TRUE,
      verbose = FALSE,
      test = "none",
      nsamp = 30
    )
  )

  int <- get_inlavaan_internal(fit)
  pt <- int$partable
  expect_equal(pt$prior[pt$names == "ind60~~ind60"], "gamma(1, 1)")
  expect_equal(pt$prior[pt$names == "dem60~~dem60"], "gamma(2, 1)")
  expect_equal(pt$prior[pt$names == "dem65~~dem65"], "gamma(1,.5)")
  expect_false(anyNA(int$theta_star))
  expect_true(all(is.finite(int$theta_star)))
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
  new_dens <- prior_logdens_vectorized(obj$theta, obj$cache)
  expect_true(is.finite(new_dens), info = "Log-density should be finite")

  # --- GRADIENT TEST (against numerical finite differences) ---
  new_grad <- prior_grad_vectorized(obj$theta, obj$cache)
  h <- 1e-5
  num_grad <- sapply(seq_along(obj$theta), function(i) {
    tp <- obj$theta; tp[i] <- tp[i] + h
    tm <- obj$theta; tm[i] <- tm[i] - h
    (prior_logdens_vectorized(tp, obj$cache) - prior_logdens_vectorized(tm, obj$cache)) / (2 * h)
  })
  # Accumulate cache-indexed grad back to full theta-length vector
  full_grad <- numeric(length(obj$theta))
  full_grad[obj$cache$free_id] <- new_grad

  expect_equal(
    full_grad,
    num_grad,
    tolerance = 1e-4,
    info = "Gradient should match finite differences in single-group models"
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
  new_dens <- prior_logdens_vectorized(obj$theta, obj$cache)
  expect_true(is.finite(new_dens), info = "Log-density should be finite")

  # --- GRADIENT TEST (against numerical finite differences) ---
  new_grad <- prior_grad_vectorized(obj$theta, obj$cache)
  h <- 1e-5
  num_grad <- sapply(seq_along(obj$theta), function(i) {
    tp <- obj$theta; tp[i] <- tp[i] + h
    tm <- obj$theta; tm[i] <- tm[i] - h
    (prior_logdens_vectorized(tp, obj$cache) - prior_logdens_vectorized(tm, obj$cache)) / (2 * h)
  })
  # Accumulate cache-indexed grad back to full theta-length vector
  full_grad <- numeric(length(obj$theta))
  full_grad[obj$cache$free_id] <- new_grad

  expect_equal(
    full_grad,
    num_grad,
    tolerance = 1e-4,
    info = "Gradient should match finite differences even with equality constraints"
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

test_that("Beta prior gradient with tanh transform matches numerical", {
  # Regression test: non-trivial Beta(5,5) on correlation with tanh transform
  # Previously the gradient was off by a factor of 2
  cache <- list(
    free_id = 1L,
    trans_type = 2L,   # tanh (correlation)
    prior_type = 3L,   # beta
    p1 = 5,            # beta(5,5)
    p2 = 5,
    is_sd_prior = FALSE,
    prior_names = "rho"
  )

  h <- 1e-7
  for (th in c(-0.8, -0.3, 0.0, 0.3, 0.8)) {
    ag <- as.numeric(prior_grad_vectorized(th, cache))
    ng <- (prior_logdens_vectorized(th + h, cache) -
           prior_logdens_vectorized(th - h, cache)) / (2 * h)
    expect_equal(ag, ng, tolerance = 1e-5,
      info = paste0("Beta(5,5) tanh gradient at theta=", th))
  }

  # Also test asymmetric Beta(2,8)
  cache$p1 <- 2
  cache$p2 <- 8
  for (th in c(-0.5, 0.0, 0.5)) {
    ag <- as.numeric(prior_grad_vectorized(th, cache))
    ng <- (prior_logdens_vectorized(th + h, cache) -
           prior_logdens_vectorized(th - h, cache)) / (2 * h)
    expect_equal(ag, ng, tolerance = 1e-5,
      info = paste0("Beta(2,8) tanh gradient at theta=", th))
  }
})

test_that("dbeta_box with log = TRUE returns log-density", {
  x <- seq(0, 100, length.out = 20)
  ld  <- dbeta_box(x, shape1 = 2, shape2 = 5, a = 0, b = 100, log = TRUE)
  nld <- dbeta_box(x, shape1 = 2, shape2 = 5, a = 0, b = 100, log = FALSE)
  expect_equal(ld, log(nld))
  expect_true(all(is.finite(ld[x > 0 & x < 100])))
})

test_that("dbeta_box stops when b <= a", {
  expect_error(dbeta_box(0.5, shape1 = 1, shape2 = 1, a = 1, b = 0),
               "Require finite scalars with b > a")
  expect_error(dbeta_box(0.5, shape1 = 1, shape2 = 1, a = 0, b = 0),
               "Require finite scalars with b > a")
})
