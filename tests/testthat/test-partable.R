test_that("partable_classify_sem_matrix classifies SEM entries correctly", {
  ov <- c("y1", "y2")

  # Loadings
  expect_equal(
    partable_classify_sem_matrix(
      "eta1",
      "=~",
      "y1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "lambda"
  )

  # Observed residuals: variance vs correlation / covariance
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "~~",
      "y1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "theta_var"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "~~",
      "y2",
      ov.names = ov,
      std.ov = TRUE,
      std.lv = FALSE
    ),
    "theta_cor"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "~~",
      "y2",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "theta_cov"
  )

  # Latent (co)variances
  expect_equal(
    partable_classify_sem_matrix(
      "eta1",
      "~~",
      "eta1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "psi_var"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "eta1",
      "~~",
      "eta2",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = TRUE
    ),
    "psi_cor"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "eta1",
      "~~",
      "eta2",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "psi_cov"
  )

  # Residual–latent covariance
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "~*~",
      "eta1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "delta"
  )

  # Regressions
  expect_equal(
    partable_classify_sem_matrix(
      "eta2",
      "~",
      "eta1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "beta"
  )

  # Intercepts / means
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "~1",
      "",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "nu"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "eta1",
      "~1",
      "",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "alpha"
  )

  # Thresholds
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "|",
      "t1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "tau"
  )

  # Defined parameters, constraints, fixed
  expect_equal(
    partable_classify_sem_matrix(
      "p1",
      ":=",
      "y1 + y2",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "defined"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "==",
      "y2",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "constraint"
  )
  expect_equal(
    partable_classify_sem_matrix(
      "y1",
      "@",
      "1",
      ov.names = ov,
      std.ov = FALSE,
      std.lv = FALSE
    ),
    "fixed"
  )
})

test_that("partable_prior_from_row maps to dp entries correctly", {
  dp <- list(
    nu = "nu_prior",
    alpha = "alpha_prior",
    lambda = "lambda_prior",
    beta = "beta_prior",
    theta = "theta_prior",
    psi = "psi_prior",
    rho = "rho_prior",
    tau = "tau_prior"
  )

  dummy <- list(lhs = "eta1", rhs = "eta2", op = "~~")

  expect_equal(
    partable_prior_from_row("nu", dummy$lhs, dummy$rhs, dummy$op, dp),
    "nu_prior"
  )
  expect_equal(
    partable_prior_from_row("alpha", dummy$lhs, dummy$rhs, dummy$op, dp),
    "alpha_prior"
  )
  expect_equal(
    partable_prior_from_row("lambda", dummy$lhs, dummy$rhs, dummy$op, dp),
    "lambda_prior"
  )
  expect_equal(
    partable_prior_from_row("beta", dummy$lhs, dummy$rhs, dummy$op, dp),
    "beta_prior"
  )

  # theta_*: variance vs correlation
  expect_equal(
    partable_prior_from_row("theta_var", dummy$lhs, dummy$rhs, dummy$op, dp),
    "theta_prior"
  )
  expect_equal(
    partable_prior_from_row("theta_cov", dummy$lhs, dummy$rhs, dummy$op, dp),
    "rho_prior"
  )

  # psi_*: variance vs correlation
  expect_equal(
    partable_prior_from_row("psi_var", dummy$lhs, dummy$rhs, dummy$op, dp),
    "psi_prior"
  )
  expect_equal(
    partable_prior_from_row("psi_cor", dummy$lhs, dummy$rhs, dummy$op, dp),
    "rho_prior"
  )

  expect_equal(
    partable_prior_from_row("tau", dummy$lhs, dummy$rhs, dummy$op, dp),
    "tau_prior"
  )

  expect_true(is.na(
    partable_prior_from_row("unknown", dummy$lhs, dummy$rhs, dummy$op, dp)
  ))
})

test_that("safe_tanh rescales tanh and remains within (-1, 1)", {
  x <- c(-10, -1, 0, 1, 10)
  y <- safe_tanh(x)
  expect_equal(y, (1 - 1e-6) * tanh(x))
  expect_true(all(y > -1 & y < 1))
})

test_that("partable_transform_funcs returns appropriate transforms", {
  # Identity case
  tf_id <- partable_transform_funcs("lambda")
  x <- c(-1, 0, 2)
  expect_equal(tf_id$g(x), x)
  expect_equal(tf_id$ginv(x), x)
  expect_equal(tf_id$g_prime(x), 1)
  expect_equal(tf_id$ginv_prime(x), 1)
  expect_equal(tf_id$ginv_prime2(x), 0)

  # Variance: log / exp mapping
  tf_var <- partable_transform_funcs("theta_var")
  x_var <- c(0.5, 1, 2)
  gx <- tf_var$g(x_var)
  expect_equal(tf_var$ginv(gx), x_var, tolerance = 1e-10)
  expect_equal(tf_var$g_prime(x_var), 1 / x_var)
  expect_equal(tf_var$ginv_prime(gx), exp(gx))
  expect_equal(tf_var$ginv_prime2(gx), exp(gx))

  # Correlation: atanh / tanh mapping
  tf_cor <- partable_transform_funcs("psi_cor")
  r <- c(-0.5, 0, 0.4)
  gr <- tf_cor$g(r)
  expect_equal(tf_cor$ginv(gr), tanh(gr))
  expect_equal(tf_cor$ginv(gr), r, tolerance = 1e-10)
  expect_equal(tf_cor$g_prime(r), 1 / (1 - r^2))
  expect_equal(tf_cor$ginv_prime(gr), 1 - tanh(gr)^2)
})

test_that("inlavaanify_partable integrates with lavaan / blavaan objects", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("blavaan")

  HS.model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  '

  fit <- lavaan::cfa(HS.model, lavaan::HolzingerSwineford1939, std.lv = TRUE)
  pt <- lavaan::parTable(fit)

  res <- inlavaanify_partable(
    pt = pt,
    dp = blavaan::dpriors(),
    lavdata = fit@Data,
    lavoptions = fit@Options
  )

  # Basic structure
  expect_type(res, "list")
  expect_true(all(
    c("lhs", "rhs", "op", "mat", "prior", "g", "ginv") %in% names(res)
  ))

  # Check some classifications exist and look reasonable
  expect_true(any(res$mat == "lambda"))
  expect_true(any(res$mat == "theta_var"))
  expect_true(any(res$mat == "psi_var"))

  # Names should be non-empty and length matches number of rows
  expect_equal(length(res$names), nrow(pt))
  expect_true(all(nzchar(res$names)))
})
