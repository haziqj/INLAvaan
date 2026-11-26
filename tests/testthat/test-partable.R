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

test_that("Parameter names follow coef()", {
  dat <- lavaan::HolzingerSwineford1939
  dat$grp <- rep_len(1:12, nrow(dat))
  # Labels ending in g1, groups 10 and above, and a := row
  fit <- suppressWarnings(lavaan::cfa(
    "visual =~ x1 + lag1*x2 + x3\n big1 := 2*lag1",
    dat,
    group = "grp",
    ceq.simple = TRUE,
    do.fit = FALSE
  ))
  pt <- inlavaanify_partable(
    fit@ParTable,
    lavdata = fit@Data,
    lavoptions = fit@Options
  )
  free <- pt$free > 0 & !duplicated(pt$free)
  expect_setequal(pt$names[free], names(lavaan::coef(fit)))
  expect_true(all(c("big1", "visual=~x3.g10", "visual=~x3.g12") %in% pt$names))

  fit <- lavaan::sem(
    "level: 1\n fw =~ y1 + ab1*y2 + y3\n level: 2\n fb =~ y1 + y2 + y3",
    lavaan::Demo.twolevel,
    cluster = "cluster",
    do.fit = FALSE
  )
  pt <- inlavaanify_partable(
    fit@ParTable,
    lavdata = fit@Data,
    lavoptions = fit@Options
  )
  free <- pt$free > 0 & !duplicated(pt$free)
  expect_identical(pt$names[free], names(lavaan::coef(fit)))

  # Named levels suffix with the level name
  fit <- lavaan::sem(
    "level: within\n fw =~ y1 + ab1*y2 + y3
     level: between\n fb =~ y1 + y2 + y3",
    lavaan::Demo.twolevel,
    cluster = "cluster",
    do.fit = FALSE
  )
  pt <- inlavaanify_partable(
    fit@ParTable,
    lavdata = fit@Data,
    lavoptions = fit@Options
  )
  free <- pt$free > 0 & !duplicated(pt$free)
  expect_identical(pt$names[free], names(lavaan::coef(fit)))
  expect_true("fb=~y3.lbetween" %in% pt$names)
  expect_identical(partable_level_labels(pt), c("within", "between"))
})

test_that("A := parameter cannot reuse a parameter label", {
  expect_error(
    acfa(
      "visual =~ x1 + a*x2 + x3\n a := 2",
      lavaan::HolzingerSwineford1939,
      silent = TRUE,
      test = "none"
    ),
    "reuses a parameter label"
  )
})

test_that("Multigroup := parameters can be plotted by name", {
  skip_on_cran()
  fit <- acfa(
    "visual =~ x1 + c(a1, a2)*x2 + x3\n d := a1 - a2",
    lavaan::HolzingerSwineford1939,
    group = "school",
    silent = TRUE,
    nsamp = 10,
    test = "none"
  )
  expect_true("d" %in% rownames(get_inlavaan_internal(fit)$summary))
  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(plot(fit, params = "d"))
})

test_that("Multigroup two-level tables work block by block", {
  dat <- lavaan::Demo.twolevel
  dat$g <- ifelse(dat$cluster %% 2 == 0, "a", "b")
  blk <- "level: 1\n fw =~ y1 + y2 + y3\n level: 2\n fb =~ y1 + y2 + y3\n"
  mod <- paste0("group: a\n", blk, "group: b\n", blk)
  fit0 <- suppressWarnings(lavaan::sem(
    mod,
    dat,
    cluster = "cluster",
    group = "g",
    do.fit = FALSE
  ))
  pt0 <- lavaan::parTable(fit0)
  expect_identical(partable_block_index(pt0), as.integer(pt0$block))
  pt <- inlavaanify_partable(pt0, priors_for(), fit0@Data, fit0@Options)
  free <- pt$free > 0L
  expect_identical(pt$names[free], names(lavaan::coef(fit0)))
  expect_identical(pt$group, pt0$group)
})

test_that("Covariances start from their correlation", {
  # With fixed.x = FALSE lavaan starts x2 ~~ x3 at its sample covariance, 1.78,
  # whose atanh() is NaN
  fit0 <- lavaan::sem(
    "y1 ~ x1 + x2 + x3",
    lavaan::PoliticalDemocracy,
    fixed.x = FALSE,
    do.fit = FALSE
  )
  pt <- inlavaanify_partable(
    fit0@ParTable,
    priors_for(),
    fit0@Data,
    fit0@Options
  )
  i <- which(pt$lhs == "x2" & pt$op == "~~" & pt$rhs == "x3")
  v2 <- which(pt$lhs == "x2" & pt$op == "~~" & pt$rhs == "x2")
  v3 <- which(pt$lhs == "x3" & pt$op == "~~" & pt$rhs == "x3")
  expect_equal(
    tanh(pt$parstart[i]),
    pt$start[i] / sqrt(pt$start[v2] * pt$start[v3])
  )
  expect_true(all(is.finite(pt$parstart[pt$free > 0])))
})

# Mode of a fit, named by parameter and group
named_mode <- function(fit) {
  int <- get_inlavaan_internal(fit)
  pt <- int$partable
  k <- pt$free > 0 & !duplicated(pt$free)
  stats::setNames(int$theta_star, paste(pt$names[k], pt$group[k]))
}

test_that("std.lv with a free second-group variance fits as written out", {
  hs <- lavaan::HolzingerSwineford1939
  mod_std <- "
    visual  =~ c(l1, l1)*x1 + c(l2, l2)*x2 + c(l3, l3)*x3
    textual =~ c(l4, l4)*x4 + c(l5, l5)*x5 + c(l6, l6)*x6
    visual  ~~ c(1, NA)*visual
    textual ~~ c(1, NA)*textual
  "
  mod_hand <- "
    visual  =~ NA*x1 + c(l1, l1)*x1 + c(l2, l2)*x2 + c(l3, l3)*x3
    textual =~ NA*x4 + c(l4, l4)*x4 + c(l5, l5)*x5 + c(l6, l6)*x6
    visual  ~~ c(1, NA)*visual
    textual ~~ c(1, NA)*textual
  "
  fit_with <- function(mod, std_lv) {
    acfa(
      mod,
      hs,
      group = "school",
      std.lv = std_lv,
      test = "none",
      nsamp = 3,
      silent = TRUE
    )
  }
  fit_std <- fit_with(mod_std, TRUE)
  fit_hand <- fit_with(mod_hand, FALSE)

  pt <- get_inlavaan_internal(fit_std)$partable
  is_lv_cov <- pt$op == "~~" & pt$lhs == "visual" & pt$rhs == "textual"
  expect_equal(pt$mat[is_lv_cov], c("psi_cor", "psi_cov"))

  expect_equal(
    get_inlavaan_internal(fit_std)$mloglik,
    get_inlavaan_internal(fit_hand)$mloglik,
    tolerance = 1e-8
  )
  mode_std <- named_mode(fit_std)
  expect_equal(
    mode_std,
    named_mode(fit_hand)[names(mode_std)],
    tolerance = 1e-6
  )
})

test_that("std.ov residual covariances fit as on standardised data", {
  pd <- lavaan::PoliticalDemocracy[, paste0("y", 1:4)]
  mod <- "
    dem60 =~ y1 + y2 + y3 + y4
    y2 ~~ y4
  "
  fit_std <- acfa(
    mod,
    pd,
    std.ov = TRUE,
    test = "none",
    nsamp = 3,
    silent = TRUE
  )
  # lavaan standardises with the n - 1 divisor, as scale() does
  fit_hand <- acfa(
    mod,
    as.data.frame(scale(pd)),
    test = "none",
    nsamp = 3,
    silent = TRUE
  )

  pt <- get_inlavaan_internal(fit_std)$partable
  expect_equal(pt$mat[pt$lhs == "y2" & pt$rhs == "y4"], "theta_cov")
  expect_equal(
    get_inlavaan_internal(fit_std)$mloglik,
    get_inlavaan_internal(fit_hand)$mloglik,
    tolerance = 1e-8
  )
  expect_equal(named_mode(fit_std), named_mode(fit_hand), tolerance = 1e-6)
})

test_that("std.lv with unit latent variances keeps the correlation class", {
  fit <- acfa(
    "visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6",
    lavaan::HolzingerSwineford1939,
    std.lv = TRUE,
    test = "none",
    nsamp = 3,
    silent = TRUE
  )
  pt <- get_inlavaan_internal(fit)$partable
  expect_equal(pt$mat[pt$lhs == "visual" & pt$rhs == "textual"], "psi_cor")
})
