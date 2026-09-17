test_that("saturated-mean intercepts get exactly zero VB shift", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  invisible(capture.output(suppressMessages(
    fit <- inlavaan(
      mod,
      data = lavaan::HolzingerSwineford1939,
      model.type = "cfa",
      meanstructure = TRUE,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = 3,
      test = "none",
      debug = TRUE
    )
  )))

  pt <- fit$partable
  nu_id <- pt$free[pt$mat == "nu" & pt$free > 0]

  # The fast path must have recognised this mean structure for the shift to be
  # pinned; if it ever stops doing so, this expectation is the tripwire.
  expect_gt(length(nu_id), 0)
  expect_equal(fit$vb$correction[nu_id], rep(0, length(nu_id)))
  expect_true(any(abs(fit$vb$correction[-nu_id]) > 0))
})

test_that("diagnostics() reports the VB shift's own quadrature error", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  invisible(capture.output(suppressMessages(
    fit <- inlavaan(
      mod,
      data = lavaan::HolzingerSwineford1939,
      model.type = "cfa",
      meanstructure = TRUE,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )))

  glob <- diagnostics(fit, type = "global")
  par <- diagnostics(fit, type = "param")

  expect_true(all(c("vb_mcse_max", "vb_mcse_mean") %in% names(glob)))
  expect_true("vb_mcse_sigma" %in% names(par))
  expect_true(all(is.finite(par$vb_mcse_sigma)))
  expect_true(all(par$vb_mcse_sigma >= 0))
  expect_equal(glob[["vb_mcse_max"]], max(par$vb_mcse_sigma))
  expect_equal(glob[["vb_mcse_mean"]], mean(par$vb_mcse_sigma))
  expect_equal(glob[["vb_shift_max"]], max(abs(par$vb_shift_sigma)))

  # Pinned coordinates use no quadrature, so they carry no quadrature error.
  int <- get_inlavaan_internal(fit)
  nu_id <- int$partable$free[int$partable$mat == "nu" & int$partable$free > 0]
  expect_equal(par$vb_mcse_sigma[nu_id], rep(0, length(nu_id)))
})

test_that("no VB correction means no quadrature error to report", {
  invisible(capture.output(suppressMessages(
    fit <- inlavaan(
      "visual =~ x1 + x2 + x3",
      data = lavaan::HolzingerSwineford1939,
      model.type = "cfa",
      vb_correction = FALSE,
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )))

  expect_true(is.na(diagnostics(fit, type = "global")[["vb_shift_max"]]))
  expect_true(is.na(diagnostics(fit, type = "global")[["vb_mcse_max"]]))
  expect_true(all(is.na(diagnostics(fit, type = "param")$vb_mcse_sigma)))
})

test_that("n_qmc sets the VB node count and is validated", {
  mod <- "visual =~ x1 + x2 + x3"
  fit_at <- function(n) {
    invisible(capture.output(suppressMessages(
      f <- inlavaan(
        mod,
        data = lavaan::HolzingerSwineford1939,
        model.type = "cfa",
        n_qmc = n,
        marginal_method = "marggaus",
        verbose = FALSE,
        nsamp = 3,
        test = "none",
        debug = TRUE
      )
    )))
    f
  }

  expect_equal(fit_at(128L)$vb$n_qmc, 128L)
  expect_equal(fit_at(40L)$vb$n_qmc, 40L)

  # More nodes must not make the quadrature error worse.
  se <- sqrt(diag(fit_at(40L)$Sigma_theta))
  expect_lt(
    mean(fit_at(128L)$vb$mcse / se),
    mean(fit_at(40L)$vb$mcse / se)
  )

  expect_error(fit_at(1L), "at least 2")
  expect_error(fit_at(c(10L, 20L)), "single integer")
})

test_that("the Gauss-Hermite node set matches Gaussian moments", {
  Sigma <- matrix(c(2, 0.6, 0.3, 0.6, 1, 0.2, 0.3, 0.2, 0.5), 3)
  rule <- vb_nodes_gauss_hermite(Sigma)
  X <- rule$nodes
  w <- rule$weights

  expect_equal(nrow(X), 2 * ncol(Sigma) + 1)
  expect_equal(sum(w), 1)
  expect_equal(colSums(w * X), rep(0, 3))
  expect_equal(crossprod(X, w * X), Sigma)
  expect_equal(sum(w * X[, 1] * X[, 2] * X[, 3]), 0)
  expect_equal(sum(w * X[, 1]^2 * X[, 2]), 0)

  # Along a principal axis the three-point rule also gets the fourth moment.
  axis_rule <- vb_nodes_gauss_hermite(diag(c(4, 2, 1)))
  expect_equal(sum(axis_rule$weights * axis_rule$nodes[, 1]^4), 3 * 4^2)
})

test_that("vb_method = 'gauss_hermite' is deterministic and agrees with Sobol", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  fit_vb <- function(...) {
    invisible(capture.output(suppressMessages(
      f <- inlavaan(
        mod,
        data = lavaan::HolzingerSwineford1939,
        model.type = "cfa",
        marginal_method = "marggaus",
        verbose = FALSE,
        nsamp = 3,
        test = "none",
        debug = TRUE,
        ...
      )
    )))
    f
  }
  gh <- fit_vb(vb_method = "gauss_hermite")
  gh_again <- fit_vb(vb_method = "gauss_hermite")
  qmc <- fit_vb(n_qmc = 128L)
  se <- sqrt(diag(gh$Sigma_theta))

  expect_identical(gh$vb$method, "gauss_hermite")
  expect_identical(qmc$vb$method, "sobol")
  expect_true(all(is.finite(gh$vb$correction)))
  expect_false(isTRUE(gh$vb$opt$fallback))
  expect_identical(gh$vb$correction, gh_again$vb$correction)
  expect_true(all(is.na(gh$vb$mcse)))
  expect_lt(max(abs(gh$vb$correction - qmc$vb$correction) / se), 0.1)

  expect_error(fit_vb(vb_method = "trapezoid"), "should be one of")
})

test_that("the Gauss-Hermite rule keeps the fast path and reports no error", {
  mod <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  invisible(capture.output(suppressMessages(
    fit <- acfa(
      mod,
      lavaan::HolzingerSwineford1939,
      meanstructure = TRUE,
      vb_method = "gauss_hermite",
      marginal_method = "marggaus",
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    )
  )))
  int <- get_inlavaan_internal(fit)
  pt <- int$partable
  nu_id <- pt$free[pt$mat == "nu" & pt$free > 0]

  expect_gt(length(nu_id), 0)
  expect_equal(int$vb$correction[nu_id], rep(0, length(nu_id)))
  expect_true(any(abs(int$vb$correction[-nu_id]) > 0))

  glob <- diagnostics(fit)
  expect_equal(glob[["vb_applied"]], 1)
  expect_true(is.na(glob[["vb_mcse_max"]]))
  expect_true(all(is.na(diagnostics(fit, type = "param")$vb_mcse_sigma)))
})
