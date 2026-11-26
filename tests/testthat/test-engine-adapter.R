# The adapter between inlavaan() and the engine copies (R/engine-adapter.R).
# capture_engine() fits with the engine and records what inlavaan() gives
# vanilla_core() and what it returns.

capture_engine <- function(expr) {
  seen <- new.env()
  local_mocked_bindings(
    engine_call = function(...) {
      seen$args <- list(...)
      seen$core <- vanilla_core(...)
      seen$core
    }
  )
  seen$fit <- expr
  seen
}

hs <- lavaan::HolzingerSwineford1939
hs_mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
pd_mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  dem60 ~ ind60
  dem65 ~ ind60 + dem60
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
"

# HS with a mean structure, which takes the saturated-means fast path, and
# with its items rescaled to a mean of `centre` and an SD of `spread`
hs_means <- function(
  centre = NULL,
  spread = NULL,
  marginal_method = "gaussian",
  ...
) {
  dat <- hs
  if (!is.null(centre)) {
    items <- paste0("x", 1:9)
    dat[items] <- scale(dat[items]) * spread + centre
  }
  capture_engine(acfa(
    hs_mod,
    dat,
    meanstructure = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = marginal_method,
    ...
  ))
}

## ----- Hooks -----------------------------------------------------------------

test_that("the gradient is NA where the log-posterior fails", {
  ce <- capture_engine(asem(
    pd_mod,
    lavaan::PoliticalDemocracy,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "gaussian"
  ))
  lp <- ce$args$lp
  gr <- ce$args$gr
  mode <- ce$core$mode
  A <- cov_root(ce$core$cov)
  set.seed(1)
  pts <- lapply(seq_len(40), function(i) {
    mode + 8 * drop(A %*% stats::rnorm(length(mode)))
  })
  failed <- vapply(pts, lp, numeric(1)) <= -1e39
  expect_true(any(failed))
  expect_true(any(!failed))
  expect_no_warning(grads <- lapply(pts, gr))
  expect_true(all(vapply(grads[failed], function(g) all(is.na(g)), NA)))
  expect_true(all(vapply(grads[!failed], function(g) all(is.finite(g)), NA)))

  # The gradient at the point of the last log-posterior reuses its value
  x <- pts[[which(!failed)[1]]]
  lp(mode)
  g_new <- gr(x)
  lp(x)
  expect_identical(gr(x), g_new)

  # A failed point is one that the engine's failed_lp() reads as failed: NA,
  # NaN, and -1e39 and below. +Inf is not.
  fns <- engine_fns(function(theta) theta[[1]], function(theta) c(7, 7))
  for (v in c(NA, NaN, -1e39, -1e40, -Inf)) {
    expect_identical(fns$gr(c(v, 0)), c(NA_real_, NA_real_))
    expect_identical(failed_lp(v), -Inf)
  }
  for (v in c(Inf, -1e38, 0)) {
    expect_identical(fns$gr(c(v, 0)), c(7, 7))
    expect_false(identical(failed_lp(v), -Inf))
  }
})

test_that("the Hessian at the mode mirrors the cross block of the fast path", {
  # Raw HS, then T-scores and the IQ scale, where the default prior of the
  # intercepts pulls the fitted means far from the sample means
  scales <- list(raw = NULL, t_score = c(50, 10), iq = c(100, 15))
  for (s in scales) {
    expect_no_warning(
      ce <- suppressMessages(hs_means(s[1], s[2], verbose = FALSE))
    )
    mode <- ce$core$mode
    gr <- ce$args$gr
    fp <- ce$args$gaussian_marginals
    cc <- setdiff(seq_along(mode), fp)
    expect_length(fp, 9L)
    H <- ce$args$hess_mode(mode)
    # Returned as differenced, not symmetrised
    expect_false(isSymmetric(H))
    # The engine's check of a supplied Hessian
    sym_cov <- function(H) solve(-0.5 * (H + t(H)))
    expect_lt(hess_discrepancy(gr, mode, cov_root(sym_cov(H))), 1e-8)
    # The same posterior SDs as differences over every column
    H_all <- fast_jacobian(gr, as.numeric(mode))
    expect_equal(
      sqrt(diag(sym_cov(H))),
      sqrt(diag(sym_cov(H_all))),
      tolerance = 1e-6
    )
    # A zero cross block fails the engine's check off the raw scale
    if (!is.null(s)) {
      H0 <- H
      H0[fp, cc] <- 0
      H0[cc, fp] <- 0
      expect_gt(hess_discrepancy(gr, mode, cov_root(sym_cov(H0))), 1e-3)
    }
  }
})

test_that("the intercept block has no prior precision without priors", {
  ce_on <- hs_means(silent = TRUE)
  ce_off <- hs_means(
    silent = TRUE,
    control = inlavaan_control(add_priors = FALSE)
  )
  theta <- ce_off$core$mode
  fp <- ce_off$args$gaussian_marginals
  H_on <- ce_on$args$hess_mode(theta)
  H_off <- ce_off$args$hess_mode(theta)
  # The default intercept prior is normal(0,32)
  expect_equal(H_off[fp, fp] - H_on[fp, fp], diag(1 / 32^2, length(fp)))
  H_fd <- fast_jacobian(ce_off$args$gr, as.numeric(theta), cols = fp)
  expect_equal(H_off[fp, fp], H_fd[fp, fp], tolerance = 1e-6)
})

test_that("forward differences reach the engine, and a gradient always", {
  local_deprecation_warnings()
  ce_fwd <- capture_engine(acfa(
    hs_mod,
    hs,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian",
    control = inlavaan_control(hessian = "forward")
  ))
  expect_true(is.function(ce_fwd$args$hess_mode))
  mode <- ce_fwd$core$mode
  expect_equal(
    ce_fwd$args$hess_mode(mode),
    fast_jacobian(ce_fwd$args$gr, as.numeric(mode), method = "forward")
  )
  # The deprecated numerical_grad has no effect
  ce_num <- capture_engine(acfa(
    "visual =~ x1 + x2 + x3",
    hs,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian",
    numerical_grad = TRUE
  ))
  expect_true(is.function(ce_num$args$gr))
  expect_true(get_inlavaan_internal(ce_num$fit)$engine$has_grad)
})

test_that("a start where the log-posterior fails stops with INLAvaan's words", {
  expect_error(
    engine_check_start(function(theta) -1e40, c(a = 0)),
    "starting values"
  )
  expect_identical(engine_check_start(function(theta) -2, c(a = 0)), -2)
})

test_that("an engine error carries no call of inla_vanilla()", {
  boom <- function(theta) stop("boom")
  e_core <- tryCatch(
    vanilla_core(boom, init = c(a = 0), silent = TRUE),
    error = identity
  )
  expect_identical(conditionCall(e_core), quote(inla_vanilla()))
  e <- tryCatch(
    engine_call(boom, init = c(a = 0), silent = TRUE),
    error = identity
  )
  expect_s3_class(e, "inlavanilla_error")
  expect_null(conditionCall(e))
  expect_match(conditionMessage(e), "boom")
})

test_that("the fit prints a line per stage, and more with verbose = TRUE", {
  fit_once <- function(...) {
    acfa("visual =~ x1 + x2 + x3", hs, nsamp = 3, test = "none", ...)
  }
  lines <- capture_messages(fit_once())
  expect_true(any(grepl("posterior mode", lines)))
  expect_gt(length(capture_messages(fit_once(verbose = TRUE))), length(lines))
  expect_silent(fit_once(silent = TRUE))
})

## ----- Old values ------------------------------------------------------------

test_that("old values map to the engine's stages", {
  local_deprecation_warnings()
  fit_with <- function(...) {
    capture_engine(acfa(
      "visual =~ x1 + x2 + x3",
      hs,
      silent = TRUE,
      nsamp = 3,
      test = "none",
      vb_correction = FALSE,
      ...
    ))
  }
  ce <- fit_with(marginal_method = "asymgaus", marginal_correction = "none")
  expect_identical(ce$args$marginals, "skewnorm")
  expect_identical(ce$args$marginal_scan, "plain")
  expect_identical(get_inlavaan_internal(ce$fit)$marginal_method, "skewnorm")
  ce <- fit_with(marginal_method = "marggaus", optim_method = "optim")
  expect_identical(ce$args$marginals, "gaussian")
  int <- get_inlavaan_internal(ce$fit)
  expect_identical(int$marginal_method, "gaussian")
  expect_identical(int$optim_method, "nlminb")
  expect_error(
    fit_with(marginal_method = "sampling"),
    class = "inlavaan_unavailable"
  )
  ce <- fit_with(marginal_correction = "hessian", n_qmc = 1L, sn_fit_ngrid = 31)
  expect_identical(ce$args$marginal_scan, "linear")
  expect_identical(ce$args$control$n_grid, 31L)
  expect_identical(ce$args$vb_correction, "none")
  ce <- fit_with(marginal_scan = "exact", marginal_method = "gaussian")
  expect_identical(ce$args$marginal_scan, "exact")
  expect_identical(ce$args$marginals, "gaussian")
})

## ----- Fields ----------------------------------------------------------------

test_that("the fields of the internal list come from the engine", {
  ce <- capture_engine(acfa(
    hs_mod,
    hs,
    silent = TRUE,
    nsamp = 3,
    test = "none"
  ))
  core <- ce$core
  int <- get_inlavaan_internal(ce$fit)
  m <- length(core$mode)
  # theta_star is the VB mean and theta_star_novbc the mode, both unnamed
  expect_identical(int$theta_star, unname(core$mean_vb))
  expect_identical(int$theta_star_novbc, unname(core$mode))
  expect_null(names(int$theta_star))
  expect_identical(int$Omega, core$cov)
  # The engine's evidence already holds the VB term
  expect_identical(int$mloglik, core$logml)
  expect_equal(
    int$mloglik - core$logml_laplace,
    core$diagnostics$vb$kld_global
  )
  expect_identical(int$opt$dx, unname(core$diagnostics$optim$grad))
  expect_identical(int$opt$dx_analytic, int$opt$dx)
  expect_identical(int$opt$iterations, core$opt$iterations)
  expect_identical(int$vb$method, "onestep")
  expect_identical(int$vb$correction, unname(core$mean_vb - core$mode))
  expect_identical(int$vb$kld_global, core$diagnostics$vb$kld_global)
  expect_equal(unname(int$vb$kld), core$diagnostics$marginals$kld)
  expect_identical(int$vb$mcse, rep(NA_real_, m))
  # Rows for the covariances follow the m rows of the marginals
  expect_identical(
    int$approx_data[seq_len(m), ],
    core$sn[, c("xi", "omega", "alpha", "logC", "nmad", "gamma1")]
  )
  expect_identical(int$engine$diagnostics, core$diagnostics)
  expect_identical(int$engine$counts, core$counts)
  lock <- read.dcf(system.file("inlavanilla.lock", package = "INLAvaan"))
  expect_identical(int$engine$release, lock[1L, "Release"][[1L]])
  expect_identical(int$engine$commit, lock[1L, "Commit"][[1L]])
})

test_that("the layout of the internal list is that of the old stages", {
  int <- get_inlavaan_internal(acfa(
    "visual =~ x1 + x2 + x3",
    hs,
    silent = TRUE,
    nsamp = 3,
    test = "none"
  ))
  expect_named(
    int,
    c(
      "coefficients",
      "mloglik",
      "DIC",
      "summary",
      "ppp",
      "loo",
      "waic",
      "test",
      "optim_method",
      "marginal_method",
      "cov_as_cor",
      "samp_copula",
      "samp_norta",
      "theta_star_novbc",
      "theta_star",
      "Omega",
      "R_star",
      "vcov_x",
      "theta_star_trans",
      "approx_data",
      "nsamp",
      "pdf_data",
      "def_undefined",
      "partable",
      "lavmodel",
      "lavsamplestats",
      "lavdata",
      "lavcache",
      "opt",
      "timing",
      "visual_debug",
      "vb",
      "cov_pd",
      "call",
      "version",
      "engine",
      "warnings"
    )
  )
  expect_identical(
    names(int$timing),
    c(
      "init",
      "optim",
      "vb",
      "loglik",
      "marginals",
      "norta",
      "sampling",
      "covariances",
      "definedpars",
      "deltapars",
      "test"
    )
  )
  expect_true(all(unlist(int$timing) >= 0))
  expect_named(
    int$vb,
    c("opt", "n_qmc", "method", "correction", "mcse", "kld", "kld_global")
  )
  expect_identical(
    colnames(int$approx_data),
    c("xi", "omega", "alpha", "logC", "nmad", "gamma1")
  )
  expect_true(all(c("dx", "dx_analytic") %in% names(int$opt)))
  expect_type(int$theta_star_novbc, "double")
  expect_null(names(int$theta_star_novbc))
})

test_that("the evidence holds the constant of a two-level fit under fixed.x", {
  ce <- capture_engine(asem(
    "level: 1\n y1 ~ x1 + x2\n level: 2\n y1 ~ w1 + x2",
    lavaan::Demo.twolevel,
    cluster = "cluster",
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE,
    marginal_method = "gaussian"
  ))
  int <- get_inlavaan_internal(ce$fit)
  adj <- twolevel_fixedx_loglik_adj(
    int$lavmodel,
    pars_to_x(int$theta_star_novbc, int$partable),
    int$lavdata,
    int$lavsamplestats
  )
  expect_gt(abs(adj), 1)
  expect_identical(int$mloglik, ce$core$logml + adj)
})

test_that("with no VB correction the VB fields are NA", {
  ce <- capture_engine(acfa(
    "visual =~ x1 + x2 + x3",
    hs,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = FALSE
  ))
  int <- get_inlavaan_internal(ce$fit)
  expect_identical(ce$args$vb_correction, "none")
  for (field in c("opt", "correction", "mcse", "kld", "kld_global")) {
    expect_identical(int$vb[[field]], NA)
  }
  expect_identical(int$vb$method, NA_character_)
  expect_identical(int$theta_star, int$theta_star_novbc)
  expect_identical(int$mloglik, ce$core$logml_laplace)
  expect_true(all(is.na(int$summary$kld)))
})

test_that("the fast-path rows have no scan", {
  ce <- suppressMessages(hs_means(
    verbose = FALSE,
    marginal_method = "skewnorm"
  ))
  int <- get_inlavaan_internal(ce$fit)
  fp <- ce$args$gaussian_marginals
  ad <- int$approx_data[seq_along(int$theta_star), ]
  expect_true(all(ad[fp, "alpha"] == 0))
  expect_true(all(is.na(ad[fp, c("logC", "nmad", "gamma1")])))
  expect_true(all(is.finite(ad[-fp, c("logC", "nmad", "gamma1")])))
  vd <- int$visual_debug
  expect_named(vd, names(ce$core$mode))
  expect_true(all(vapply(vd[fp], is.null, NA)))
  for (curve in vd[-fp]) {
    expect_named(curve, c("x", "Original", "Corrected", "SN_Fit"))
    expect_identical(max(curve$Corrected), 1)
    expect_named(attr(curve, "sn_params"), c("xi", "omega", "alpha", "logC"))
  }
  # A width the scan could not compute leaves the rest of the curve
  core <- ce$core
  j <- setdiff(seq_along(core$mode), fp)[1L]
  core$scan$width[2L, j] <- NA
  curve <- engine_visual_debug(core)[[j]]
  expect_true(is.na(curve$Corrected[2L]))
  expect_true(all(is.finite(curve$Corrected[-2L])))
  expect_identical(max(curve$Corrected, na.rm = TRUE), 1)
})

## ----- Source ----------------------------------------------------------------

test_that("only the adapter calls vanilla_core()", {
  dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(dir), "needs the source tree")
  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE)
  # An installed copy (as under covr) has no R files in R/
  skip_if(length(files) == 0L, "needs the R files of the source tree")
  files <- files[!startsWith(basename(files), "vanilla-")]
  calls <- vapply(
    files,
    function(f) {
      pd <- utils::getParseData(parse(f, keep.source = TRUE))
      any(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "vanilla_core")
    },
    NA
  )
  expect_identical(unname(basename(files[calls])), "engine-adapter.R")
})

test_that("no INLAvaan file reads the old field Sigma_theta", {
  dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(dir), "needs the source tree")
  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE)
  # An installed copy (as under covr) has no R files in R/
  skip_if(length(files) == 0L, "needs the R files of the source tree")
  files <- files[!startsWith(basename(files), "vanilla-")]
  # A symbol, an argument name or a string, so that $Sigma_theta,
  # [["Sigma_theta"]] and Sigma_theta = all count
  old <- c("Sigma_theta", "\"Sigma_theta\"", "'Sigma_theta'")
  reads <- vapply(
    files,
    function(f) {
      pd <- utils::getParseData(parse(f, keep.source = TRUE))
      any(pd$token != "COMMENT" & pd$text %in% old)
    },
    NA
  )
  expect_identical(unname(basename(files[reads])), character(0))
})

test_that("the engine's Hessian checks warn in INLAvaan's words", {
  lp <- function(x) -sum(x^2) / 2
  init <- c(a = 0.3, b = -0.2)
  hessian_warnings <- function(...) {
    ws <- list()
    withCallingHandlers(
      engine_call(
        lp,
        init = init,
        vb_correction = "none",
        marginals = "gaussian",
        silent = TRUE,
        ...
      ),
      warning = function(w) {
        ws[[length(ws) + 1L]] <<- w
        tryInvokeRestart("muffleWarning")
      }
    )
    ws
  }
  expect_reworded <- function(ws, n, words) {
    expect_length(ws, n)
    for (w in ws) {
      expect_s3_class(w, "inlavaan_hessian_warning")
      expect_false(inherits(w, "inlavanilla_warning"))
      msg <- cli::ansi_strip(conditionMessage(w))
      expect_false(grepl("logpost|`grad`|`hess`", msg))
    }
    expect_match(cli::ansi_strip(conditionMessage(ws[[1]])), words)
  }
  # A gradient whose Jacobian is not symmetric
  ws <- hessian_warnings(gr = function(x) c(-x[1] + 0.5 * x[2], -x[2]))
  expect_reworded(ws, 1L, "gradient of the log-posterior")
  # A Hessian at the mode that is not symmetric, and so also differs from the
  # differences of the gradient
  ws <- hessian_warnings(
    gr = function(x) -x,
    hess_mode = function(x) matrix(c(-1, 0, 0.5, -1), 2)
  )
  expect_reworded(ws, 2L, "relative asymmetry 0.5")
  # A symmetric Hessian at the mode that is wrong
  ws <- hessian_warnings(gr = function(x) -x, hess_mode = function(x) {
    -2 * diag(2)
  })
  expect_reworded(ws, 1L, "central differences of the gradient by")
  # Any other engine warning passes unchanged
  expect_false(engine_hessian_warning("Some other warning of the engine."))
})
