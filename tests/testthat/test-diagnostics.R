dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
fit <- acfa(
  mod,
  dat,
  silent = TRUE,
  nsamp = 3,
  test = "none",
  vb_correction = "none",
  marginal_method = "gaussian"
)
# The default marginal method, so the skew-normal columns are populated.
fit_sn <- acfa(
  mod,
  dat,
  silent = TRUE,
  nsamp = 3,
  test = "none",
  vb_correction = "none"
)

# ---- diagnostics() ----

test_that("diagnostics(type = 'global') returns named numeric vector", {
  dg <- diagnostics(fit)
  expect_s3_class(dg, "diagnostics.INLAvaan")
  expect_type(dg, "double")
  # The engine's names in its order, then INLAvaan's, then the deprecated ones
  expected_names <- c(
    "npar",
    "converged",
    "iterations",
    "grad_inf",
    "grad_inf_rel",
    "grad_l2",
    "mode_shift_max",
    "hess_cond",
    "hess_cond_cor",
    "vb_applied",
    "vb_shift_max",
    "vb_kld_global",
    "kld_max",
    "kld_mean",
    "nmad_max",
    "nmad_mean",
    "unscanned_mass_max",
    "grad_gap_max",
    "vb_fallback",
    "vb_step_max",
    "vb_lhat",
    "vb_j0",
    "vb_j1",
    "vb_solve",
    "mean_gap_max",
    "ridge_bend_max",
    "n_heavy_tails",
    "nsamp",
    "psi_npd_share",
    "theta_npd_share",
    "vb_mcse_max",
    "vb_mcse_mean"
  )
  expect_named(dg, expected_names)
  expect_equal(unname(dg["npar"]), length(coef(fit)))
  expect_equal(unname(dg["converged"]), 1)
  expect_equal(unname(dg["nsamp"]), 3)
  expect_true(all(is.na(dg[c("vb_mcse_max", "vb_mcse_mean")])))
  # The engine's numbers
  int <- get_inlavaan_internal(fit)
  ed <- int$engine$diagnostics
  eng <- diagnostics_internal(ed$optim, ed$vb, ed$marginals, int$theta_star)
  n_eng <- length(eng$global)
  expect_identical(unclass(dg)[seq_len(n_eng)], unclass(eng$global))
  # hess_cond is that of the covariance matrix, as before
  ev <- eigen(int$Omega, only.values = TRUE)$values
  expect_equal(dg[["hess_cond"]], max(ev) / min(ev))
})

test_that("diagnostics(type = 'param') returns data frame", {
  dp <- diagnostics(fit, type = "param")
  expect_s3_class(dp, "diagnostics.INLAvaan.param")
  expect_s3_class(dp, "data.frame")
  expect_equal(nrow(dp), length(coef(fit)))
  expect_named(
    dp,
    c(
      "names",
      "grad",
      "grad_abs",
      "grad_rel",
      "mode_shift_sigma",
      "kld",
      "vb_shift",
      "vb_shift_sigma",
      "nmad",
      "alpha",
      "unscanned_mass",
      "grad_gap",
      "vb_step_sigma",
      "mean_gap",
      "gamma1",
      "tails",
      "ridge_bend",
      "grad_num",
      "grad_diff",
      "vb_mcse_sigma"
    )
  )
  expect_identical(dp$names, names(coef(fit)))
  # Deprecated in 0.4.0: grad_gap checks the gradient instead
  expect_true(all(is.na(dp[c("grad_num", "grad_diff", "vb_mcse_sigma")])))
  expect_true(all(is.finite(dp$grad_gap)))
  expect_equal(diagnostics(fit)[["grad_gap_max"]], max(dp$grad_gap))
})

test_that("diagnostics() reports the skew-normal marginal fit", {
  glob <- diagnostics(fit_sn)
  dp <- diagnostics(fit_sn, type = "param")
  expect_true(all(c("alpha", "unscanned_mass") %in% names(dp)))
  expect_true(all(is.finite(dp$alpha)))
  expect_true(all(is.finite(dp$unscanned_mass)))
  expect_true(all(dp$unscanned_mass > 0 & dp$unscanned_mass < 1))
  # A healthy fit leaves next to nothing outside the scanned window: the
  # Gaussian reference is 6.3e-05 and this model sits near 2.5e-04.
  expect_lt(max(dp$unscanned_mass), 0.05)
  expect_equal(glob[["unscanned_mass_max"]], max(dp$unscanned_mass))
  expect_true(all(dp$tails))
  expect_equal(glob[["n_heavy_tails"]], 0)
  # The names before 0.4.0 are gone
  expect_false("scan_end_mass_max" %in% names(glob))
  expect_false("scan_end_mass" %in% names(dp))
})

test_that("the skew-normal diagnostics are NA for another method", {
  glob <- diagnostics(fit)
  dp <- diagnostics(fit, type = "param")
  expect_true(is.na(glob[["unscanned_mass_max"]]))
  expect_true(is.na(glob[["nmad_max"]]))
  expect_true(all(is.na(dp$alpha)))
  expect_true(all(is.na(dp$unscanned_mass)))
})

# ---- fit-time diagnostics warnings ----

test_that("a healthy fit passes warn_fit_diagnostics() silently", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  expect_no_warning(INLAvaan:::warn_fit_diagnostics(int))
})

test_that("an unconverged optimiser triggers the fit-time warning", {
  w <- expect_warning(
    acfa(
      mod,
      dat,
      verbose = FALSE,
      nsamp = 3,
      test = "none",
      vb_correction = "none",
      marginal_method = "gaussian",
      std.lv = TRUE, # keeps the Hessian PD at the early stopping point
      control = inlavaan_control(mode_maxit = 2)
    ),
    class = "inlavaan_diagnostics_warning"
  )
  msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  expect_match(msg, "did not converge")
  expect_match(msg, "posterior mode is not zero")
  # The advice names the entries of inlavaan_control() that raise the limits
  expect_match(msg, "mode_maxit", fixed = TRUE)
  expect_match(msg, "inlavaan_control()", fixed = TRUE)
  # The bullet on the mode gives the message of nlminb() and the advice, so
  # the bullet on the convergence does not repeat them
  expect_match(msg, "The optimiser did not converge.", fixed = TRUE)
  count <- function(pattern) {
    lengths(regmatches(msg, gregexpr(pattern, msg, fixed = TRUE)))
  }
  expect_identical(count("iteration limit reached"), 1L)
  expect_identical(count("mode_maxit"), 1L)
})

test_that("an optimiser that stops short near the mode gets advice", {
  int <- get_inlavaan_internal(fit)
  warning_for <- function(message) {
    int$engine$diagnostics$optim$convergence <- 1L
    int$engine$diagnostics$optim$message <- message
    w <- expect_warning(
      warn_fit_diagnostics(int),
      class = "inlavaan_diagnostics_warning"
    )
    gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  }
  # Stopped at a limit, with the mode not flagged
  msg <- warning_for("iteration limit reached without convergence (10)")
  expect_match(msg, "flagged 1 potential issue:", fixed = TRUE)
  expect_match(
    msg,
    "did not converge: iteration limit reached without convergence (10).",
    fixed = TRUE
  )
  expect_match(
    msg,
    "Raise `mode_maxit` or `mode_maxeval` in `inlavaan_control()`.",
    fixed = TRUE
  )
  expect_no_match(msg, "says:", fixed = TRUE)
  # Stopped for another reason
  msg <- warning_for("false convergence (8)")
  expect_match(msg, "did not converge: false convergence (8).", fixed = TRUE)
  expect_match(msg, "Try other starting values", fixed = TRUE)
  expect_no_match(msg, "mode_maxit", fixed = TRUE)
})

test_that("silent = TRUE prints nothing, and the fit records its warnings", {
  fit_unconverged <- function(...) {
    acfa(
      mod,
      dat,
      nsamp = 3,
      test = "none",
      vb_correction = "none",
      marginal_method = "gaussian",
      std.lv = TRUE,
      control = inlavaan_control(mode_maxit = 2),
      ...
    )
  }
  expect_silent(fit_s <- fit_unconverged(silent = TRUE))
  rec <- get_inlavaan_internal(fit_s, "warnings")
  expect_length(rec, 1L)
  expect_match(rec, "did not converge")

  # Without silent the warning is raised, and the same text is recorded
  w <- expect_warning(
    fit_v <- fit_unconverged(verbose = FALSE),
    class = "inlavaan_diagnostics_warning"
  )
  expect_identical(
    get_inlavaan_internal(fit_v, "warnings"),
    cli::ansi_strip(conditionMessage(w))
  )
  expect_identical(rec, get_inlavaan_internal(fit_v, "warnings"))

  # Messages are muffled too
  expect_silent(acfa(
    mod,
    dat,
    likelihood = "wishart",
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian"
  ))
  expect_identical(get_inlavaan_internal(fit, "warnings"), character(0))
  expect_error(acfa(mod, dat, silent = NA), "silent")
  # verbose = TRUE prints the details, which silent = TRUE would muffle
  expect_error(
    acfa(mod, dat, verbose = TRUE, silent = TRUE),
    "cannot both be"
  )
  expect_error(acfa(mod, dat, verbose = NA), "verbose")
})

test_that("warn_fit_diagnostics() flags high NMAD marginals by name", {
  int <- INLAvaan:::get_inlavaan_internal(fit_sn)
  int$engine$diagnostics$marginals$nmad[1:4] <- c(0.5, 0.3, 0.2, 0.15)
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "NMAD")
  expect_match(conditionMessage(w), names(coef(fit_sn))[1], fixed = TRUE)
  expect_match(conditionMessage(w), "1 other") # top 3 shown, 1 elided
  expect_match(conditionMessage(w), "flagged 1 potential issue:")
  expect_match(conditionMessage(w), "?diagnostics", fixed = TRUE)
})

test_that("a healthy skew-normal fit passes warn_fit_diagnostics() silently", {
  int <- INLAvaan:::get_inlavaan_internal(fit_sn)
  expect_no_warning(INLAvaan:::warn_fit_diagnostics(int))
})

test_that("warn_fit_diagnostics() flags a marginal that outruns the scan", {
  int <- INLAvaan:::get_inlavaan_internal(fit_sn)
  int$engine$diagnostics$marginals$unscanned_mass[1] <- 0.71
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "beyond the scanned")
  expect_match(conditionMessage(w), names(coef(fit_sn))[1], fixed = TRUE)
  expect_no_match(conditionMessage(w), "far from the posterior mass")
  # Past 0.9 the mode itself is in doubt
  int$engine$diagnostics$marginals$unscanned_mass[1] <- 0.95
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "far from the posterior mass")
})

test_that("warn_fit_diagnostics() flags large VB shifts", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  m <- length(coef(fit))
  ed <- int$engine$diagnostics
  ed$vb <- list(
    iterations = 1L,
    fallback = FALSE,
    one_pass = TRUE,
    solve = FALSE,
    check = c(lhat = 0.2, j0 = -0.1, j1 = 0.1),
    check_worst = names(coef(fit))[1],
    kld_global = 1
  )
  ed$marginals$vb_shift_sigma <- rep(2, m)
  ed$marginals$kld <- rep(2, m)
  int$engine$diagnostics <- ed
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "VB correction shifted")
  expect_no_match(conditionMessage(w), "failed its check")
})

test_that("the advice on an unsettled VB correction names no ended argument", {
  int <- get_inlavaan_internal(fit_sn)
  expect_true(int$engine$has_grad)
  m <- length(coef(fit_sn))
  ed <- int$engine$diagnostics
  ed$vb <- list(
    iterations = 1L,
    fallback = FALSE,
    one_pass = TRUE,
    solve = FALSE,
    check = c(lhat = 0.2, j0 = -0.1, j1 = 0.1),
    check_worst = names(coef(fit_sn))[1],
    kld_global = 0.01
  )
  ed$marginals$vb_shift_sigma <- rep(0.1, m)
  ed$marginals$kld <- rep(0.005, m)
  ed$marginals$vb_step_sigma <- c(0.5, rep(0.01, m - 1L))
  int$engine$diagnostics <- ed
  message_of <- function(int) {
    w <- expect_warning(
      warn_fit_diagnostics(int),
      class = "inlavaan_diagnostics_warning"
    )
    gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  }
  msg <- message_of(int)
  expect_match(msg, "did not settle", fixed = TRUE)
  expect_no_match(msg, "numerical_grad", fixed = TRUE)
  # Every fit has the analytic gradient, so INLAvaan has no advice for a fit
  # without one, and the warning is the same whatever has_grad says.
  int$engine$has_grad <- FALSE
  expect_identical(message_of(int), msg)
  int$engine$has_grad <- NULL
  expect_identical(message_of(int), msg)
})

test_that("warn_fit_diagnostics() reports a failed VB one-step check", {
  int <- get_inlavaan_internal(fit_sn)
  m <- length(coef(fit_sn))
  worst <- names(coef(fit_sn))[2]
  ed <- int$engine$diagnostics
  ed$vb <- list(
    iterations = 4L,
    fallback = FALSE,
    one_pass = FALSE,
    solve = TRUE,
    check = c(lhat = 1.8, j0 = -0.2, j1 = 0.3),
    check_worst = worst,
    kld_global = 0.01
  )
  ed$marginals$vb_shift_sigma <- rep(0.1, m)
  ed$marginals$kld <- rep(0.005, m)
  # A solve that stopped within the settling level counts as solved
  ed$marginals$vb_step_sigma <- c(0.05, rep(0.01, m - 1L))
  int$engine$diagnostics <- ed
  expect_equal(diagnostics_fit(int)$global[["vb_solve"]], 1)
  message_of <- function(int) {
    w <- expect_warning(
      warn_fit_diagnostics(int),
      class = "inlavaan_diagnostics_warning"
    )
    gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  }
  msg <- message_of(int)
  expect_match(msg, "flagged 1 potential issue:", fixed = TRUE)
  expect_match(msg, "The one-step VB correction failed its check", fixed = TRUE)
  expect_match(msg, paste0("most of all for `", worst, "`"), fixed = TRUE)
  expect_match(msg, "The VB equation was solved instead.", fixed = TRUE)
  expect_match(msg, "compare with MCMC, such as blavaan", fixed = TRUE)
  expect_no_match(msg, "did not settle", fixed = TRUE)
  # The skew-normal marginals do not rest on the VB mean, and the Gaussian
  # marginals do
  expect_no_match(msg, "posterior means", fixed = TRUE)
  int$marginal_method <- "gaussian"
  expect_match(message_of(int), "Treat the posterior means,", fixed = TRUE)
  # A solve that stopped short is not said to have solved the equation, and
  # the step still to go is in the bullet on the check, not in one of its own
  int$engine$diagnostics$marginals$vb_step_sigma <- c(
    0.01,
    0.5,
    rep(0.01, m - 2L)
  )
  msg <- message_of(int)
  expect_match(msg, "flagged 1 potential issue:", fixed = TRUE)
  expect_no_match(msg, "solved instead", fixed = TRUE)
  expect_match(
    msg,
    paste0("stopped short: one more step would move `", worst, "` by 0.5"),
    fixed = TRUE
  )
  expect_no_match(msg, "did not settle", fixed = TRUE)
})

test_that("a fit that fails the VB one-step check warns once, in INLAvaan's words", {
  # On its first 60 rows the posterior of this model is far from Gaussian:
  # the one step does not shrink the residual of the VB equation (vb_lhat is
  # about 2).
  caught <- list()
  fit_vb <- withCallingHandlers(
    acfa(
      "visual =~ x1 + x2 + x3",
      dat[1:60, ],
      verbose = FALSE,
      nsamp = 3,
      test = "none"
    ),
    warning = function(w) {
      caught[[length(caught) + 1L]] <<- w
      tryInvokeRestart("muffleWarning")
    }
  )
  dg <- diagnostics(fit_vb)
  expect_equal(dg[["vb_solve"]], 1)
  expect_gt(dg[["vb_lhat"]], 1)
  # The engine's own warning is muffled, and the fit warning reports the check
  expect_length(caught, 1L)
  w <- caught[[1L]]
  expect_s3_class(w, "inlavaan_diagnostics_warning")
  expect_false(inherits(w, "inlavanilla_warning"))
  msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  expect_match(msg, "The one-step VB correction failed its check", fixed = TRUE)
  worst <- get_inlavaan_internal(fit_vb)$engine$diagnostics$vb$check_worst
  expect_match(msg, paste0("most of all for `", worst, "`"), fixed = TRUE)
  expect_identical(
    get_inlavaan_internal(fit_vb, "warnings"),
    cli::ansi_strip(conditionMessage(w))
  )
})

test_that("a healthy fit passes the VB one-step check silently", {
  fit_ok <- acfa(mod, dat, silent = TRUE, nsamp = 3, test = "none")
  expect_identical(get_inlavaan_internal(fit_ok, "warnings"), character(0))
  dg <- diagnostics(fit_ok)
  expect_equal(dg[["vb_solve"]], 0)
  expect_lt(dg[["vb_lhat"]], 1)
  expect_true(all(abs(dg[c("vb_j0", "vb_j1")]) < 1))
  # After a passed check the one step stands, with no step still to go
  expect_true(get_inlavaan_internal(fit_ok)$engine$diagnostics$vb$one_pass)
  expect_true(is.na(dg[["vb_step_max"]]))
  # The full correction iterates to its fixed point, and has no check
  fit_full <- acfa(
    mod,
    dat,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "full"
  )
  dg_full <- diagnostics(fit_full)
  expect_true(all(is.na(dg_full[c("vb_lhat", "vb_j0", "vb_j1", "vb_solve")])))
})

test_that("warn_fit_diagnostics() flags a near-singular Hessian", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  # The check reads the condition number on the correlation scale
  int$engine$diagnostics$optim$hess_cond <- 1e12
  expect_no_warning(INLAvaan:::warn_fit_diagnostics(int))
  int$engine$diagnostics$optim$hess_cond_cor <- 1e12
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "near-singular")
})

test_that("warn_fit_diagnostics() flags a gradient that does not match", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  int$engine$diagnostics$marginals$grad_gap[2] <- 0.5
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  msg <- conditionMessage(w)
  expect_match(msg, "analytic gradient does not match")
  expect_match(msg, names(coef(fit))[2], fixed = TRUE)
  # The advice line that follows is not counted as an issue
  expect_match(msg, "flagged 1 potential issue:")
})

test_that("warn_fit_diagnostics() flags heavy tails", {
  int <- INLAvaan:::get_inlavaan_internal(fit_sn)
  int$engine$diagnostics$marginals$tails[3] <- FALSE
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "Heavy tails")
  expect_match(conditionMessage(w), names(coef(fit_sn))[3], fixed = TRUE)
  expect_equal(diagnostics_fit(int)$global[["n_heavy_tails"]], 1)
})

test_that("heavy tails on a loading of a factor with a marker get a hint", {
  int <- INLAvaan:::get_inlavaan_internal(fit_sn)
  loading <- match("visual=~x2", names(coef(fit_sn)))
  int$engine$diagnostics$marginals$tails[loading] <- FALSE
  w <- expect_warning(
    issues <- INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  msg <- conditionMessage(w)
  expect_match(msg, "std.lv = TRUE", fixed = TRUE)
  # The hint is not an issue of its own
  expect_match(msg, "flagged 1 potential issue:")
  # It follows the bullet that it explains
  at <- which(startsWith(issues, "Heavy tails"))
  expect_identical(names(issues)[at + 1L], "i")
  expect_match(issues[[at + 1L]], "std.lv = TRUE", fixed = TRUE)
})

test_that("heavy tails get no hint where std.lv = TRUE cannot help", {
  # A residual variance
  int <- INLAvaan:::get_inlavaan_internal(fit_sn)
  variance <- match("x1~~x1", names(coef(fit_sn)))
  int$engine$diagnostics$marginals$tails[variance] <- FALSE
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "Heavy tails")
  expect_false(grepl("std.lv", conditionMessage(w), fixed = TRUE))

  # A loading of a factor with a fixed variance and no marker
  fit_lv <- acfa(
    mod,
    dat,
    std.lv = TRUE,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    vb_correction = "none"
  )
  int <- INLAvaan:::get_inlavaan_internal(fit_lv)
  loading <- match("visual=~x2", names(coef(fit_lv)))
  int$engine$diagnostics$marginals$tails[loading] <- FALSE
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "Heavy tails")
  expect_false(grepl("std.lv", conditionMessage(w), fixed = TRUE))
})

test_that("heavy_marker_loading() needs a flagged loading and a marker", {
  pt <- INLAvaan:::get_inlavaan_internal(fit_sn)$partable
  expect_true(INLAvaan:::heavy_marker_loading("textual=~x6", pt))
  expect_true(INLAvaan:::heavy_marker_loading(c("x1~~x1", "visual=~x3"), pt))
  expect_false(INLAvaan:::heavy_marker_loading("visual~~textual", pt))
  expect_false(INLAvaan:::heavy_marker_loading(character(0), pt))
  # A loading freed by hand, on a factor with a fixed variance
  pt$free[pt$names == "textual=~x4"] <- max(pt$free) + 1L
  pt$ustart[pt$names == "textual=~x4"] <- NA
  expect_false(INLAvaan:::heavy_marker_loading("textual=~x6", pt))
  expect_true(INLAvaan:::heavy_marker_loading("visual=~x2", pt))
})

test_that("print.diagnostics.INLAvaan works", {
  dg <- diagnostics(fit)
  expect_output(print(dg))
})

test_that("print.diagnostics.INLAvaan.param works", {
  dp <- diagnostics(fit, type = "param")
  expect_output(print(dp))
})

# ---- timing() ----

test_that("timing(what = 'total') returns total time", {
  tt <- timing(fit)
  expect_s3_class(tt, "timing.INLAvaan")
  expect_type(tt, "double")
  expect_named(tt, "total")
  expect_true(tt > 0)
})

test_that("timing(what = 'all') returns all segments", {
  tt <- timing(fit, what = "all")
  expect_s3_class(tt, "timing.INLAvaan")
  expect_true(length(tt) > 1)
  expect_true("total" %in% names(tt))
})

test_that("timing(what = ...) selects specific segments", {
  tt <- timing(fit, what = c("optim", "total"))
  expect_named(tt, c("optim", "total"))
})

test_that("timing rejects unknown segments", {
  expect_error(timing(fit, what = "nonexistent"), "Unknown")
})

test_that("timing distinguishes a not-run loo/waic from an unknown segment", {
  # fit above uses test = "none", so "loo"/"waic" are valid segment names
  # that simply were not computed for this fit
  expect_error(timing(fit, what = "loo"), "not computed", fixed = TRUE)
  expect_error(timing(fit, what = "waic"), "not computed", fixed = TRUE)
  expect_error(timing(fit, what = c("loo", "nonexistent")), "Unknown")
  expect_error(
    timing(fit, what = c("loo", "nonexistent")),
    "not computed",
    fixed = TRUE
  )
})

test_that("timing segments are disjoint and sum to the total", {
  tt <- timing(fit, what = "all")
  # the lavaan setup is inside "init", not a second set of segments; the
  # absolute start_time stamp is not a duration and must not be exposed
  expect_false(any(c("start_time", "ov.names", "SampleStats") %in% names(tt)))
  expect_equal(
    unname(tt[["total"]]),
    sum(tt[setdiff(names(tt), "total")]),
    tolerance = 1e-8
  )
})

test_that("print.timing.INLAvaan works", {
  tt <- timing(fit, what = "all")
  expect_output(print(tt))
})

# ---- positive definiteness of psi and theta ----

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
fit_pd <- asem(
  pd_mod,
  lavaan::PoliticalDemocracy,
  silent = TRUE,
  nsamp = 20,
  test = "none"
)

test_that("a model with residual covariances has PD psi and theta", {
  dg <- diagnostics(fit_pd)
  expect_equal(dg[["psi_npd_share"]], 0)
  expect_equal(dg[["theta_npd_share"]], 0)
  int <- INLAvaan:::get_inlavaan_internal(fit_pd)
  expect_true(all(int$cov_pd$est_ok))
})

test_that("cov_pd_check() finds a residual block that is not PD", {
  int <- INLAvaan:::get_inlavaan_internal(fit_pd)
  x <- int$coefficients
  # Correlations of 0.9, 0.9, 0.9 and -0.9 around the cycle y2-y4-y8-y6
  # cannot hold together: the smallest eigenvalue of that block is -0.27.
  set_cor <- function(x, a, b, r) {
    x[[paste0(a, "~~", b)]] <- r *
      sqrt(x[[paste0(a, "~~", a)]] * x[[paste0(b, "~~", b)]])
    x
  }
  x <- set_cor(x, "y2", "y4", 0.9)
  x <- set_cor(x, "y2", "y6", 0.9)
  x <- set_cor(x, "y4", "y8", 0.9)
  x <- set_cor(x, "y6", "y8", -0.9)
  x_samp <- rbind(x, x, int$coefficients, int$coefficients)
  res <- INLAvaan:::cov_pd_check(x, x_samp, int$lavmodel)
  expect_false(res$est_ok[["theta"]])
  expect_true(res$est_ok[["psi"]])
  expect_equal(res$npd_share[["theta"]], 0.5)
  expect_equal(res$npd_share[["psi"]], 0)
})

test_that("a CFA without free covariances in theta skips the check", {
  int <- INLAvaan:::get_inlavaan_internal(fit)
  # psi has the factor covariance, theta is diagonal
  expect_equal(int$cov_pd$npd_share[["theta"]], 0)
})

test_that("warn_fit_diagnostics() flags psi and theta that are not PD", {
  int <- INLAvaan:::get_inlavaan_internal(fit_pd)
  int$cov_pd$est_ok[["psi"]] <- FALSE
  int$cov_pd$npd_share[["theta"]] <- 0.25
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "latent variables")
  expect_match(conditionMessage(w), "posterior estimates")
  expect_match(conditionMessage(w), "25.0% of the posterior draws")
})

test_that("a small share of draws that are not PD stays silent", {
  int <- INLAvaan:::get_inlavaan_internal(fit_pd)
  int$cov_pd$npd_share[["theta"]] <- 0.005
  expect_no_warning(INLAvaan:::warn_fit_diagnostics(int))
})

test_that("fits saved without cov_pd report NA and do not warn", {
  int <- INLAvaan:::get_inlavaan_internal(fit_pd)
  int$cov_pd <- NULL
  glob <- INLAvaan:::diagnostics_fit(int)$global
  expect_true(is.na(glob[["psi_npd_share"]]))
  expect_no_warning(INLAvaan:::warn_fit_diagnostics(int))
})
