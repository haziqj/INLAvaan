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
  vb_correction = FALSE,
  marginal_method = "marggaus"
)
# The default marginal method, so the skew-normal columns are populated.
fit_sn <- acfa(
  mod,
  dat,
  silent = TRUE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE
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
    "vb_gain_ratio",
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
  expect_identical(unclass(dg)[1:24], unclass(eng$global)[1:24])
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
      vb_correction = FALSE,
      marginal_method = "marggaus",
      std.lv = TRUE, # keeps the Hessian PD at the early stopping point
      control = list(iter.max = 2)
    ),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "did not converge")
  expect_match(conditionMessage(w), "posterior mode is not zero")
})

test_that("silent = TRUE prints nothing, and the fit records its warnings", {
  fit_unconverged <- function(...) {
    acfa(
      mod,
      dat,
      nsamp = 3,
      test = "none",
      vb_correction = FALSE,
      marginal_method = "marggaus",
      std.lv = TRUE,
      control = list(iter.max = 2),
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
    vb_correction = FALSE,
    marginal_method = "marggaus"
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
    gain_ratio = 0.9,
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
  # A one-step correction that delivered little of its promised gain
  int$engine$diagnostics$marginals$vb_shift_sigma <- rep(0.1, m)
  int$engine$diagnostics$vb$gain_ratio <- 0.3
  w <- expect_warning(
    INLAvaan:::warn_fit_diagnostics(int),
    class = "inlavaan_diagnostics_warning"
  )
  expect_match(conditionMessage(w), "One-step VB delivered")
  expect_match(conditionMessage(w), "vb_correction = \"full\"", fixed = TRUE)
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
