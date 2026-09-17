#' Convergence and Approximation Diagnostics for INLAvaan Models
#'
#' Extract convergence and approximation-quality diagnostics from a fitted
#' \code{INLAvaan} model.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. \code{"global"} (default) returns a named numeric
#'   vector of scalar diagnostics. \code{"param"} returns a data frame with one
#'   row per free parameter containing per-parameter diagnostics.
#' @param ... Currently unused.
#'
#' @details
#' \strong{Global diagnostics} (\code{type = "global"}):
#' \describe{
#'   \item{\code{npar}}{Number of free parameters.}
#'   \item{\code{nsamp}}{Number of posterior samples drawn.}
#'   \item{\code{converged}}{1 if the optimiser converged, 0 otherwise.}
#'   \item{\code{iterations}}{Number of optimiser iterations.}
#'   \item{\code{grad_inf}}{L-infinity norm of the analytic gradient at the mode
#'     (max |grad|). Should be ~0 at convergence.}
#'   \item{\code{grad_inf_rel}}{Relative L-infinity norm of the analytic gradient
#'     (max |grad| / (|par| + 1e-6)).}
#'   \item{\code{grad_l2}}{L2 (Euclidean) norm of the analytic gradient at the mode.}
#'   \item{\code{mode_shift_max}}{Maximum, across parameters, of the Newton
#'     step at the reported mode in posterior-SD units
#'     (max |\eqn{\Sigma_\theta} grad| / se). Unlike the raw gradient norms
#'     this is scale-free: it estimates how far the reported mode sits from
#'     the true posterior mode relative to the posterior uncertainty. Should
#'     be ~0 at convergence.}
#'   \item{\code{hess_cond}}{Condition number of the Hessian (precision matrix)
#'     computed from \eqn{\Sigma_\theta}. Large values indicate near-singularity.}
#'   \item{\code{hess_min_eig}}{Smallest eigenvalue of the Hessian, that is
#'     \eqn{1 / \max \mathrm{eig}(\Sigma_\theta)}. Companion to
#'     \code{hess_cond}: the condition number is relative, so a
#'     well-conditioned Hessian can still be flat in every direction. This is
#'     the absolute curvature of the log-posterior along its flattest
#'     direction, and it carries the scale of the parameters. Reported, not
#'     checked.}
#'   \item{\code{vb_kld_global}}{Global KL divergence from the VB mean correction
#'     (NA if VB correction was not applied).}
#'   \item{\code{vb_applied}}{1 if VB correction was applied, 0 otherwise.}
#'   \item{\code{vb_shift_max}}{Maximum, across parameters, of the absolute
#'     VB correction in posterior-SD units (max |\code{vb_shift_sigma}|). This
#'     is the quantity the fit-time check tests. NA if the VB correction was
#'     not applied.}
#'   \item{\code{kld_max}}{Maximum per-parameter KL divergence from the VB correction.}
#'   \item{\code{kld_mean}}{Mean per-parameter KL divergence.}
#'   \item{\code{vb_mcse_max}}{Maximum, across parameters, of the estimated
#'     quadrature error of the VB shift, in posterior-SD units. See
#'     \code{vb_mcse_sigma} below.}
#'   \item{\code{vb_mcse_mean}}{Mean estimated quadrature error of the VB
#'     shift, in posterior-SD units.}
#'   \item{\code{nmad_max}}{Maximum normalised max-absolute-deviation across
#'     marginals (skew-normal method only; NA otherwise).}
#'   \item{\code{nmad_mean}}{Mean NMAD across marginals.}
#'   \item{\code{scan_end_mass_max}}{Maximum, across parameters, of
#'     \code{scan_end_mass} (see below). NA unless the skew-normal marginal
#'     method was used.}
#' }
#'
#' \strong{Per-parameter diagnostics} (\code{type = "param"}):
#' A data frame with columns:
#' \describe{
#'   \item{\code{names}}{Parameter name.}
#'   \item{\code{grad}}{Analytic gradient of the negative log-posterior at the
#'     mode. Should be ~0 at convergence.}
#'   \item{\code{grad_num}}{Numerical (finite-difference) gradient at the mode.
#'     Should agree with \code{grad}; large discrepancies indicate a bug in the
#'     analytic gradient.}
#'   \item{\code{grad_diff}}{Difference \code{grad_num - grad}: should be ~0.}
#'   \item{\code{grad_abs}}{Absolute analytic gradient.}
#'   \item{\code{grad_rel}}{Relative analytic gradient |grad| / (|par| + 1e-6).}
#'   \item{\code{mode_shift_sigma}}{Newton step at the reported mode in
#'     posterior-SD units. Should be ~0 at convergence.}
#'   \item{\code{kld}}{Per-parameter KL divergence from the VB correction.}
#'   \item{\code{vb_shift}}{VB correction shift (in original scale).}
#'   \item{\code{vb_shift_sigma}}{VB shift in units of posterior SD.}
#'   \item{\code{vb_mcse_sigma}}{Estimated quadrature error of the VB shift, in
#'     posterior-SD units. The shift is the solution of an integral evaluated by
#'     quasi-Monte Carlo over a finite node set, so it carries an integration
#'     error of its own. This estimates that error by splitting the node set in
#'     half and taking half the disagreement between the two half-set solutions.
#'     Read it as an error bar on \code{vb_shift_sigma}: a value of 0.05 means
#'     the reported posterior mean of that parameter could move by roughly that
#'     much, in SD units, purely from the choice of node set. It runs
#'     conservative, because the two halves are negatively correlated and
#'     quasi-Monte Carlo error falls faster than root-n. It is exactly zero for
#'     parameters whose shift is pinned by the saturated-means fast path, since
#'     no quadrature is used there.}
#'   \item{\code{nmad}}{Normalised max-absolute-deviation of the skew-normal fit
#'     (NA when not using the skewnorm method).}
#'   \item{\code{alpha}}{Shape parameter of the fitted skew-normal marginal,
#'     on the unconstrained scale. Zero is a Gaussian marginal, and the sign
#'     gives the direction of the skew (NA when not using the skewnorm
#'     method).}
#'   \item{\code{scan_end_mass}}{Mass the fitted marginal puts outside the
#'     window that was scanned to fit it,
#'     \eqn{F(\hat\theta_j - 4 s_j) + 1 - F(\hat\theta_j + 4 s_j)}, where
#'     \eqn{F} is the fitted skew-normal distribution function,
#'     \eqn{\hat\theta_j} the posterior mode and \eqn{s_j} the Laplace
#'     posterior SD. The marginal is fitted inside that window only, so mass
#'     outside it is extrapolation, and when this is large the 2.5\% and
#'     97.5\% credible limits are the numbers to distrust. A Gaussian marginal
#'     gives \eqn{2\Phi(-4)} = 6.3e-05, and healthy fits sit between 1e-03 and
#'     1e-02 (NA when not using the skewnorm method).}
#' }
#'
#' \strong{Fit-time warnings}: [inlavaan()] runs these checks once at the end
#' of every fit and emits a single consolidated warning (condition class
#' \code{"inlavaan_diagnostics_warning"}) when any of them look off: the
#' optimiser did not converge, \code{mode_shift_max} exceeds 0.1,
#' any marginal has NMAD above 0.1, the VB correction shifted a posterior
#' mean by more than 1 posterior SD, or the Hessian condition number exceeds
#' 1e8. A healthy fit stays silent. Silence the check with
#' \code{suppressWarnings()}, or selectively by handling the condition class.
#'
#' @returns For \code{type = "global"}, a named numeric vector (class
#'   \code{"diagnostics.INLAvaan"}). For \code{type = "param"}, a data frame
#'   (class \code{c("diagnostics.INLAvaan.param", "data.frame")}).
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
#'             test = "none", verbose = FALSE)
#'
#' # Global convergence summary
#' diagnostics(fit)
#'
#' # Per-parameter table
#' diagnostics(fit, type = "param")
#' }
#'
#' @seealso [timing()], [fitmeasures()], [plot()]
#'
#' @export
setGeneric("diagnostics", function(object, ...) standardGeneric("diagnostics"))

#' @name diagnostics
#' @rdname diagnostics
#' @aliases diagnostics,INLAvaan-method
#' @export
setMethod(
  "diagnostics",
  "INLAvaan",
  function(object, type = c("global", "param"), ...) {
    type <- match.arg(type)
    dg <- diagnostics_internal(object@external$inlavaan_internal)
    if (type == "global") {
      dg$global
    } else {
      dg$param
    }
  }
)

# Converged flag and iteration count from an optimiser result; mirrors how
# create_lav_from_inlavaan_internal() fills the lavaan @Fit slot.
optim_convergence <- function(opt, optim_method) {
  if (optim_method == "nlminb") {
    list(
      converged = opt$convergence == 0L,
      iterations = as.integer(opt$iterations)
    )
  } else if (optim_method == "optim") {
    # nocov start
    list(
      converged = opt$convergence == 0L,
      iterations = as.integer(opt$counts["function"])
    )
  } else {
    # ucminf
    list(
      converged = opt$convergence == 1L,
      iterations = as.integer(opt$info["neval"])
    )
  } # nocov end
}

# Shared computation behind diagnostics() and the fit-time warning; works on
# the internal list so it can run before the S4 object exists.
diagnostics_internal <- function(int) {
  grad_analytic <- int$opt$dx_analytic
  grad_num <- int$opt$dx
  pars <- int$theta_star
  parnames <- names(int$coefficients)
  m <- length(parnames)

  Sigma_theta <- int$Sigma_theta
  se_laplace <- sqrt(diag(Sigma_theta))
  conv <- optim_convergence(int$opt, int$optim_method)

  # Newton step at the reported mode in posterior-SD units: a scale-free
  # measure of how far a non-zero gradient says the true mode is
  mode_shift_sigma <- abs(as.numeric(Sigma_theta %*% grad_analytic)) /
    se_laplace

  # VB diagnostics
  vb <- int$vb
  vb_applied <- all(!is.na(vb$correction))
  vb_shift <- if (vb_applied) vb$correction else rep(NA_real_, m)
  vb_kld <- if (vb_applied) vb$kld else rep(NA_real_, m)
  vb_shift_sigma <- vb_shift / se_laplace
  # Quadrature (Monte Carlo) error of the VB shift, in posterior-SD units.
  vb_mcse <- if (vb_applied && !is.null(vb$mcse)) vb$mcse else rep(NA_real_, m)
  vb_mcse_sigma <- vb_mcse / se_laplace

  # Skew-normal fit of each marginal (skewnorm method only). approx_data may
  # carry extra rows for covariance and defined parameters, so keep the first
  # m (marginal-scan) rows. Read one column at a time, because a fit may
  # record some of these columns and not others.
  sn_col <- function(nm) {
    val <- tryCatch(int$approx_data[seq_len(m), nm], error = function(e) NULL)
    if (is.null(val) || length(val) != m) rep(NA_real_, m) else as.numeric(val)
  }
  nmad <- sn_col("nmad")
  sn_alpha <- sn_col("alpha")
  sn_xi <- sn_col("xi")
  sn_omega <- sn_col("omega")

  # Hessian condition number: kappa(H) = kappa(Sigma_theta)
  eig <- eigen(Sigma_theta, symmetric = TRUE, only.values = TRUE)$values
  hess_cond <- if (length(eig) > 0) max(eig) / min(eig) else NA_real_
  # Smallest eigenvalue of the Hessian, the reciprocal of the largest
  # eigenvalue of Sigma_theta. The condition number is relative, so it says
  # nothing on its own about how flat the posterior is. This is the absolute
  # curvature along the flattest direction.
  hess_min_eig <- if (length(eig) > 0) 1 / max(eig) else NA_real_

  # Mass the fitted marginal puts outside the window that was scanned to fit
  # it, four posterior SDs either side of the raw mode. Fits saved before the
  # raw mode was recorded fall back to the reported mode, which moves the
  # window by the VB shift.
  theta_scan <- int$theta_star_novbc
  if (is.null(theta_scan)) {
    theta_scan <- pars
  }
  scan_lo <- theta_scan - 4 * se_laplace
  scan_hi <- theta_scan + 4 * se_laplace
  scan_end_mass <- psnorm(scan_lo, sn_xi, sn_omega, sn_alpha) +
    psnorm(scan_hi, sn_xi, sn_omega, sn_alpha, lower_tail = FALSE)

  global <- c(
    npar = m,
    nsamp = int$nsamp,
    converged = as.numeric(conv$converged),
    iterations = conv$iterations,
    grad_inf = max(abs(grad_analytic)),
    grad_inf_rel = max(abs(grad_analytic) / (abs(pars) + 1e-06)),
    grad_l2 = sqrt(sum(grad_analytic^2)),
    mode_shift_max = max(mode_shift_sigma),
    hess_cond = hess_cond,
    hess_min_eig = hess_min_eig,
    vb_applied = as.numeric(vb_applied),
    vb_shift_max = if (all(is.na(vb_shift_sigma))) {
      NA_real_
    } else {
      max(abs(vb_shift_sigma), na.rm = TRUE)
    },
    vb_kld_global = if (vb_applied) vb$kld_global else NA_real_,
    kld_max = if (all(is.na(vb_kld))) NA_real_ else max(vb_kld, na.rm = TRUE),
    kld_mean = if (all(is.na(vb_kld))) NA_real_ else mean(vb_kld, na.rm = TRUE),
    vb_mcse_max = if (all(is.na(vb_mcse_sigma))) {
      NA_real_
    } else {
      max(vb_mcse_sigma, na.rm = TRUE)
    },
    vb_mcse_mean = if (all(is.na(vb_mcse_sigma))) {
      NA_real_
    } else {
      mean(vb_mcse_sigma, na.rm = TRUE)
    },
    nmad_max = if (all(is.na(nmad))) NA_real_ else max(nmad, na.rm = TRUE),
    nmad_mean = if (all(is.na(nmad))) NA_real_ else mean(nmad, na.rm = TRUE),
    scan_end_mass_max = if (all(is.na(scan_end_mass))) {
      NA_real_
    } else {
      max(scan_end_mass, na.rm = TRUE)
    }
  )
  class(global) <- c("diagnostics.INLAvaan", "numeric")

  param <- data.frame(
    names = parnames,
    grad = grad_analytic,
    grad_num = grad_num,
    grad_diff = grad_num - grad_analytic,
    grad_abs = abs(grad_analytic),
    grad_rel = abs(grad_analytic) / (abs(pars) + 1e-06),
    mode_shift_sigma = mode_shift_sigma,
    kld = vb_kld,
    vb_shift = vb_shift,
    vb_shift_sigma = vb_shift_sigma,
    vb_mcse_sigma = vb_mcse_sigma,
    nmad = nmad,
    alpha = sn_alpha,
    scan_end_mass = scan_end_mass,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(param) <- c("diagnostics.INLAvaan.param", "data.frame")

  list(global = global, param = param)
}

# Fit-time diagnostics check, called once at the end of inlavaan(). Emits a
# single consolidated warning when the checks reported by diagnostics() look
# off. Thresholds are deliberately loose so that a healthy fit stays silent:
# healthy fits sit orders of magnitude below them (mode shifts ~1e-4 SD,
# NMAD < 0.06, VB shifts < 0.6 SD, condition numbers < 1e3). The
# scan-endpoint mass of a healthy fit runs from 1e-3 to 1e-2 (0.001 on
# Holzinger-Swineford with std.lv, 0.0036 on Political Democracy). Across the
# base cells of the calibration study at n >= 150 its p95 was 0.0066 and its
# maximum 0.0104.
warn_fit_diagnostics <- function(
  int,
  mode_shift_tol = 0.1,
  nmad_tol = 0.1,
  scan_end_mass_tol = 0.05,
  vb_shift_tol = 1,
  hess_cond_tol = 1e8
) {
  dg <- diagnostics_internal(int)
  glob <- dg$global
  param <- dg$param
  issues <- character(0)

  fmt <- function(x) formatC(x, digits = 3, format = "g")

  if (glob[["converged"]] == 0) {
    msg <- int$opt$message
    issues <- c(
      issues,
      "x" = paste0(
        "The optimiser did not converge",
        if (!is.null(msg) && nzchar(msg)) paste0(": ", msg),
        "."
      )
    )
  }

  if (isTRUE(glob[["mode_shift_max"]] > mode_shift_tol)) {
    worst <- param$names[which.max(param$mode_shift_sigma)]
    issues <- c(
      issues,
      "x" = paste0(
        "The gradient at the posterior mode is not zero (max |grad| = ",
        fmt(glob[["grad_inf"]]),
        "): a Newton step would move {.code ",
        worst,
        "} by ",
        fmt(glob[["mode_shift_max"]]),
        " posterior SDs."
      )
    )
  }

  bad_nmad <- which(!is.na(param$nmad) & param$nmad > nmad_tol)
  if (length(bad_nmad)) {
    bad_nmad <- bad_nmad[order(param$nmad[bad_nmad], decreasing = TRUE)]
    shown <- head(bad_nmad, 3L)
    listed <- paste0(
      "{.code ",
      param$names[shown],
      "} (",
      formatC(param$nmad[shown], digits = 2, format = "f"),
      ")",
      collapse = ", "
    )
    more <- length(bad_nmad) - length(shown)
    issues <- c(
      issues,
      "x" = paste0(
        "The fitted marginal deviates from the scanned posterior (NMAD > ",
        nmad_tol,
        ") for ",
        listed,
        if (more > 0) paste0(" and ", more, " other", if (more > 1) "s"),
        "."
      )
    )
  }

  bad_mass <- which(
    !is.na(param$scan_end_mass) & param$scan_end_mass > scan_end_mass_tol
  )
  if (length(bad_mass)) {
    bad_mass <- bad_mass[
      order(param$scan_end_mass[bad_mass], decreasing = TRUE)
    ]
    shown <- head(bad_mass, 3L)
    listed <- paste0(
      "{.code ",
      param$names[shown],
      "} (",
      formatC(param$scan_end_mass[shown], digits = 2, format = "f"),
      ")",
      collapse = ", "
    )
    more <- length(bad_mass) - length(shown)
    issues <- c(
      issues,
      "x" = paste0(
        "The fitted marginal puts more than ",
        scan_end_mass_tol,
        " of its mass beyond the scanned window (4 posterior SDs either side
         of the mode) for ",
        listed,
        if (more > 0) paste0(" and ", more, " other", if (more > 1) "s"),
        "; its credible limits rely on extrapolation."
      )
    )
  }

  if (isTRUE(glob[["vb_shift_max"]] > vb_shift_tol)) {
    worst <- param$names[which.max(abs(param$vb_shift_sigma))]
    issues <- c(
      issues,
      "x" = paste0(
        "The VB correction shifted {.code ",
        worst,
        "} by ",
        fmt(glob[["vb_shift_max"]]),
        " posterior SDs; the Gaussian approximation at the mode may be
         inaccurate."
      )
    )
  }

  if (isTRUE(glob[["hess_cond"]] > hess_cond_tol)) {
    issues <- c(
      issues,
      "x" = paste0(
        "The Hessian at the mode is near-singular (condition number = ",
        fmt(glob[["hess_cond"]]),
        "); some parameters may be weakly identified."
      )
    )
  }

  if (length(issues)) {
    cli_warn(
      c(
        "Fit diagnostics flagged {length(issues)} potential issue{?s}:",
        issues,
        "i" = "Inspect with {.code diagnostics(fit)} and
               {.code diagnostics(fit, type = \"param\")}."
      ),
      class = "inlavaan_diagnostics_warning"
    )
  }
  invisible(issues)
}

#' @exportS3Method print diagnostics.INLAvaan
print.diagnostics.INLAvaan <- function(x, ...) {
  nm <- names(x)
  int_names <- c("npar", "nsamp", "converged", "iterations", "vb_applied")
  # Quantities that span orders of magnitude, or sit far below the four
  # decimals of the fixed format, are printed in scientific notation.
  sci_names <- c(
    "hess_cond",
    "hess_min_eig",
    "mode_shift_max",
    "scan_end_mass_max"
  )
  formatted <- vapply(
    seq_along(x),
    function(i) {
      val <- x[i]
      name <- nm[i]

      if (is.na(val)) {
        "NA"
      } else if (name %in% int_names) {
        as.character(as.integer(round(val)))
      } else if (startsWith(name, "grad_") || name %in% sci_names) {
        formatC(val, digits = 2, format = "e")
      } else {
        formatC(val, digits = 4, format = "f", drop0trailing = FALSE)
      }
    },
    character(1)
  )
  names(formatted) <- nm
  print(formatted, quote = FALSE, right = TRUE)
  invisible(x)
}

#' @exportS3Method print diagnostics.INLAvaan.param
print.diagnostics.INLAvaan.param <- function(x, digits = 4, ...) {
  num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  for (nm in num_cols) {
    # A healthy scan-endpoint mass is smaller than the last of the four
    # decimals, so round() would print it as zero.
    x[[nm]] <- if (nm == "scan_end_mass") {
      signif(x[[nm]], 3)
    } else {
      round(x[[nm]], digits)
    }
  }
  print.data.frame(x, ...)
  invisible(x)
}
