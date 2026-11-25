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
#' The diagnostics are those of the engine that fits the model (the files of
#' inlavanilla that INLAvaan copies), in its order and under its names. Then
#' come the diagnostics that INLAvaan adds for structural equation models, and
#' last the names that INLAvaan 0.4.0 deprecates. Read the values by name, not
#' by position. A posterior SD here is the Laplace SD at the mode, \eqn{s_j},
#' the square root of the diagonal of \eqn{\Omega}, the covariance matrix of
#' the Laplace approximation. Values that a fit did not compute are
#' \code{NA}: the VB quantities with \code{vb_correction = "none"}, and the
#' scan quantities with Gaussian marginals (\code{marginal_method =
#' "gaussian"}) and for the intercepts of a saturated mean structure, whose
#' marginals are Gaussian and have no scan.
#'
#' \strong{Global diagnostics} (\code{type = "global"}):
#' \describe{
#'   \item{\code{npar}}{Number of free parameters.}
#'   \item{\code{converged}}{1 if the optimiser converged, 0 otherwise.}
#'   \item{\code{iterations}}{Number of optimiser iterations. When the
#'     gradient at the first mode is too large, the optimiser runs again from
#'     there, and this is the total of both runs.}
#'   \item{\code{grad_inf}}{L-infinity norm of the analytic gradient at the mode
#'     (max |grad|). Should be ~0 at convergence.}
#'   \item{\code{grad_inf_rel}}{Relative L-infinity norm of the analytic gradient
#'     (max |grad| / (|par| + 1e-6)).}
#'   \item{\code{grad_l2}}{L2 (Euclidean) norm of the analytic gradient at the mode.}
#'   \item{\code{mode_shift_max}}{Maximum, across parameters, of the Newton
#'     step at the reported mode in posterior-SD units
#'     (max |\eqn{\Omega} grad| / s). Unlike the raw gradient norms
#'     this is scale-free: it estimates how far the reported mode sits from
#'     the true posterior mode relative to the posterior uncertainty. Should
#'     be ~0 at convergence.}
#'   \item{\code{hess_cond}}{Condition number of the Hessian at the mode, the
#'     ratio of the largest to the smallest eigenvalue of \eqn{\Omega}. It
#'     depends on the units of the parameters.}
#'   \item{\code{hess_cond_cor}}{The same for the posterior correlation
#'     matrix, with each parameter in its own posterior-SD units, so it is
#'     free of units. Large values mean that some combination of the
#'     parameters is weakly identified. The fit-time check reads this one.}
#'   \item{\code{vb_applied}}{1 if VB correction was applied, 0 otherwise.}
#'   \item{\code{vb_shift_max}}{Maximum, across parameters, of the absolute
#'     VB correction in posterior-SD units (max |\code{vb_shift_sigma}|).}
#'   \item{\code{vb_kld_global}}{KL divergence between the Laplace Gaussian
#'     and its VB-shifted copy, \eqn{\frac12 \delta^\top \Omega^{-1}
#'     \delta}{delta' Omega^-1 delta / 2}, which is also the VB term of the
#'     marginal log-likelihood.}
#'   \item{\code{kld_max}}{Maximum per-parameter KL divergence from the VB correction.}
#'   \item{\code{kld_mean}}{Mean per-parameter KL divergence.}
#'   \item{\code{nmad_max}}{Maximum normalised max-absolute-deviation across
#'     the scanned marginals.}
#'   \item{\code{nmad_mean}}{Mean NMAD across the scanned marginals.}
#'   \item{\code{unscanned_mass_max}}{Maximum, across parameters, of
#'     \code{unscanned_mass} (see below). Before INLAvaan 0.4.0 it was
#'     \code{scan_end_mass_max}.}
#'   \item{\code{grad_gap_max}}{Maximum \code{grad_gap} (see below). Should be
#'     ~0.}
#'   \item{\code{vb_fallback}}{1 if the VB correction handed over to its
#'     fallback optimiser, 0 otherwise. It is recorded only: the fallback is a
#'     route of the solver, and the other VB checks judge where it lands.}
#'   \item{\code{vb_step_max}}{Maximum \code{vb_step_sigma} (see below).}
#'   \item{\code{vb_gain_ratio}}{The rise in the expected log-posterior that
#'     the one-step VB correction delivered, as a share of the rise that it
#'     promised. Near 1 is healthy. \code{NA} when the correction iterated
#'     (\code{vb_correction = "full"}), or its step was too small to check.}
#'   \item{\code{mean_gap_max}}{Maximum, across parameters, of
#'     |\code{mean_gap}| (see below).}
#'   \item{\code{ridge_bend_max}}{Maximum \code{ridge_bend} (see below).}
#'   \item{\code{n_heavy_tails}}{Number of marginals with heavy tails, those
#'     whose \code{tails} is \code{FALSE}.}
#'   \item{\code{nsamp}}{Number of posterior samples drawn.}
#'   \item{\code{psi_npd_share}}{Share of the posterior draws in which the
#'     covariance matrix of the latent variables (\code{psi}) of some block is
#'     not positive definite. Each correlation is kept in (-1, 1) on its own,
#'     but a block of several free correlations can still leave the matrix
#'     indefinite, and the likelihood guards only the model-implied
#'     covariance matrix. Zero when no \code{psi} matrix has a free
#'     covariance.}
#'   \item{\code{theta_npd_share}}{The same share for the residual covariance
#'     matrix of the observed variables (\code{theta}).}
#'   \item{\code{vb_mcse_max}, \code{vb_mcse_mean}}{Deprecated, and always
#'     \code{NA}: the VB correction uses a deterministic rule, which has no
#'     quadrature error. They will be removed in INLAvaan 0.5.0.}
#' }
#'
#' \strong{Per-parameter diagnostics} (\code{type = "param"}):
#' A data frame with columns:
#' \describe{
#'   \item{\code{names}}{Parameter name.}
#'   \item{\code{grad}}{Analytic gradient of the negative log-posterior at the
#'     mode. Should be ~0 at convergence.}
#'   \item{\code{grad_abs}}{Absolute analytic gradient.}
#'   \item{\code{grad_rel}}{Relative analytic gradient |grad| / (|par| + 1e-6),
#'     with par the posterior mean on the internal scale.}
#'   \item{\code{mode_shift_sigma}}{Newton step at the reported mode in
#'     posterior-SD units. Should be ~0 at convergence.}
#'   \item{\code{kld}}{KL divergence between the Laplace Gaussian marginal and
#'     its VB-shifted copy, \code{vb_shift_sigma^2 / 2}.}
#'   \item{\code{vb_shift}}{VB correction shift (on the internal scale).}
#'   \item{\code{vb_shift_sigma}}{VB shift in units of posterior SD.}
#'   \item{\code{nmad}}{Normalised max-absolute-deviation of the skew-normal
#'     fit from the scanned posterior.}
#'   \item{\code{alpha}}{Shape parameter of the fitted skew-normal marginal,
#'     on the internal scale. Zero is a Gaussian marginal, and the sign gives
#'     the direction of the skew.}
#'   \item{\code{unscanned_mass}}{Mass the fitted marginal puts outside the
#'     window that was scanned to fit it,
#'     \eqn{F(\hat\theta_j - 4 s_j) + 1 - F(\hat\theta_j + 4 s_j)}, where
#'     \eqn{F} is the fitted skew-normal distribution function and
#'     \eqn{\hat\theta_j} the posterior mode. The marginal is fitted inside
#'     that window only, so mass outside it is extrapolation, and when this is
#'     large the 2.5\% and 97.5\% credible limits are the numbers to distrust.
#'     A Gaussian marginal gives \eqn{2\Phi(-4)} = 6.3e-05, and healthy fits
#'     sit between 1e-03 and 1e-02. Before INLAvaan 0.4.0 it was
#'     \code{scan_end_mass}.}
#'   \item{\code{grad_gap}}{How far the analytic gradient is from central
#'     differences of the log-posterior at the mode: the larger of the gap in
#'     slope per conditional posterior SD (about how far a wrong gradient
#'     moves the mode, in posterior SDs) and half the relative gap in
#'     curvature (about the relative error it makes in a posterior SD).
#'     Should be ~0.}
#'   \item{\code{vb_step_sigma}}{The step still to go at the VB mean, in
#'     posterior-SD units, as \code{mode_shift_sigma} is for the mode.
#'     \code{NA} after the default one-step VB correction.}
#'   \item{\code{mean_gap}}{Gap between the mean of the skew-normal marginal
#'     and the VB mean, in posterior-SD units. Both estimate the shift from the
#'     mode to the posterior mean, so a large gap says that the posterior is
#'     far from Gaussian along this parameter.}
#'   \item{\code{gamma1}}{The volume correction of the scan: the slope, per
#'     posterior SD, added to the scanned log-posterior for the changing
#'     volume of the other parameters.}
#'   \item{\code{tails}}{\code{FALSE} when the density at an end of the scan is
#'     still above 5\% of its peak: the tail is heavy there, and the
#'     skew-normal extrapolates it.}
#'   \item{\code{ridge_bend}}{The log-density, in nats, that the straight scan
#'     misses at 2 posterior SDs either side of the mode, where the ridge of
#'     high posterior density bends away from it. A large value means that
#'     the marginal may be too narrow: 0.25 is a posterior SD about 10\% too
#'     small.}
#'   \item{\code{grad_num}, \code{grad_diff}, \code{vb_mcse_sigma}}{Deprecated,
#'     and always \code{NA}: \code{grad_gap} checks the analytic gradient in
#'     place of \code{grad_num} and \code{grad_diff}, and the VB shift has no
#'     quadrature error. They will be removed in INLAvaan 0.5.0.}
#' }
#'
#' \strong{Fit-time warnings}: [inlavaan()] runs these checks once at the end
#' of every fit and emits a single consolidated warning (condition class
#' \code{"inlavaan_diagnostics_warning"}) when any of them looks off:
#' \itemize{
#'   \item the optimiser did not converge;
#'   \item any parameter with \code{grad_gap} above 0.1;
#'   \item \code{mode_shift_max} above 0.1 posterior SDs;
#'   \item the VB correction did not settle: \code{vb_step_max} above 0.1
#'     posterior SDs, or \code{vb_gain_ratio} below 0.5;
#'   \item any marginal with \code{nmad} above 0.1;
#'   \item any marginal with \code{unscanned_mass} above 0.05;
#'   \item any marginal with heavy tails;
#'   \item \code{vb_shift_max} above 1 posterior SD;
#'   \item \code{hess_cond_cor} above 1e8;
#'   \item the latent or residual covariance matrix is not positive definite
#'     at the posterior estimates (the check that \pkg{lavaan} runs after a
#'     fit);
#'   \item \code{psi_npd_share} or \code{theta_npd_share} above 0.01.
#' }
#' When the optimiser did not converge, the warning says what to try: to raise
#' \code{mode_maxit} or \code{mode_maxeval} in [inlavaan_control()] when it
#' stopped at one of these limits, and other starting values otherwise.
#' When \code{mode_shift_max} is above 1, or a marginal puts more than 0.9 of
#' its mass outside its scan, the warning also says that the mode may be far
#' from the posterior mass. The ridge bend and the mean gap never warn.
#' Three of these checks have calibration behind them. Over 1,046 simulated
#' fits with MCMC references, the Spearman correlation with the worst-case
#' posterior-mean discrepancy was 0.64 for the VB shift in SD units, 0.59 for
#' the unscanned mass and 0.48 for the NMAD. The other checks are rules of
#' thumb. Every number above is a package default, chosen so that a healthy
#' fit stays silent, and not a calibrated cut-off: a tripped check is a prompt
#' to look again, and silence is not a certificate of accuracy. Silence the
#' check with \code{suppressWarnings()}, or selectively by handling the
#' condition class.
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
#'             test = "none", silent = TRUE)
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
    int <- check_fit_layout(object@external$inlavaan_internal, "diagnostics")
    dg <- diagnostics_fit(int)
    if (type == "global") {
      dg$global
    } else {
      dg$param
    }
  }
)

# Shared computation behind diagnostics() and the fit-time warning; works on
# the internal list so it can run before the S4 object exists. The engine's
# diagnostics_internal() computes the numbers from the engine's record of the
# fit, with the VB mean as `pars`, as INLAvaan's own did. INLAvaan appends
# nsamp and the shares of draws with a latent or residual covariance matrix
# that is not positive definite, and then the names deprecated in 0.4.0, which
# are NA. The engine's list itself is returned too, as `engine`, for
# diagnostic_problems().
diagnostics_fit <- function(int) {
  ed <- int$engine$diagnostics
  dg <- diagnostics_internal(
    ed$optim,
    ed$vb,
    ed$marginals,
    pars = int$theta_star
  )
  # Fits saved before this check existed carry no cov_pd
  cov_pd <- int$cov_pd
  npd_share <- function(mm) {
    if (is.null(cov_pd)) NA_real_ else cov_pd$npd_share[[mm]]
  }

  global <- c(
    unclass(dg$global),
    nsamp = int$nsamp,
    psi_npd_share = npd_share("psi"),
    theta_npd_share = npd_share("theta"),
    # Deprecated in 0.4.0, to be removed in 0.5.0
    vb_mcse_max = NA_real_,
    vb_mcse_mean = NA_real_
  )
  class(global) <- c("diagnostics.INLAvaan", "numeric")

  param <- dg$param
  class(param) <- "data.frame"
  # Deprecated in 0.4.0, to be removed in 0.5.0
  param$grad_num <- NA_real_
  param$grad_diff <- NA_real_
  param$vb_mcse_sigma <- NA_real_
  class(param) <- c("diagnostics.INLAvaan.param", "data.frame")

  list(global = global, param = param, engine = dg)
}

# Positive definiteness of the latent (psi) and residual (theta) covariance
# matrices of every block. Each tanh map keeps one correlation in (-1, 1), but
# not a block of several correlations jointly positive definite, and the
# likelihood guards only the model-implied covariance matrix. This checks the
# posterior estimates x_est, as lavaan's post-fit check does, and the share of
# posterior draws x_samp in which the matrix is not positive definite. Only
# matrices with a free off-diagonal element are checked, since a diagonal
# matrix of variances is always positive (semi-)definite. The tolerance is
# lavaan's, so a singular matrix with a variance fixed at zero passes.
cov_pd_check <- function(x_est, x_samp, lavmodel) {
  out <- list(
    est_ok = c(psi = TRUE, theta = TRUE),
    npd_share = c(psi = 0, theta = 0)
  )
  mats <- names(lavmodel@GLIST)
  blk <- rep(seq_len(lavmodel@nblocks), times = lavmodel@nmat)
  check_idx <- which(
    mats %in%
      c("psi", "theta") &
      vapply(
        seq_along(mats),
        function(k) {
          ij <- arrayInd(lavmodel@m.free.idx[[k]], dim(lavmodel@GLIST[[k]]))
          any(ij[, 1] != ij[, 2])
        },
        logical(1)
      )
  )
  if (length(check_idx) == 0L) {
    return(out)
  }

  is_psd <- function(mat) {
    eig <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
    all(is.finite(eig)) &&
      !any(eig < -.Machine$double.eps^(3 / 4) * max(1, abs(eig[1L])))
  }
  # For each checked matrix, whether it is positive definite at x
  pd_at <- function(x) {
    glist <- lavaan::lav_model_set_parameters(lavmodel, as.numeric(x))@GLIST
    vapply(
      check_idx,
      function(k) {
        mat <- glist[[k]]
        if (mats[k] == "theta") {
          num_idx <- lavmodel@num.idx[[blk[k]]]
          mat <- mat[num_idx, num_idx, drop = FALSE]
        }
        nrow(mat) == 0L || is_psd(mat)
      },
      logical(1)
    )
  }

  check_mats <- mats[check_idx]
  ok_est <- if (is.null(x_est)) rep(TRUE, length(check_idx)) else pd_at(x_est)
  ok_samp <- matrix(
    apply(x_samp, 1, pd_at),
    nrow = length(check_idx)
  )
  for (mm in unique(check_mats)) {
    rows <- check_mats == mm
    out$est_ok[[mm]] <- all(ok_est[rows])
    out$npd_share[[mm]] <- mean(!apply(ok_samp[rows, , drop = FALSE], 2, all))
  }
  out
}

# The wording of the engine's diagnostic_problems() for INLAvaan's users (see
# advice_entries in R/vanilla-diagnostics.R): the arguments of inlavaan() that
# act on each problem, and, in `help`, the help page that explains the checks.
# INLAvaan always gives the engine the analytic gradient, so `no_grad`, the
# advice for a fit without one, is empty, and the engine then gives none.
inlavaan_advice <- list(
  grad_gap = "The analytic gradient does not match the log-posterior",
  no_grad = "",
  vb_full = "{.code vb_correction = \"full\"}",
  vb_none = "{.code vb_correction = \"none\"}",
  mode_limit = paste(
    "Raise {.code mode_maxit} or {.code mode_maxeval} in",
    "{.fn inlavaan_control}."
  ),
  mode_retry = paste(
    "Try other starting values, such as {.code start(1)*x1} in the model",
    "syntax, and check that the model is identified."
  ),
  funnel = paste(
    "Check that the model is identified, and that the priors keep each",
    "variance away from zero."
  ),
  help = "{.code ?diagnostics}"
)

# Fit-time diagnostics check, called once at the end of inlavaan(). Emits a
# single consolidated warning when the checks reported by diagnostics() look
# off. The bullets of the engine's checks, and their levels, come from the
# engine's diagnostic_problems(), in INLAvaan's words (inlavaan_advice).
# INLAvaan adds two checks of its own: the return code of nlminb(), which the
# engine leaves out, first, and the latent and residual covariance matrices
# that are not positive definite, last. Only the "x" bullets count as issues:
# the engine adds "i" lines with advice.
warn_fit_diagnostics <- function(int, npd_share_tol = 0.01) {
  dg <- diagnostics_fit(int)
  glob <- dg$global
  gaussian <- int$marginal_method != "skewnorm"
  # A fit made before has_grad was recorded had a gradient
  problems <- diagnostic_problems(
    dg$engine,
    gaussian = gaussian,
    has_grad = !isFALSE(int$engine$has_grad),
    advice = inlavaan_advice
  )
  # The templates of the bullets are filled in from this environment
  env <- attr(problems, "envir")

  issues <- character(0)
  if (glob[["converged"]] == 0) {
    # When the mode is flagged too, the engine's bullet on the mode gives the
    # message of nlminb() and the advice, so they are not repeated here.
    msg <- dg$engine$message
    mode_flagged <- check_flags(dg$engine, gaussian)[["mode"]]
    env$conv_message <- msg
    issues <- c(
      "x" = if (!is.null(msg) && nzchar(msg) && !mode_flagged) {
        "The optimiser did not converge: {conv_message}."
      } else {
        "The optimiser did not converge."
      }
    )
    if (!mode_flagged) {
      issues <- c(
        issues,
        "i" = if (isTRUE(grepl("limit", msg))) {
          inlavaan_advice$mode_limit
        } else {
          inlavaan_advice$mode_retry
        }
      )
    }
  }
  issues <- c(issues, problems)

  cov_labels <- c(
    psi = "covariance matrix of the latent variables ({.code psi})",
    theta = "residual covariance matrix ({.code theta})"
  )
  est_ok <- int$cov_pd$est_ok
  if (is.null(est_ok)) {
    est_ok <- c(psi = TRUE, theta = TRUE)
  }
  for (mm in names(est_ok)[!est_ok]) {
    issues <- c(
      issues,
      "x" = paste0(
        "The ",
        cov_labels[[mm]],
        " is not positive definite at the posterior estimates."
      )
    )
  }
  npd_share <- c(
    psi = glob[["psi_npd_share"]],
    theta = glob[["theta_npd_share"]]
  )
  for (mm in names(npd_share)[which(npd_share > npd_share_tol)]) {
    issues <- c(
      issues,
      "x" = paste0(
        "The ",
        cov_labels[[mm]],
        " is not positive definite in ",
        formatC(100 * npd_share[[mm]], digits = 1, format = "f"),
        "% of the posterior draws; summaries computed from the draws include
         them."
      )
    )
  }

  env$n_issues <- sum(names(issues) == "x")
  if (env$n_issues > 0L) {
    cli_warn(
      c(
        "Fit diagnostics flagged {n_issues} potential issue{?s}:",
        issues,
        "i" = paste0(
          "Inspect with {.code diagnostics(fit)} and ",
          "{.code diagnostics(fit, type = \"param\")}; ",
          inlavaan_advice$help,
          " explains each check."
        )
      ),
      class = "inlavaan_diagnostics_warning",
      .envir = env
    )
  }
  invisible(issues)
}

#' @exportS3Method print diagnostics.INLAvaan
print.diagnostics.INLAvaan <- function(x, ...) {
  nm <- names(x)
  int_names <- c(
    "npar",
    "nsamp",
    "converged",
    "iterations",
    "vb_applied",
    "vb_fallback",
    "n_heavy_tails"
  )
  # Quantities that span orders of magnitude, or sit far below the four
  # decimals of the fixed format, are printed in scientific notation.
  sci_names <- c(
    "hess_cond",
    "hess_cond_cor",
    "mode_shift_max",
    "unscanned_mass_max"
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
  # The logical column tails is left as it is
  num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  for (nm in num_cols) {
    # A healthy unscanned mass is smaller than the last of the four decimals,
    # so round() would print it as zero.
    x[[nm]] <- if (nm == "unscanned_mass") {
      signif(x[[nm]], 3)
    } else {
      round(x[[nm]], digits)
    }
  }
  print.data.frame(x, ...)
  invisible(x)
}
