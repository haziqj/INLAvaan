#' Diagnostics for INLAvaan Models
#'
#' Extract convergence and approximation-quality diagnostics from a fitted
#' INLAvaan model.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. `"global"` (default) returns a named numeric vector
#'   of scalar diagnostics. `"param"` returns a data frame with one row per
#'   free parameter containing per-parameter diagnostics.
#' @param ... Currently unused.
#'
#' @details
#' **Global diagnostics** (`type = "global"`):
#' \describe{
#'   \item{`npar`}{Number of free parameters.}
#'   \item{`nsamp`}{Number of posterior samples drawn.}
#'   \item{`converged`}{1 if the optimiser converged, 0 otherwise.}
#'   \item{`iterations`}{Number of optimiser iterations.}
#'   \item{`grad_inf`}{L-infinity norm of the analytic gradient at the mode
#'     (max |grad|). Should be ~0 at convergence.}
#'   \item{`grad_inf_rel`}{Relative L-infinity norm of the analytic gradient
#'     (max |grad| / (|par| + 1e-6)).}
#'   \item{`grad_l2`}{L2 (Euclidean) norm of the analytic gradient at the mode.}
#'   \item{`hess_cond`}{Condition number of the Hessian (precision matrix)
#'     computed from \eqn{\Sigma_\theta}. Large values indicate
#'     near-singularity.}
#'   \item{`vb_kld_global`}{Global KL divergence from the VB mean
#'     correction (NA if VB correction was not applied).}
#'   \item{`vb_applied`}{1 if VB correction was applied, 0 otherwise.}
#'   \item{`kld_max`}{Maximum per-parameter KL divergence from the VB
#'     correction.}
#'   \item{`kld_mean`}{Mean per-parameter KL divergence.}
#'   \item{`nmad_max`}{Maximum normalised max-absolute-deviation across
#'     marginals (skew-normal method only; NA otherwise).}
#'   \item{`nmad_mean`}{Mean NMAD across marginals.}
#' }
#'
#' **Per-parameter diagnostics** (`type = "param"`):
#' A data frame with columns:
#' \describe{
#'   \item{`param`}{Parameter name.}
#'   \item{`grad`}{Analytic gradient of the negative log-posterior at the
#'     mode. Should be ~0 at convergence.}
#'   \item{`grad_num`}{Numerical (finite-difference) gradient at the mode.
#'     Should agree with `grad`; large discrepancies indicate a bug in the
#'     analytic gradient.}
#'   \item{`grad_diff`}{Difference `grad_num - grad`: should be ~0.}
#'   \item{`grad_abs`}{Absolute analytic gradient.}
#'   \item{`grad_rel`}{Relative analytic gradient |grad| / (|par| + 1e-6).}
#'   \item{`kld`}{Per-parameter KL divergence from the VB correction.}
#'   \item{`vb_shift`}{VB correction shift (in original scale).}
#'   \item{`vb_shift_sigma`}{VB shift in units of posterior SD.}
#'   \item{`nmad`}{Normalised max-absolute-deviation of the skew-normal fit
#'     (NA when not using the skewnorm method).}
#' }
#'
#' @returns For `type = "global"`, a named numeric vector (class
#'   `"diagnostics.INLAvaan"`). For `type = "param"`, a data frame (class
#'   `c("diagnostics.INLAvaan.param", "data.frame")`).
#'
#' @rdname INLAvaan-class
#' @export
setGeneric("diagnostics", function(object, ...) standardGeneric("diagnostics"))

#' @rdname INLAvaan-class
#' @aliases diagnostics,INLAvaan-method
#' @export
setMethod(
  "diagnostics",
  "INLAvaan",
  function(object, type = c("global", "param"), ...) {
    type <- match.arg(type)
    int <- object@external$inlavaan_internal

    grad_analytic <- int$opt$dx_analytic
    grad_num      <- int$opt$dx
    pars <- int$theta_star
    parnames <- names(int$coefficients)
    m <- length(parnames)

    Sigma_theta <- int$Sigma_theta

    se_laplace <- sqrt(diag(Sigma_theta))

    # VB diagnostics
    vb <- int$vb
    vb_applied <- all(!is.na(vb$correction))
    vb_shift <- if (vb_applied) vb$correction else rep(NA_real_, m)
    vb_kld <- if (vb_applied) vb$kld else rep(NA_real_, m)
    vb_shift_sigma <- vb_shift / se_laplace

    # NMAD (skewnorm method only)
    nmad <- tryCatch(
      int$approx_data[seq_len(m), "nmad"],
      error = function(e) rep(NA_real_, m)
    )
    if (is.null(nmad) || length(nmad) == 0) {
      nmad <- rep(NA_real_, m)
    }

    # Hessian condition number: kappa(H) = kappa(Sigma_theta)
    eig <- eigen(Sigma_theta, symmetric = TRUE, only.values = TRUE)$values
    hess_cond <- max(eig) / min(eig)

    if (type == "global") {
      out <- c(
        npar = m,
        nsamp = int$nsamp,
        converged = as.numeric(object@optim$converged),
        iterations = object@Fit@iterations,
        grad_inf = max(abs(grad_analytic)),
        grad_inf_rel = max(abs(grad_analytic) / (abs(pars) + 1e-06)),
        grad_l2 = sqrt(sum(grad_analytic^2)),
        hess_cond = hess_cond,
        vb_applied = as.numeric(vb_applied),
        vb_kld_global = if (vb_applied) vb$kld_global else NA_real_,
        kld_max = max(vb_kld, na.rm = TRUE),
        kld_mean = mean(vb_kld, na.rm = TRUE),
        nmad_max = if (all(is.na(nmad))) NA_real_ else max(nmad, na.rm = TRUE),
        nmad_mean = if (all(is.na(nmad))) NA_real_ else mean(nmad, na.rm = TRUE)
      )
      class(out) <- c("diagnostics.INLAvaan", "numeric")
      out
    } else {
      df <- data.frame(
        names = parnames,
        grad = grad_analytic,
        grad_num = grad_num,
        grad_diff = grad_num - grad_analytic,
        grad_abs = abs(grad_analytic),
        grad_rel = abs(grad_analytic) / (abs(pars) + 1e-06),
        kld = vb_kld,
        vb_shift = vb_shift,
        vb_shift_sigma = vb_shift_sigma,
        nmad = nmad,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      class(df) <- c("diagnostics.INLAvaan.param", "data.frame")
      df
    }
  }
)

#' @exportS3Method print diagnostics.INLAvaan
print.diagnostics.INLAvaan <- function(x, ...) {
  nm <- names(x)
  formatted <- vapply(
    seq_along(x),
    function(i) {
      val <- x[i]
      name <- nm[i]

      if (
        name %in% c("npar", "nsamp", "converged", "iterations", "vb_applied")
      ) {
        as.character(as.integer(round(val)))
      } else if (startsWith(name, "grad_")) {
        formatC(val, digits = 2, format = "e")
      } else if (name == "hess_cond") {
        formatC(val, digits = 2, format = "e")
      } else if (is.na(val)) {
        "NA"
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
  num_cols <- sapply(x, is.numeric)
  x[, num_cols] <- round(x[, num_cols], digits)
  print.data.frame(x, ...)
  invisible(x)
}
