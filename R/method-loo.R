#' Approximate Leave-One-Out Cross-Validation for INLAvaan Models
#'
#' Computes leave-one-out (LOO) cross-validation for a fitted [INLAvaan]
#' model from a single fit, with no refitting and no sampling, via a Taylor
#' approximation of the case-deletion posterior around the Laplace summary.
#' Single-level models are scored per subject (leave-one-subject-out, LOSO);
#' two-level models are scored per cluster (leave-one-cluster-out, LOCO).
#'
#' @details
#' For a unit \eqn{u} (a subject for LOSO, a cluster for LOCO) with
#' log-likelihood contribution \eqn{\ell_u(\theta)}, score \eqn{s_u} and Hessian
#' \eqn{H_u} evaluated at the posterior summary \eqn{(\theta^*, \Omega)}, the
#' log conditional predictive ordinate is approximated to first and second order
#' by
#' \deqn{\log \mathrm{CPO}_u^{(1)} = \ell_u - \tfrac{1}{2} s_u' \Omega s_u,}
#' \deqn{\log \mathrm{CPO}_u^{(2)} = \ell_u
#'   - \tfrac{1}{2} s_u' (\Omega^{-1} + H_u)^{-1} s_u
#'   + \tfrac{1}{2} \log |I + \Omega H_u|.}
#' The reported `elpd_loo` is the sum of the second-order terms (first-order
#' when `second_order = FALSE`), with standard error
#' \eqn{\sqrt{n \, \mathrm{var}(\log \mathrm{CPO}_u)}} and `looic`
#' \eqn{= -2 \, \mathrm{elpd}}. `p_loo` is the **loo** package's effective
#' number of parameters,
#' \eqn{p_{\mathrm{loo}} = \sum_u (\mathrm{lpd}_u - \log \mathrm{CPO}_u)}, where
#' \eqn{\mathrm{lpd}_u} is the analogous Taylor approximation of the
#' full-posterior pointwise log predictive density -- the same definition
#' `loo::loo()` reports, and *not* the \eqn{p_D} of the DIC.
#'
#' \eqn{\log \mathrm{CPO}_u^{(2)}} exists exactly when \eqn{\Omega^{-1} + H_u}
#' is positive definite (recorded in `per_unit$ok`) and
#' \eqn{\mathrm{lpd}_u^{(2)}} exactly when \eqn{\Omega^{-1} - H_u} is. A unit
#' failing the former drops every estimate to first order over all units (with a
#' warning), while one failing only the latter contributes its first-order
#' difference to `p_loo`.
#'
#' The leverages `k_max`, `k_min`, and `k_sum` read these conditions off the
#' spectrum of \eqn{-\Omega H_u} (`k_max < 1`, `k_min > -1`), with `k_sum`
#' summing across units to the trace form of \eqn{p_D}. `p_loo` (cross-product
#' form) and \eqn{p_D} (second-derivative form) agree only in the
#' correct-specification limit, and printing a result reports the
#' first-to-second-order gap against its limit \eqn{p_D/2} as a free check on
#' the Taylor truncation. Because the first-order elpd overstates the truth by
#' \eqn{\tfrac12 p_D} in the limit, keep `second_order = TRUE` whenever models
#' of different dimension are compared.
#'
#' `type = "auto"` resolves to the marginal per-cluster `"loco"` for two-level
#' fits and per-subject `"loso"` otherwise. Forcing `"loso"` on a two-level
#' model scores the *conditional* predictive of Merkle, Furr & Rabe-Hesketh
#' (2019) instead, and warns. Multigroup units are scored against their own
#' group's implied moments and identified by case number, so [compare()] pairs
#' them across fits.
#'
#' The score follows the fitted likelihood's treatment of exogenous
#' covariates, i.e. joint under `fixed.x = FALSE`, conditional under
#' `fixed.x = TRUE` (recorded as `"joint"` or `"conditional"` in the result's
#' `flavour` field), and the two flavours are never comparable ([compare()]
#' refuses to mix them).
#'
#' Supplying `theta`/`Omega` evaluates the LOO at an arbitrary Gaussian
#' posterior summary (a singular `Omega` is restricted to its non-degenerate
#' block), the building block for refit-free submodel scoring. `Sigma` is
#' the deprecated former name of `Omega`, still accepted through `...` with
#' a warning.
#'
#' Under the default `test = "standard"` the LOO is computed and stored at fit
#' time when the model is supported and the predicted cost fits a 10-second
#' budget (`test = "loo"` forces it, `add_loo()` stores it post hoc), and
#' `loo(fit)` with default arguments returns the stored result. 
#' 
#' The default `cores = NULL` runs serially, and `cores > 1` parallelises the 
#' Hessian stage. Supported models are continuous-indicator models fitted with 
#' the `ML` estimator, single- or two-level, single-group or multigroup 
#' (multigroup two-level models are not supported yet).
#'
#' @param x A fitted [INLAvaan] object (or its `inlavaan_internal` list).
#' @param type Unit type: `"auto"` (default) resolves to `"loso"`
#'   (per-subject) for single-level models and `"loco"` (per-cluster,
#'   marginal predictive) for two-level models. `"loco"` cannot be forced
#'   on a model without clusters; `"loso"` on a two-level model scores the
#'   conditional (leave-one-unit-out) predictive instead (with a warning;
#'   see Details).
#' @param units Optional integer vector of unit indices to score; defaults
#'   to all units. For LOSO these are case numbers (row numbers of the
#'   analysed dataset); for LOCO, cluster positions.
#' @param second_order Logical; compute the second-order correction
#'   (default `TRUE`). `FALSE` skips the Hessian stage and reports
#'   first-order estimates, which cannot be compared across models of
#'   different dimension (see Details).
#' @param theta,Omega Optional posterior mean vector and covariance matrix
#'   (in the unconstrained parameter space, as stored in `theta_star` and
#'   `Sigma_theta`) at which to evaluate the LOO instead of the fit's own
#'   Laplace summary. See Details.
#' @param cores Number of cores for the Hessian stage. The default `NULL`
#'   runs serially; parallelism must be requested explicitly.
#' @param verbose Logical; print progress (default `FALSE`).
#' @param ... Not used, beyond catching the deprecated argument name
#'   `Sigma` (see `Omega`).
#'
#' @returns An object of class `inlavaan_loo`: a list with elements
#'   \describe{
#'     \item{`per_unit`}{Data frame of pointwise results, one row per
#'       unit:
#'       \describe{
#'         \item{`unit`}{Case number for LOSO, cluster position for LOCO.}
#'         \item{`group`}{Group membership (multigroup fits only).}
#'         \item{`nobs`}{1 for LOSO, the cluster size for LOCO.}
#'         \item{`l_star`}{Unit log-likelihood at the summary.}
#'         \item{`score_norm`}{Norm of the unit score \eqn{s_u}.}
#'         \item{`lpd_1`, `lpd_2`}{Pointwise log predictive density, at
#'           first and second order.}
#'         \item{`log_cpo_1`, `log_cpo_2`}{Pointwise LOO contributions,
#'           at first and second order.}
#'         \item{`det_term`}{\eqn{\tfrac12 \log |I + \Omega H_u|}, the
#'           determinant term of the second-order score.}
#'         \item{`k_max`, `k_min`, `k_sum`}{Leverage diagnostics (see
#'           Details).}
#'         \item{`k_ssq`}{\eqn{\mathrm{tr}[(\Omega H_u)^2]}, consumed by
#'           the closed-form [waic()] penalty.}
#'         \item{`ok`}{Whether the second-order \eqn{\log \mathrm{CPO}}
#'           exists.}
#'       }}
#'     \item{`estimates`}{Matrix with rows `elpd_loo`, `p_loo`, `looic` and
#'       columns `Estimate`, `SE`, at the highest order available to each.}
#'     \item{`elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`}{
#'       First- and second-order aggregates; the second-order ones are `NA`
#'       when any \eqn{\log \mathrm{CPO}_u^{(2)}} does not exist.}
#'     \item{`elpd_gap`, `pd_trace`}{The two sides of the curvature check
#'       (see Details); both are `NA` at first order and partial totals
#'       under a `units` subset.}
#'     \item{`type`, `flavour`, `n_units`, `n_groups`, `n_ok`, `n_lpd_ok`,
#'       `second_order`, `use_second`, `theta_overridden`}{Metadata.}
#'   }
#'
#' @references
#' Alhyari, M., Jamil, H., Montcho, H., & Rue, H. (2026). *Deterministic
#' leave-one-cluster-out cross-validation for multilevel Bayesian structural
#' equation models*. arXiv. (Preprint forthcoming; placeholder.)
#'
#' Merkle, E. C., Furr, D., & Rabe-Hesketh, S. (2019). Bayesian comparison of
#' latent variable models: Conditional versus marginal likelihoods.
#' *Psychometrika*, *84*(3), 802--829.
#' <https://doi.org/10.1007/s11336-019-09679-0>
#'
#' @seealso [fitmeasures()], [compare()], [inlavaan()]
#'
#' @example inst/examples/ex-loo.R
#'
#' @export
loo <- function(x, ...) {
  UseMethod("loo")
}

#' @rdname loo
#' @exportS3Method loo INLAvaan
loo.INLAvaan <- function(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  theta = NULL,
  Omega = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  loo.inlavaan_internal(
    x@external$inlavaan_internal,
    type = type,
    units = units,
    second_order = second_order,
    theta = theta,
    Omega = Omega,
    cores = cores,
    verbose = verbose,
    ...
  )
}

#' @rdname loo
#' @exportS3Method loo inlavaan_internal
loo.inlavaan_internal <- function(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  theta = NULL,
  Omega = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  type <- match.arg(type)
  Omega <- resolve_deprecated_Sigma(Omega, ...)

  # Reuse a stored result (computed at fit time via test = "loo", or with
  # add_loo()) when no argument deviates from the defaults
  all_defaults <- type == "auto" &&
    is.null(units) &&
    isTRUE(second_order) &&
    is.null(theta) &&
    is.null(Omega)
  if (all_defaults && !is.null(x$loo)) {
    if (isTRUE(verbose)) {
      cli_alert_info("Returning the LOO result stored with the fit.")
    }
    return(x$loo)
  }

  inlav_loo(
    int = x,
    type = type,
    units = units,
    second_order = second_order,
    theta = theta,
    Sigma = Omega,
    eff_cores = resolve_loo_cores(cores),
    verbose = verbose
  )
}

# The covariance argument was renamed from Sigma to Omega, the notation of the
# accompanying manuscript. A legacy `Sigma =` no longer matches a formal and
# lands in ..., where it is honoured so existing scripts keep running, with a
# deprecation warning.
resolve_deprecated_Sigma <- function(Omega, ...) {
  dots <- list(...)
  if (!"Sigma" %in% names(dots)) {
    return(Omega)
  }
  if (!is.null(Omega)) {
    cli_abort(
      "Supply only {.arg Omega}; {.arg Sigma} is its deprecated former name."
    )
  }
  cli_warn(
    "The {.arg Sigma} argument of {.fn loo} is deprecated; use {.arg Omega}
     instead.",
    class = "inlavaan_deprecated_sigma"
  )
  dots$Sigma
}

#' @rdname loo
#' @param object A fitted [INLAvaan] object, or an `inlavaan_loo` result for
#'   `summary()`.
#' @returns `summary()` is an alias for `print()`: it prints the same output
#'   and returns the result invisibly.
#' @returns `add_loo()` returns a copy of `object` with the LOO result
#'   stored alongside the fit (the input object is unchanged); reassign it,
#'   e.g. `fit <- add_loo(fit)`. Only the default LOO is stored, so the
#'   stored result always matches `loo(fit)`.
#' @export
add_loo <- function(object, cores = NULL, verbose = FALSE) {
  if (!is_INLAvaan(object)) {
    cli_abort("{.arg object} must be a fitted {.cls INLAvaan} model.")
  }
  res <- loo(object, cores = cores, verbose = verbose)
  object@external$inlavaan_internal$loo <- res
  object
}

#' @rdname loo
#' @exportS3Method print inlavaan_loo
print.inlavaan_loo <- function(x, ...) {
  label <- switch(
    x$type,
    loso = "Leave-one-subject-out",
    loco = "Leave-one-cluster-out"
  )
  unit_word <- switch(x$type, loso = "subject", loco = "cluster")
  loo_cat_rule(label, loo_rule_label(x, unit_word))
  cat("\n")
  print(round(x$estimates, 1))
  # A missing log CPO term already warned at computation time, and the rule
  # above records the order, so repeating it here would only duplicate. The
  # lpd substitution has no warning, so this is the only place it is said.
  if (isTRUE(x$use_second) && x$n_lpd_ok < x$n_units) {
    n <- x$n_units
    n_bad <- n - x$n_lpd_ok
    cat("\n")
    loo_cat_note(pluralize(
      "p_loo uses first-order contributions for {n_bad} of {n} units with no
       second-order lpd; elpd_loo and looic are unaffected."
    ))
  }
  if (isTRUE(x$theta_overridden)) {
    cat("\n")
    loo_cat_note("Evaluated at a user-supplied (theta, Sigma) summary.")
  }
  loo_print_curvature(x)
  invisible(x)
}

#' @rdname loo
#' @method summary inlavaan_loo
#' @exportS3Method summary inlavaan_loo
summary.inlavaan_loo <- function(object, ...) {
  print(object, ...)
}

# Rule header and grey note, on stdout. cli's cli_rule()/cli_alert_info()
# signal a condition that the default handler writes to stderr, which a print
# method must not do (and which capture.output()/expect_output() would miss),
# so the string-returning rule() and format_message() are used and cat() picks
# the connection. format_message() still reflows to the console width.
loo_cat_rule <- function(left, right) {
  cat(rule(left = left, right = right), "\n", sep = "")
}

loo_cat_note <- function(text) {
  writeLines(col_grey(format_message(c("i" = text))))
}

# Right-hand label of a rule header: how many units the result covers, in how
# many groups, and at which Taylor order. Shared by the LOO and WAIC prints.
# qty(n) is needed because the length-1 unit_word would otherwise set the
# quantity that {?s} reads.
loo_rule_label <- function(x, unit_word) {
  n <- x$n_units
  paste0(
    pluralize("{n} {unit_word}{qty(n)}{?s}"),
    if (!is.null(x$n_groups) && x$n_groups > 1L) {
      paste0(" in ", x$n_groups, " groups")
    },
    ", ",
    if (isTRUE(x$use_second)) "second-order" else "first-order"
  )
}

# The curvature check: the summed first-to-second-order gap against pD/2, the
# limit it approaches from above. Both sides are read off the same Laplace
# summary, so they carry the same Laplace error and much of it cancels, leaving
# the truncation error the check is after -- which is why the trace route to
# p_D is used here rather than the sampled pD that summary() prints. Nothing is
# thresholded: as everywhere else in loo(), existence is the only condition the
# package acts on, so the excess is reported and the reading is left to the
# user.
loo_print_curvature <- function(x) {
  gap <- x$elpd_gap %||% NA_real_
  pd_trace <- x$pd_trace %||% NA_real_
  if (!isTRUE(x$use_second) || !is.finite(gap) || !is.finite(pd_trace)) {
    return(invisible(NULL))
  }
  cat("\n")
  loo_cat_rule("Curvature check", "")
  cat("\n")
  row <- function(lab, val) cat(sprintf("  %-27s %9s\n", lab, val))
  row("first-to-second-order gap", sprintf("%.1f", gap))
  row("pD/2 (trace)", sprintf("%.1f", pd_trace / 2))
  # A non-positive trace has no scale to take an excess against; the two totals
  # still say what they say.
  if (pd_trace > 0) {
    row(
      "excess over pD/2 (trace)",
      sprintf("%+.1f%%", 100 * (gap / (pd_trace / 2) - 1))
    )
    cat("\n")
    loo_cat_note(
      "The gap approaches pD/2 (trace) from above. A large excess says the
       second-order expansion has not settled over the sample."
    )
  }
  invisible(NULL)
}
