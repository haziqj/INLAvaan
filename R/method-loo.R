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
#' log-likelihood contribution \eqn{\ell_u(\theta)}, score \eqn{s_u} and
#' Hessian \eqn{H_u} evaluated at the posterior summary
#' \eqn{(\theta^*, \Sigma)}, the log conditional predictive ordinate is
#' approximated to first and second order by
#' \deqn{\log \mathrm{CPO}_u^{(1)} = \ell_u - \tfrac{1}{2} s_u' \Sigma s_u,}
#' \deqn{\log \mathrm{CPO}_u^{(2)} = \ell_u
#'   - \tfrac{1}{2} s_u' (\Sigma^{-1} + H_u)^{-1} s_u
#'   + \tfrac{1}{2} \log |I + \Sigma H_u|.}
#' The reported `elpd_loo` is the sum of the second-order terms (first-order
#' when `second_order = FALSE`), with standard error
#' \eqn{\sqrt{n \, \mathrm{var}(\log \mathrm{CPO}_u)}} and
#' `looic` \eqn{= -2 \, \mathrm{elpd}}. The effective number of parameters is
#' \eqn{p_{\mathrm{loo}} = \sum_u (\mathrm{lpd}_u - \log \mathrm{CPO}_u)},
#' where \eqn{\mathrm{lpd}_u} is the analogous Taylor approximation of the
#' full-posterior pointwise log predictive density. Units whose second-order
#' curvature matrix is not positive definite fall back to first order for
#' that unit only (flagged in `per_unit$ok`).
#'
#' The type is resolved automatically: per-cluster (`"loco"`) when the model
#' was fitted with a `cluster` argument, per-subject (`"loso"`) otherwise.
#' On a two-level model, `type = "loso"` may be forced (with a warning) as a
#' diagnostic: row \eqn{i} of cluster \eqn{j} then contributes
#' \eqn{\ell_i = \ell_j(\mathrm{full}) - \ell_j(\mathrm{minus\ row\ } i)},
#' the conditional density of the row given the remaining rows in its
#' cluster, computed by downdating the cluster's sufficient statistics. This
#' path costs one cluster evaluation per row per Hessian direction and is
#' less extensively validated than the per-cluster default.
#'
#' Supplying `theta` and/or `Sigma` scores the model at an *arbitrary*
#' Gaussian posterior summary instead of the fit's own, without refitting.
#' This is the building block for refit-free model exploration: for example,
#' conditioning the encompassing model's summary on a parameter being zero
#' (a rank-one update of `theta` and `Sigma`) and scoring the result gives
#' the LOO of that submodel from a single fit. INLAvaan provides only this
#' evaluation API; search strategies are left to the user. A conditioned
#' `Sigma` may be singular; the computation automatically restricts to the
#' non-degenerate block, which is exact.
#'
#' Parallelism is strictly opt-in: the default `cores = NULL` runs serially,
#' and `cores > 1` parallelises the Hessian stage via forking (not available
#' on Windows).
#'
#' Calling `loo()` never modifies the fitted object. Under the default
#' `test = "standard"`, [inlavaan()] already computes and stores the full
#' LOO at fit time whenever the model is supported, has a mean structure,
#' and the predicted serial cost is within a 10-second budget (measured by
#' timing one score evaluation); `test = "loo"` forces the computation
#' regardless of the budget, and `fit <- add_loo(fit)` stores it post hoc.
#' A stored result is returned directly by `loo(fit)` when called with
#' default arguments, and is reused by [fitmeasures()] and [compare()]
#' without recomputation.
#'
#' **Exogenous covariates.** The flavour of the score follows the fitted
#' likelihood. Under `fixed.x = FALSE` the covariates are modelled jointly
#' and each unit is scored by the joint predictive density of its outcomes
#' *and* covariates (`flavour = "joint"`). Under `fixed.x = TRUE` (the
#' lavaan default) the fitted likelihood is the conditional one, and each
#' unit is scored by the predictive density of its outcomes *given* its
#' covariates (`flavour = "conditional"`); since the conditional likelihood
#' is exactly invariant to the frozen covariate moments, this involves no
#' additional approximation. The two flavours estimate different quantities
#' whose scales differ by the covariate predictive density, so a joint and a
#' conditional elpd must never be compared ([compare()] refuses
#' mixed-flavour comparisons). Conditional scores of models conditioning on
#' *different* covariate sets are comparable provided the outcome variables
#' match -- the natural setting for covariate selection. For two-level
#' models the conditional flavour currently requires every exogenous
#' covariate to be a cluster-level (between) variable.
#'
#' Supported models: single-group, complete-data, continuous-indicator
#' models fitted with the `ML` estimator. If the `loo` package is attached
#' it masks this generic, but `loo(fit)` continues to dispatch correctly
#' because the method is registered by generic name.
#'
#' @param x A fitted [INLAvaan] object (or its `inlavaan_internal` list).
#' @param type Unit type: `"auto"` (default) resolves to `"loso"`
#'   (per-subject) for single-level models and `"loco"` (per-cluster) for
#'   two-level models. `"loco"` cannot be forced on a model without
#'   clusters; `"loso"` on a two-level model scores row deletions (see
#'   Details) and emits a warning.
#' @param units Optional integer vector of unit indices (row numbers for
#'   LOSO, cluster positions for LOCO) to score; defaults to all units.
#' @param second_order Logical; compute the second-order correction
#'   (default `TRUE`). `FALSE` skips the Hessian stage entirely and reports
#'   first-order estimates.
#' @param theta,Sigma Optional posterior mean vector and covariance matrix
#'   (in the unconstrained parameter space, as stored in `theta_star` and
#'   `Sigma_theta`) at which to evaluate the LOO instead of the fit's own
#'   Laplace summary. See Details.
#' @param cores Number of cores for the Hessian stage. The default `NULL`
#'   runs serially; parallelism must be requested explicitly.
#' @param verbose Logical; print progress (default `FALSE`).
#' @param ... Not used.
#'
#' @returns An object of class `inlavaan_loo`: a list with elements
#'   \describe{
#'     \item{`per_unit`}{Data frame of pointwise results: `unit`, `nobs`
#'       (1 for LOSO, the cluster size for LOCO), `l_star` (unit
#'       log-likelihood at the summary), `score_norm`, `lpd_1`/`lpd_2`
#'       (pointwise log predictive density), `log_cpo_1`/`log_cpo_2`
#'       (pointwise LOO contributions), `det_term`, and `ok` (second-order
#'       success flag).}
#'     \item{`estimates`}{Matrix with rows `elpd_loo`, `p_loo`, `looic` and
#'       columns `Estimate`, `SE` (headline second-order values).}
#'     \item{`elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`}{
#'       First- and second-order aggregates.}
#'     \item{`type`, `flavour`, `n_units`, `n_ok`, `second_order`,
#'       `theta_overridden`}{Metadata; `flavour` records whether units were
#'       scored jointly with their covariates (`"joint"`) or conditionally
#'       on them (`"conditional"`, for `fixed.x` fits).}
#'   }
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
  Sigma = NULL,
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
    Sigma = Sigma,
    cores = cores,
    verbose = verbose
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
  Sigma = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  type <- match.arg(type)

  # Reuse a stored result (computed at fit time via test = "loo", or with
  # add_loo()) when no argument deviates from the defaults
  all_defaults <- type == "auto" &&
    is.null(units) &&
    isTRUE(second_order) &&
    is.null(theta) &&
    is.null(Sigma)
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
    Sigma = Sigma,
    eff_cores = resolve_loo_cores(cores),
    verbose = verbose
  )
}

#' @rdname loo
#' @param object A fitted [INLAvaan] object.
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

#' @exportS3Method print inlavaan_loo
print.inlavaan_loo <- function(x, ...) {
  label <- switch(
    x$type,
    loso = "leave-one-subject-out",
    loco = "leave-one-cluster-out"
  )
  unit_word <- switch(x$type, loso = "subject", loco = "cluster")
  order_lab <- if (x$second_order && x$n_ok > 0L) {
    "second-order"
  } else {
    "first-order"
  }
  cat("Taylor ", label, " cross-validation (INLAvaan)\n", sep = "")
  cat(
    "Computed from ",
    x$n_units,
    " ",
    unit_word,
    if (x$n_units != 1L) "s",
    " (",
    order_lab,
    " Taylor approximation)\n",
    sep = ""
  )
  if (identical(x$flavour, "conditional")) {
    cat("Scored conditionally on the exogenous covariates (fixed.x fit)\n")
  }
  cat("\n")
  print(round(x$estimates, 1))
  if (x$second_order && x$n_ok < x$n_units) {
    cat(
      "\n",
      x$n_units - x$n_ok,
      " of ",
      x$n_units,
      " units fell back to first order (non-positive-definite curvature).\n",
      sep = ""
    )
  }
  if (isTRUE(x$theta_overridden)) {
    cat("\nEvaluated at a user-supplied (theta, Sigma) summary.\n")
  }
  invisible(x)
}
