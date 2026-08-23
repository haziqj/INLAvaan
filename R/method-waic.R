#' Widely Applicable Information Criterion for INLAvaan Models
#'
#' Computes the WAIC of a fitted [INLAvaan] model in closed form from the
#' fit's Laplace summary -- the same per-unit Taylor quantities behind
#' [loo()], with no posterior draws and no Monte Carlo error. Single-level
#' models are scored per subject; two-level models are scored per cluster by
#' default, matching the units used by [loo()]. For a two-level model
#' `type = "loso"` instead scores the *conditional* (leave-one-unit-out)
#' WAIC; see Details.
#'
#' @details
#' Writing \eqn{\ell_u(\theta) = \log p(y_u \mid \theta)} and expanding it
#' to second order about the posterior mode, with the posterior taken as
#' \eqn{N(\theta^*, \Sigma)}, both WAIC terms are available in closed form:
#' the pointwise log predictive density \eqn{\mathrm{lpd}_u} is the same
#' Gaussian integral [loo()] computes, and the penalty is the polynomial
#' \deqn{p_{\mathrm{waic},u} = \mathrm{Var}[\ell_u(\theta)]
#'   = s_u^\top \Sigma\, s_u
#'   + \tfrac12 \mathrm{tr}\!\left[(H_u \Sigma)^2\right],}
#' with \eqn{s_u} and \eqn{H_u} the unit's score and Hessian. Then
#' \eqn{\mathrm{elpd}_{\mathrm{waic}} = \sum_u (\mathrm{lpd}_u -
#' p_{\mathrm{waic},u})} and \eqn{\mathrm{WAIC} = -2\,
#' \mathrm{elpd}_{\mathrm{waic}}}. Unlike the lpd and log CPO integrals, the
#' penalty is a variance -- a polynomial in Gaussian moments -- so it exists
#' for every unit. A unit whose second-order \eqn{\mathrm{lpd}_u} does not
#' exist (see the existence discussion in [loo()]) contributes its
#' first-order lpd instead; this substitution lands in
#' \eqn{\mathrm{elpd}_{\mathrm{waic}}} itself, so it is noted when the
#' result is printed and counted by `n_lpd_ok`. At first order the identity
#' \eqn{\mathrm{lpd}^{(1)}_u - p^{(1)}_{\mathrm{waic},u} = \log
#' \mathrm{CPO}^{(1)}_u} holds exactly: first-order WAIC and first-order
#' LOO are the same number.
#'
#' The same model restrictions as [loo()] apply, and so does the flavour
#' rule: fits with `fixed.x = TRUE` are scored conditionally on the
#' exogenous covariates, fits with `fixed.x = FALSE` jointly (see [loo()]).
#'
#' **Marginal vs conditional WAIC (two-level models).** The default
#' per-cluster scoring is the *marginal* WAIC, which corresponds to
#' leave-one-cluster-out cross-validation -- prediction for a *new* cluster.
#' Setting `type = "loso"` scores the *conditional* WAIC, corresponding to
#' leave-one-unit-out -- prediction for a new observation within an
#' *observed* cluster. The two answer different questions and are easily
#' conflated (Merkle, Furr & Rabe-Hesketh, 2019); the per-cluster marginal
#' is the usual model-comparison target, so it is the default, and
#' `type = "loso"` warns. This matches `loo(type = "loso")` -- the two read
#' the same estimand off the same expansion.
#'
#' Under the default `test = "standard"`, [inlavaan()] derives the WAIC at
#' fit time from the same computation as the fit-time LOO (at no extra
#' cost, whenever that LOO runs) and stores it with the fit: `waic(fit)`
#' then returns the stored result when called with default arguments, and
#' [fitmeasures()] reports `waic`, `p_waic`, `se_waic` as part of `"all"`
#' for free. If the `loo` package is attached it masks this generic, but
#' dispatch on INLAvaan objects continues to work.
#'
#' @param x A fitted [INLAvaan] object (or its `inlavaan_internal` list).
#' @param type Unit type: `"auto"` (default) resolves to per-subject for
#'   single-level models and per-cluster (marginal WAIC) for two-level
#'   models. `"loso"` on a two-level model scores the conditional
#'   (leave-one-unit-out) WAIC instead (with a warning; see Details);
#'   `"loco"` cannot be forced on a model without clusters.
#' @param units Optional integer vector of unit indices to score; defaults
#'   to all units.
#' @param second_order Logical; include the second-order (Hessian) terms
#'   (default `TRUE`). `FALSE` gives the first-order WAIC, which equals the
#'   first-order LOO exactly.
#' @param cores Number of cores for differentiating the unit scores. The
#'   default `NULL` runs serially; parallelism must be requested
#'   explicitly.
#' @param verbose Logical; print progress (default `FALSE`).
#' @param ... Not used.
#'
#' @returns An object of class `inlavaan_waic`: a list with `per_unit`
#'   (pointwise `lpd`, `p_waic`, `elpd_waic`, with the same `unit`/`group`
#'   identification as [loo()]), `estimates` (matrix with rows
#'   `elpd_waic`, `p_waic`, `waic` and columns `Estimate`, `SE`), `type`,
#'   `flavour`, `n_units`, `n_groups`, `n_lpd_ok`, and `second_order`.
#'
#' @seealso [loo()], [fitmeasures()]
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, meanstructure = TRUE)
#' waic(fit)
#' }
#'
#' @export
waic <- function(x, ...) {
  UseMethod("waic")
}

#' @rdname waic
#' @exportS3Method waic INLAvaan
waic.INLAvaan <- function(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  waic.inlavaan_internal(
    x@external$inlavaan_internal,
    type = type,
    units = units,
    second_order = second_order,
    cores = cores,
    verbose = verbose,
    ...
  )
}

#' @rdname waic
#' @exportS3Method waic inlavaan_internal
waic.inlavaan_internal <- function(
  x,
  type = c("auto", "loso", "loco"),
  units = NULL,
  second_order = TRUE,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  type <- match.arg(type)
  if ("nsamp" %in% names(list(...))) {
    cli_warn(
      "{.arg nsamp} is ignored: the WAIC is computed in closed form from
       the Laplace summary, without posterior draws."
    )
  }
  # Reuse the result stored at fit time when no argument deviates from the
  # defaults -- but only a result from the current, deterministic
  # implementation: a fit saved by an older INLAvaan carries a draw-based
  # object (no second_order field), a different estimand, so recompute
  if (
    type == "auto" &&
      is.null(units) &&
      isTRUE(second_order) &&
      isTRUE(x$waic$second_order)
  ) {
    if (isTRUE(verbose)) {
      cli_alert_info("Returning the WAIC stored with the fit.")
    }
    return(x$waic)
  }
  inlav_waic(
    int = x,
    type = type,
    units = units,
    second_order = second_order,
    eff_cores = resolve_loo_cores(cores),
    verbose = verbose
  )
}

#' @exportS3Method print inlavaan_waic
print.inlavaan_waic <- function(x, ...) {
  unit_word <- switch(x$type, loso = "subject", loco = "cluster")
  order_lab <- if (isTRUE(x$second_order)) "second-order" else "first-order"
  cat("WAIC (INLAvaan)\n")
  cat(
    "Computed from the Laplace summary over ",
    x$n_units,
    " ",
    unit_word,
    if (x$n_units != 1L) "s",
    if (!is.null(x$n_groups) && x$n_groups > 1L) {
      paste0(" in ", x$n_groups, " groups")
    },
    " (",
    order_lab,
    " approximation)\n",
    sep = ""
  )
  cat("\n")
  print(round(x$estimates, 1))
  # The lpd substitution changes elpd_waic itself (unlike p_loo in loo()),
  # and this is the only place it is said
  if (isTRUE(x$second_order) && x$n_lpd_ok < x$n_units) {
    n <- x$n_units
    n_bad <- n - x$n_lpd_ok
    cat(
      "\n",
      pluralize(
        "lpd uses first-order contributions for {n_bad} of {n} units"
      ),
      "\nwith no second-order term.\n",
      sep = ""
    )
  }
  n_high <- sum(x$per_unit$p_waic > 0.4, na.rm = TRUE)
  if (n_high > 0L) {
    cat(
      "\n",
      n_high,
      " unit",
      if (n_high != 1L) "s",
      " with p_waic > 0.4: the WAIC may be unreliable; prefer loo().\n",
      sep = ""
    )
  }
  invisible(x)
}
