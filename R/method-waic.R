#' Widely Applicable Information Criterion for INLAvaan Models
#'
#' Computes the WAIC of a fitted [INLAvaan] model from unit log-likelihoods
#' evaluated over posterior draws. Single-level models are scored per
#' subject; two-level models are scored per cluster, matching the units used
#' by [loo()].
#'
#' @details
#' For each posterior draw \eqn{\theta_s} (drawn with the same copula
#' sampler used for the fit's posterior summaries) and unit \eqn{u}, the
#' log-likelihood \eqn{\log p(y_u \mid \theta_s)} is evaluated; then
#' \eqn{\mathrm{lpd}_u = \log \tfrac{1}{S}\sum_s p(y_u \mid \theta_s)},
#' \eqn{p_{\mathrm{waic},u} = \mathrm{var}_s \log p(y_u \mid \theta_s)}, and
#' \eqn{\mathrm{elpd}_{\mathrm{waic}} = \sum_u (\mathrm{lpd}_u -
#' p_{\mathrm{waic},u})} with \eqn{\mathrm{WAIC} = -2\,
#' \mathrm{elpd}_{\mathrm{waic}}}. Unlike [loo()], this is a sampling-based
#' estimate: results vary with the random draws, and units with
#' \eqn{p_{\mathrm{waic},u} > 0.4} trigger a reliability warning (also
#' annotated when printing). The same model restrictions as [loo()] apply,
#' and so does the flavour rule: fits with `fixed.x = TRUE` are scored
#' conditionally on the exogenous covariates, fits with `fixed.x = FALSE`
#' jointly (see [loo()]).
#'
#' Under the default `test = "standard"`, [inlavaan()] computes the WAIC at
#' fit time by reusing the posterior draws the fit already produced (when
#' the model is supported, has a mean structure, and `nsamp >= 100`), and
#' stores it with the fit: `waic(fit)` then returns the stored result when
#' called with default arguments, and [fitmeasures()] reports `waic`,
#' `p_waic`, `se_waic` as part of `"all"` for free. If the `loo` package is
#' attached it masks this generic, but dispatch on INLAvaan objects
#' continues to work.
#'
#' @param x A fitted [INLAvaan] object (or its `inlavaan_internal` list).
#' @param units Optional integer vector of unit indices to score; defaults
#'   to all units.
#' @param nsamp Number of posterior draws. Defaults to the `nsamp` used when
#'   fitting the model.
#' @param cores Number of cores for evaluating draws. The default `NULL`
#'   runs serially; parallelism must be requested explicitly.
#' @param verbose Logical; print progress (default `FALSE`).
#' @param ... Not used.
#'
#' @returns An object of class `inlavaan_waic`: a list with `per_unit`
#'   (pointwise `lpd`, `p_waic`, `elpd_waic`), `estimates` (matrix with rows
#'   `elpd_waic`, `p_waic`, `waic` and columns `Estimate`, `SE`), `type`,
#'   `n_units`, and `nsamp`.
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
  units = NULL,
  nsamp = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  waic.inlavaan_internal(
    x@external$inlavaan_internal,
    units = units,
    nsamp = nsamp,
    cores = cores,
    verbose = verbose
  )
}

#' @rdname waic
#' @exportS3Method waic inlavaan_internal
waic.inlavaan_internal <- function(
  x,
  units = NULL,
  nsamp = NULL,
  cores = NULL,
  verbose = FALSE,
  ...
) {
  # Reuse the result stored at fit time when no argument deviates from the
  # defaults
  if (is.null(units) && is.null(nsamp) && !is.null(x$waic)) {
    if (isTRUE(verbose)) {
      cli_alert_info("Returning the WAIC stored with the fit.")
    }
    return(x$waic)
  }
  inlav_waic(
    int = x,
    units = units,
    nsamp = nsamp,
    eff_cores = resolve_loo_cores(cores),
    verbose = verbose
  )
}

#' @exportS3Method print inlavaan_waic
print.inlavaan_waic <- function(x, ...) {
  unit_word <- switch(x$type, loso = "subject", loco = "cluster")
  cat("WAIC (INLAvaan)\n")
  cat(
    "Computed from ",
    x$nsamp,
    " posterior draws and ",
    x$n_units,
    " ",
    unit_word,
    if (x$n_units != 1L) "s",
    "\n",
    sep = ""
  )
  if (identical(x$flavour, "conditional")) {
    cat("Scored conditionally on the exogenous covariates (fixed.x fit)\n")
  }
  cat("\n")
  print(round(x$estimates, 1))
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
