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
#' full-posterior pointwise log predictive density.
#'
#' **Existence, and a single order throughout.** The second-order terms are
#' Gaussian integrals that need not converge. \eqn{\log \mathrm{CPO}_u^{(2)}}
#' exists exactly when \eqn{\Sigma^{-1} + H_u} is positive definite, and
#' \eqn{\mathrm{lpd}_u^{(2)}} exactly when \eqn{\Sigma^{-1} - H_u} is; these
#' are separate conditions on the same \eqn{H_u}, and a unit can satisfy one
#' and fail the other. A missing term is `NA` in `per_unit`, with
#' `per_unit$ok` recording the \eqn{\log \mathrm{CPO}} condition.
#'
#' When a term any statistic needs does not exist, that statistic is reported
#' at *first order over every unit* rather than substituting a first-order
#' term for the failing units alone. Such a substitution drops precisely the
#' curvature that made the integral diverge, so its error is systematic,
#' one-directional and concentrated on the units it is applied to, and the
#' resulting total would be an approximation of no single order. Because the
#' two conditions are gated separately, `p_loo` can fall to first order while
#' `elpd_loo` and `looic` remain at second order; each is order-homogeneous in
#' itself. Failures of the \eqn{\log \mathrm{CPO}} condition warn, since they
#' say the unit is influential enough that leave-one-out is not identified
#' from the fit; both cases are noted when the result is printed.
#'
#' Two curvature diagnostics accompany each unit. Writing the importance
#' ratio between the unit-deleted and full posteriors as
#' \eqn{r(\theta) \propto 1 / p(y_u \mid \theta)}, its moments satisfy
#' \eqn{E[r^a] < \infty} exactly when \eqn{a < 1 / k_u}, where
#' \eqn{k_u = \lambda_{\max}(-\Sigma H_u)} is reported as `k_max`. It
#' measures how much of the posterior precision the unit itself carries, so
#' \eqn{k_u < 1} is precisely the existence condition above (`k_max >= 1`
#' iff `ok` is `FALSE`), and \eqn{k_u} approaching 1 is the approach to a
#' term that diverges. `k_sum` is \eqn{\mathrm{tr}(-\Sigma H_u)}, the unit's
#' total leverage, which sums across units to the effective number of
#' parameters. Both are obtained in closed form from the Laplace summary
#' rather than estimated from draws, and are `NA` when the second-order term
#' is not computed. No threshold is applied to either: existence is the only
#' condition the package acts on.
#'
#' The type is resolved automatically: per-cluster (`"loco"`) when the model
#' was fitted with a `cluster` argument, per-subject (`"loso"`) otherwise.
#' For a two-level model these are the two estimands of Merkle, Furr &
#' Rabe-Hesketh (2019): the default per-cluster `"loco"` is the *marginal*
#' predictive (leave-one-cluster-out -- prediction for a *new* cluster),
#' while `type = "loso"` forces the *conditional* predictive
#' (leave-one-unit-out -- prediction for a new observation within an
#' *observed* cluster), where row \eqn{i} of cluster \eqn{j} contributes
#' \eqn{\ell_i = \ell_j(\mathrm{full}) - \ell_j(\mathrm{minus\ row\ } i)},
#' the conditional density of the row given the rest of its cluster. The two
#' answer different questions and are easily conflated, so the per-cluster
#' marginal is the default and `type = "loso"` warns. It works with and
#' without missing data, costs one cluster evaluation per row per Hessian
#' direction, and is best subset with `units`.
#'
#' **Multigroup models.** Groups are independent, so each unit is scored
#' against its own group's implied moments; without a mean structure the
#' exchangeability transformation applies per group, and cross-group
#' equality constraints (`group.equal`) flow through the packed parameter
#' space automatically. The per-unit results are stacked by group (a
#' `group` column records the membership), and units are identified by
#' *case number* -- the row number of the analysed dataset -- so a unit
#' keeps its identity across fits that assign or order groups differently
#' (e.g. a pooled fit versus a grouped fit of the same data, which
#' [compare()] pairs unit by unit). This makes
#' `compare(..., loo = TRUE)` the instrument of choice for the
#' measurement-invariance ladder: configural, metric, and scalar fits are
#' compared on a proper predictive scale with paired standard errors.
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
#' match -- the natural setting for covariate selection. Both flavours
#' support any covariate placement: single-level covariates, and
#' cluster-level (between) and/or within-level covariates in two-level
#' models.
#'
#' **Missing data.** Fits estimated by full-information maximum likelihood
#' (`missing = "ml"`) are scored on the *observed-data* predictive: each
#' unit contributes the density of the entries it actually has, with its
#' full row (single-level) or whole cluster (two-level) removed from the
#' conditioning set. For single-level fits the casewise kernels operate on
#' each unit's observed subset, grouping rows by missing pattern, so a unit
#' with fewer observed entries contributes a smaller log-likelihood term
#' *and* a smaller score and thus self-weights in the elpd. Two-level fits
#' are scored per cluster (`"loco"`): each cluster contributes its
#' observed-data marginal likelihood, evaluated by lavaan's raw-data
#' cluster kernels (no per-cluster sufficient statistics are needed, since
#' leave-one-cluster-out deletes the whole cluster). All carry the same
#' missing-at-random assumption as the FIML fit itself. Because the score
#' is the observed-entry predictive, a [compare()] of two missing-data fits
#' is meaningful only when they share the same observed entries (the same
#' data *and* the same holes). The two-level conditional predictive
#' (`type = "loso"`) is available under missing data too, on the same
#' kernels.
#'
#' Supported models: continuous-indicator models fitted with the `ML`
#' estimator (including FIML, `missing = "ml"`, single- and two-level),
#' single-group or multigroup (multigroup two-level models are not
#' supported yet). If the `loo` package is attached it masks this generic,
#' but `loo(fit)` continues to dispatch correctly because the method is
#' registered by generic name.
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
#'   analysed dataset, as recorded in the fit -- for multigroup fits the
#'   full results are stacked by group, but a unit is always addressed by
#'   its case number); for LOCO, cluster positions.
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
#'     \item{`per_unit`}{Data frame of pointwise results: `unit` (case
#'       number for LOSO, cluster position for LOCO), `group` (multigroup
#'       fits only), `nobs` (1 for LOSO, the cluster size for LOCO),
#'       `l_star` (unit log-likelihood at the summary), `score_norm`,
#'       `lpd_1`/`lpd_2` (pointwise log predictive density),
#'       `log_cpo_1`/`log_cpo_2` (pointwise LOO contributions), `det_term`,
#'       `k_max`/`k_sum` (leverage diagnostics, see below), and `ok`
#'       (whether the second-order \eqn{\log \mathrm{CPO}} exists).}
#'     \item{`estimates`}{Matrix with rows `elpd_loo`, `p_loo`, `looic` and
#'       columns `Estimate`, `SE`, at the highest order available to each.}
#'     \item{`elpd_1`, `elpd_2`, `se_1`, `se_2`, `p_loo_1`, `p_loo_2`}{
#'       First- and second-order aggregates; the second-order ones are `NA`
#'       when any term they need does not exist.}
#'     \item{`type`, `flavour`, `n_units`, `n_groups`, `n_ok`, `n_lpd_ok`,
#'       `second_order`, `use_second`, `use_second_p`,
#'       `theta_overridden`}{Metadata; `n_ok` and `n_lpd_ok` count the units
#'       whose second-order \eqn{\log \mathrm{CPO}} and \eqn{\mathrm{lpd}}
#'       exist, `use_second` and `use_second_p` record the order actually
#'       used for `elpd_loo` and for `p_loo`; `flavour` records
#'       whether units were scored jointly with their covariates
#'       (`"joint"`) or conditionally on them (`"conditional"`, for
#'       `fixed.x` fits).}
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
  order_lab <- if (isTRUE(x$use_second)) "second-order" else "first-order"
  cat("Taylor ", label, " cross-validation (INLAvaan)\n", sep = "")
  cat(
    "Computed from ",
    x$n_units,
    " ",
    unit_word,
    if (x$n_units != 1L) "s",
    if (!is.null(x$n_groups) && x$n_groups > 1L) {
      paste0(" in ", x$n_groups, " groups")
    },
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
  # Which existence condition failed, and hence which estimates dropped an
  # order. The log CPO condition takes every estimate down with it; a missing
  # second-order lpd reaches p_loo alone.
  if (isTRUE(x$second_order) && x$n_ok < x$n_units) {
    n <- x$n_units
    n_bad <- n - x$n_ok
    cat(
      "\n",
      pluralize(
        "{n_bad} of {n} units {qty(n_bad)}{?has/have} no second-order term"
      ),
      " (k_max >= 1);\nall estimates are reported at first order.\n",
      sep = ""
    )
  } else if (isTRUE(x$second_order) && x$n_lpd_ok < x$n_units) {
    n <- x$n_units
    n_bad <- n - x$n_lpd_ok
    cat(
      "\n",
      pluralize(
        "{n_bad} of {n} units {qty(n_bad)}{?has/have} no second-order lpd"
      ),
      ";\np_loo is reported at first order.\n",
      sep = ""
    )
  }
  if (isTRUE(x$theta_overridden)) {
    cat("\nEvaluated at a user-supplied (theta, Sigma) summary.\n")
  }
  invisible(x)
}
