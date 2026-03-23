#' Compare Bayesian Models Fitted with INLAvaan
#'
#' Compare two or more Bayesian SEM fitted with INLAvaan, reporting model-fit
#' statistics and (optionally) fit indices side by side.
#'
#' @details
#' The first argument `x` serves as the **baseline** (null) model.
#' All models (including the baseline) appear in the comparison table. The
#' baseline is also passed to [fitMeasures()][lavaan::fitMeasures] when
#' incremental fit indices (BCFI, BTLI, BNFI) are requested via
#' `fit.measures`.
#'
#' The default table always includes:
#'
#'   - **npar**: Number of free parameters.
#'   - **Marg.Loglik**: Approximated marginal log-likelihood.
#'   - **logBF**: Natural-log Bayes factor relative to the best model.
#'   - **DIC** / **pD**: Deviance Information Criterion and effective number
#'     of parameters (when `test != "none"` was used during fitting).
#'
#' Set `fit.measures` to a character vector of measure names (anything
#' returned by [fitMeasures()][lavaan::fitMeasures]) to append extra columns.
#' Use `fit.measures = "all"` to include every available measure.
#'
#' @param x An [INLAvaan] (or `inlavaan_internal`) object used as the
#'   **baseline** (null) model. It is included in the comparison table and
#'   passed to [fitMeasures()][lavaan::fitMeasures] for incremental indices.
#' @param y,... One or more [INLAvaan] (or `inlavaan_internal`) objects to
#'   compare against the baseline.
#' @param fit.measures Character vector of additional fit-measure names to
#'   include (e.g. `"BRMSEA"`, `"BCFI"`). Use `"all"` to include every
#'   measure returned by [fitMeasures()][lavaan::fitMeasures]. The default
#'   (`NULL`) shows only the core comparison statistics.
#'
#' @return A data frame of class `compare.inlavaan_internal` containing model
#'   fit statistics, sorted by descending marginal log-likelihood.
#'
#' @references <https://lavaan.ugent.be/tutorial/groups.html>
#'
#' @seealso [fitmeasures()], [bfit_indices()]
#'
#' @example inst/examples/ex-model_comparison.R
#' @export
setGeneric("compare", function(x, y, ..., fit.measures = NULL) {
  standardGeneric("compare")
})

#' @name compare
#' @rdname compare
#' @aliases compare,INLAvaan-method
#' @export
setMethod("compare", "INLAvaan", function(x, y, ..., fit.measures = NULL) {
  mc <- match.call()
  dots <- list(...)

  # x = baseline, y + unnamed ... = models to compare
  model_objs <- c(list(x, y), dots)
  model_exprs <- c(
    list(mc$x, mc$y),
    as.list(mc)[-1][
      !names(as.list(mc)[-1]) %in%
        c("x", "y", "fit.measures")
    ]
  )

  modnames <- vapply(model_exprs, deparse, character(1))

  compare_impl(
    models = model_objs,
    modnames = modnames,
    fit.measures = fit.measures,
    baseline = x
  )
})

#' @exportS3Method compare inlavaan_internal
compare.inlavaan_internal <- function(x, y, ..., fit.measures = NULL) {
  mc <- match.call()
  dots <- list(...)

  model_objs <- c(list(x, y), dots)
  model_exprs <- c(
    list(mc$x, mc$y),
    as.list(mc)[-1][
      !names(as.list(mc)[-1]) %in%
        c("x", "y", "fit.measures")
    ]
  )

  modnames <- vapply(model_exprs, deparse, character(1))

  compare_impl(
    models = model_objs,
    modnames = modnames,
    fit.measures = fit.measures,
    baseline = x
  )
}

# ---- Internal workhorse ------------------------------------------------------

compare_impl <- function(
  models,
  modnames,
  fit.measures = NULL,
  baseline = NULL
) {
  # Normalise to internal objects, keeping originals for fitMeasures()
  originals <- models
  internals <- lapply(models, function(m) {
    if (inherits(m, "INLAvaan")) {
      m@external$inlavaan_internal
    } else if (inherits(m, "inlavaan_internal")) {
      m # nocov
    } else {
      cli_abort( # nocov
        "Each model must be an {.cls INLAvaan} or {.cls inlavaan_internal} object."
      )
    }
  })

  nmod <- length(internals)
  npar <- vapply(internals, function(m) length(m$theta_star), integer(1))
  marg_ll <- vapply(internals, function(m) m$mloglik, numeric(1))
  DIC_vec <- vapply(internals, function(m) m$DIC$dic %||% NA_real_, numeric(1))
  pD_vec <- vapply(internals, function(m) m$DIC$pD %||% NA_real_, numeric(1))

  best_ll <- max(marg_ll)
  logBF <- marg_ll - best_ll

  out <- data.frame(
    Model = modnames,
    npar = npar,
    Marg.Loglik = marg_ll,
    logBF = round(logBF, 3),
    stringsAsFactors = FALSE
  )

  # Append DIC/pD if any model has them
  if (!all(is.na(DIC_vec))) {
    out$DIC <- round(DIC_vec, 3)
    out$pD <- round(pD_vec, 3)
  }

  # Append extra fit measures if requested
  if (!is.null(fit.measures)) {
    has_inlavaan <- vapply(originals, function(m) is(m, "INLAvaan"), logical(1))
    if (!all(has_inlavaan)) { # nocov start
      cli_warn(
        "Fit measures require {.cls INLAvaan} objects; skipping for {.cls inlavaan_internal} models."
      )
    } else { # nocov end
      # baseline (x) is used for incremental indices
      baseline_obj <- if (is(baseline, "INLAvaan")) baseline else NULL

      fm_list <- lapply(originals, function(m) {
        tryCatch(
          fitMeasures(
            m,
            fit.measures = fit.measures,
            baseline.model = baseline_obj
          ),
          error = function(e) NULL
        )
      })
      # Union of all measure names
      all_names <- unique(unlist(lapply(fm_list, names)))
      for (nm in all_names) {
        out[[nm]] <- vapply(
          fm_list,
          function(fm) {
            if (is.null(fm) || is.na(fm[nm])) NA_real_ else round(fm[nm], 4)
          },
          numeric(1)
        )
      }
    }
  }

  if (is.null(fit.measures)) {
    out <- out[order(-out$Marg.Loglik), ]
  }

  rownames(out) <- NULL
  attr(out, "fit_measures_used") <- !is.null(fit.measures)
  attr(out, "baseline_name") <- modnames[1]
  class(out) <- c("compare.inlavaan_internal", class(out))
  out
}

#' @exportS3Method print compare.inlavaan_internal
print.compare.inlavaan_internal <- function(x, ...) {
  cat("Bayesian Model Comparison (INLAvaan)\n")
  if (isTRUE(attr(x, "fit_measures_used"))) {
    cat("Baseline model:", attr(x, "baseline_name"), "\n\n")
  } else {
    cat("Models ordered by marginal log-likelihood\n\n")
  }
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}
