#' Compare Bayesian Models Fitted with INLAvaan
#'
#' Compare Bayesian Models Fitted with INLAvaan
#'
#' The function computes the log Bayes Factor (logBF) relative to the best
#' fitting model (the one with the highest Marginal Log-Likelihood).
#'
#' The output table sorts models by descending Marginal Log-Likelihood.
#'   - **Marg.Loglik**: The approximated marginal log-likelihood.
#'   - **DIC**: Deviance Information Criterion (if available).
#'   - **pD**: Effective number of parameters (if available).
#'   - **logBF**: The natural logarithm of the Bayes Factor relative to the best model.
#'
#' @param x,y,... An object of class `INLAvaan` or `inlavaan_internal`.
#'
#' @return A data frame of class `compare.inlavaan_internal` containing model
#'   fit statistics.
#'
#' @references https://lavaan.ugent.be/tutorial/groups.html
#'
#' @example inst/examples/ex-model_comparison.R
#' @export
setGeneric("compare", function(x, y, ...) standardGeneric("compare"))

#' @rdname compare
#' @aliases compare,INLAvaan-class
#' @export
setMethod("compare", "INLAvaan", function(x, y, ...) {
  mc <- match.call()
  call_list <- as.list(mc)[-1]
  modnames <- sapply(call_list, deparse)
  FUN <- getFromNamespace("compare.inlavaan_internal", "INLAvaan")
  mc[[1]] <- FUN
  eval(mc, parent.frame())
})

compare.inlavaan_internal <- function(x, y, ...) {
  # Capture user-supplied names
  call_list <- as.list(substitute(list(x, y, ...)))[-1]
  modnames <- sapply(call_list, deparse)

  # Collect model objects
  models <- list(x, y, ...)
  nmod <- length(models)

  models <- lapply(models, function(x) {
    if (inherits(x, "INLAvaan")) {
      return(x@external$inlavaan_internal)
    } else if (inherits(x, "inlavaan_internal")) {
      return(x)
    }
  })

  # Extract criteria
  marg_ll <- sapply(models, function(m) m$mloglik)
  DIC <- unlist(sapply(models, function(m) m$DIC$dic))
  pD <- unlist(sapply(models, function(m) m$DIC$pD))
  m <- sapply(models, function(m) length(m$theta_star))

  # Identify best model by highest marginal log-likelihood
  best_idx <- which.max(marg_ll)
  best_ll <- marg_ll[best_idx]

  # Compute log Bayes Factors and BF relative to best model
  logBF <- marg_ll - best_ll
  BF <- exp(logBF)

  # Create comparison table
  out <- data.frame(
    Model = modnames,
    No.params = m,
    Marg.Loglik = marg_ll,
    DIC = if (is.null(DIC)) "" else DIC,
    pD = if (is.null(pD)) "" else pD,
    logBF = round(logBF, 3),
    # BF            = round(BF, 3),
    stringsAsFactors = FALSE
  )

  # Order table by (best first)
  out <- out[order(-out$Marg.Loglik), ]
  rownames(out) <- NULL

  class(out) <- c("compare.inlavaan_internal", class(out))
  out
}

#' @exportS3Method print compare.inlavaan_internal
print.compare.inlavaan_internal <- function(x, ...) {
  cat("Bayesian Model Comparison (INLAvaan)\n")
  cat("Models ordered by marginal log-likelihood\n\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}
