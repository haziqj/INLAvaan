#' @export
compare <- function(x, y, ...) {
  UseMethod("compare")
}

#' @export
compare.inlavaan_internal <- function(x, y, ...) {
  # Capture user-supplied names
  call_list <- as.list(substitute(list(x, y, ...)))[-1]
  modnames  <- sapply(call_list, deparse)

  # Collect model objects
  models <- list(x, y, ...)
  nmod   <- length(models)

  # Extract criteria
  marg_ll <- sapply(models, function(m) m$marg_loglik)
  AIC     <- sapply(models, function(m) m$AIC)
  BIC     <- sapply(models, function(m) m$BIC)

  # Identify best model by highest marginal log-likelihood
  best_idx <- which.max(marg_ll)
  best_ll  <- marg_ll[best_idx]

  # Compute log Bayes Factors and BF relative to best model
  logBF <- marg_ll - best_ll
  BF    <- exp(logBF)

  # Create comparison table
  out <- data.frame(
    Model         = modnames,
    marg_loglik   = marg_ll,
    AIC           = AIC,
    BIC           = BIC,
    logBF         = round(logBF, 3),
    BF            = round(BF, 3),
    stringsAsFactors = FALSE
  )

  # Order table by (best first)
  out <- out[order(-out$marg_loglik), ]
  rownames(out) <- NULL

  class(out) <- c("compare.inlavaan_compare", class(out))
  out
}

# print.compare.inlavaan_compare <- function(x, ...) {
#   cat("Bayesian Model Comparison (INLAvaan)\n")
#   cat("Models ordered by marginal log-likelihood\n\n")
#   print.data.frame(x, row.names = FALSE)
#   invisible(x)
# }
