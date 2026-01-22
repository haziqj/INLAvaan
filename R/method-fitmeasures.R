inlav_fit_measures <- function(
  object,
  fit.measures = "all",
  baseline.model = NULL
) {
  # Borrow some ideas from R/blav_fit_measures.R in blavaan package

  # Has the model converged?
  if (object@Fit@npar > 0L & !object@optim$converged) {
    cli::cli_alert_warning("Optimiser did not converge.")
  }

  # # Do we have a test statistic?
  # opts <- lavInspect(fit, "options")
  # if (opts$test == "none") {
  #   cli::abort("Fit measures cannot be obtained when `test='none'`")
  # }

  out <- vector("numeric")
  out["npar"] <- object@Fit@npar
  out["margloglik"] <- object@external$inlavaan_internal$mloglik

  # If test != "none"
  if (length(object@Fit@test$ppp) > 0) {
    out["ppp"] <- object@external$inlavaan_internal$ppp
    out["dic"] <- object@external$inlavaan_internal$DIC$dic
    out["p_dic"] <- object@external$inlavaan_internal$DIC$pD
  }

  # Diagnostics based on gradients
  grad <- object@external$inlavaan_internal$opt$dx
  pars <- object@external$inlavaan_internal$theta_star
  out["grad_inf"] <- max(abs(grad))
  out["grad_inf_rel"] <- max(abs(grad) / (abs(pars) + 1e-06))
  out["grad_l2"] <- sqrt(sum(grad^2))

  class(out) <- c("fitmeasures.inlavaan_internal", "numeric")
  out
}

#' @exportS3Method print fitmeasures.inlavaan_internal
print.fitmeasures.inlavaan_internal <- function(x, ...) {
  nm <- names(x)

  # Apply conditional formatting
  formatted_values <- sapply(seq_along(x), function(i) {
    val <- x[i]
    name <- nm[i]

    if (name == "npar") {
      return(as.character(as.integer(round(val))))
    } else if (startsWith(name, "grad_")) {
      # Use formatC to force scientific and maintain 3 significant digits
      return(formatC(val, digits = 2, format = "e"))
    } else {
      # Round to 3 decimal places
      return(formatC(val, digits = 3, format = "f", drop0trailing = FALSE))
    }
  })

  # Set names back onto the formatted character vector
  names(formatted_values) <- nm

  # Print using the standard named vector style without quotes
  print(formatted_values, quote = FALSE, right = TRUE)

  invisible(x)
}

#' Fit Measures for a Latent Variable Model estimated using INLA
#'
#' @param object An object of class [INLAvaan].
#' @param fit.measures If `"all"`, all fit measures available will be returned. If
#'   only a single or a few fit measures are specified by name, only those are
#'   computed and returned.
#' @param baseline.model Not currently used, added for compatability with `{lavaan}`.
#'
#' @returns A named numeric vector of fit measures.
#'
#' @importMethodsFrom lavaan fitMeasures
#' @rdname fitMeasures
#' @export
setMethod("fitMeasures", "INLAvaan", inlav_fit_measures)

#' @importMethodsFrom lavaan fitmeasures
#' @rdname fitMeasures
#' @export
setMethod("fitmeasures", "INLAvaan", inlav_fit_measures)
