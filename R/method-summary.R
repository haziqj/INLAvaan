#' @exportS3Method summary inlavaan_internal
summary.inlavaan_internal <- function(object, ...) {
  structure(
    list(summary = object$summary),
    class = "summary.inlavaan_internal"
  )
}

#' @exportS3Method print summary.inlavaan_internal
print.summary.inlavaan_internal <- function(x, digits = 3, ...) {
  summ <- x$summary
  which_numeric <- sapply(summ, is.numeric)
  summ[, which_numeric] <- round(summ[, which_numeric], digits)
  print(summ)
  invisible(x)
}

summary_inlavaan <- function(
  object,
  header = TRUE,
  fit.measures = TRUE,
  estimates = TRUE,
  standardized = FALSE,
  rsquare = FALSE,
  postmedian = !FALSE,
  postmode = !FALSE,
  priors = TRUE,
  nd = 3L,
  ...
) {
  ## ----- Header --------------------------------------------------------------
  if (isTRUE(header)) {
    show_inlavaan(object)
  }

  ## ----- Fit measures --------------------------------------------------------
  if (isTRUE(fit.measures) & length(object@Fit@test$ppp) > 0) {
    # DIC
    dic_list <- object@external$inlavaan_internal$DIC
    cat(
      "\nInformation Criteria:\n\n",
      sprintf("  %-38s", "Deviance (DIC)"),
      sprintf("  %10.3f", dic_list$dic),
      "\n",
      sprintf("  %-38s", "Effective parameters (pD)"),
      sprintf("  %10.3f", dic_list$pD),
      "\n"
    )
  }

  if (isTRUE(estimates)) {
    if (isTRUE(standardized) | isTRUE(rsquare)) {
      cli::cli_alert_warning(
        "{.arg standardized = TRUE} or {.arg rsquare = TRUE} are not implemented yet."
      )
    }

    marg_method <- object@external$inlavaan_internal$marginal_method
    # if (marg_method == "skewnorm")
    #   marg_method <- "Skew Normal"
    # else if (marg_method == "asymgaus")
    #   marg_method <- "Two-piece Gaussian"
    # else if (marg_method == "marggaus")
    #   marg_method <- "Marginal Gaussian"
    # else if (marg_method == "sampling")
    #   marg_method <- "Sampling"

    PE <- lavaan::parameterEstimates(
      object,
      se = TRUE,
      zstat = FALSE,
      ci = TRUE,
      standardized = standardized,
      rsquare = rsquare,
      remove.eq = FALSE,
      remove.system.eq = TRUE,
      remove.ineq = FALSE,
      remove.def = FALSE,
      header = TRUE,
      output = "text"
    )

    # Now need to put information into PE from pt and summary
    pt <- object@ParTable
    ptfreeidx <- which(pt$free > 0)
    summ <- object@external$inlavaan_internal$summary
    peidx <- match(
      paste0(
        pt$lhs[ptfreeidx],
        pt$op[ptfreeidx],
        pt$rhs[ptfreeidx],
        pt$group[ptfreeidx]
      ),
      paste0(PE$lhs, PE$op, PE$rhs, PE$group)
    )
    summidx <- match(pt$free[pt$free > 0], seq_len(nrow(summ)))

    char.format <- paste("%", max(8, nd + 5), "s", sep = "")

    PE$SD <- ""
    PE$SD[peidx] <- formatC(summ$SD[summidx], digits = nd, format = "f")

    PE$`2.5%` <- ""
    PE$`2.5%`[peidx] <- formatC(summ$`2.5%`[summidx], digits = nd, format = "f")

    if (isTRUE(postmedian)) {
      PE$`50%` <- ""
      PE$`50%`[peidx] <- formatC(summ$`50%`[summidx], digits = nd, format = "f")
    }

    PE$`97.5%` <- ""
    PE$`97.5%`[peidx] <- formatC(
      summ$`97.5%`[summidx],
      digits = nd,
      format = "f"
    )

    if (isTRUE(postmode)) {
      PE$Mode <- ""
      PE$Mode[peidx] <- formatC(summ$Mode[summidx], digits = nd, format = "f")
    }

    if (isTRUE(priors)) {
      PE$Prior <- ""
      PE$Prior[peidx] <- summ$Prior[summidx]
    }
  }

  # Repair credible intervals labels
  garb <- capture.output(PE)
  garb <- gsub("X2.5.", " 2.5%", garb)
  garb <- gsub(" X50.", "  50%", garb)
  garb <- gsub("X97.5.", " 97.5%", garb)

  # Add Parameter Estimates section
  idxpehead <- grep("Parameter Estimates", garb)
  garb[idxpehead + 2] <- capture.output(cat(
    "\n",
    sprintf("  %-38s", "Marginalisation method"),
    sprintf("  %10s", toupper(marg_method))
  ))[2]

  # Print
  cat(paste0(garb, collapse = "\n"))
  cat("\n")
}

setMethod("summary", "INLAvaan", summary_inlavaan)
