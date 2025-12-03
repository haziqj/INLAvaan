show_inlavaan <- function(object) {
  class(object) <- "lavaan"
  garb <- capture.output( tmp <- show(object) )
  tmp$test <- NULL
  garb <- capture.output( tmp )
  garb <- gsub("lavaan", "INLAvaan", garb)
  cat(paste0(garb, collapse = "\n"))
  cat("\n\n")

  ## ----- Print marginal log-likelihood and ppp -------------------------------
  show_ppp <- if(length(object@Fit@test$ppp) == 0) FALSE else TRUE

  cat(
    "Model Test (User Model):\n\n",
    sprintf("  %-38s", "Marginal log-likelihood"),
    sprintf("  %10.3f", object@Fit@test$mloglik$stat),
    "\n"
  )

  if (isTRUE(show_ppp)) {
    cat(
      sprintf("   %-38s", "PPP (Chi-square)"),
      sprintf("  %10.3f", object@Fit@test$ppp$stat),
      "\n"
    )
  }
}

setMethod("show", "INLAvaan", show_inlavaan)
