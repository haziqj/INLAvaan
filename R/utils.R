cli_messages <- c(
  "Laplace-ing through p dimensions",
  "Summoning Bayesian spirits",
  "Casting statistical spells",
  "Conjuring INLA magic",
  "Channeling Laplace's wizardry",
  "Harnessing the power of priors",
  "Diving into the probability pool",
  "Navigating the seas of stochasticity"
)

#' Convert function to single string
as_fun_string <- function(f) {
  gsub("\\s+", " ", paste(deparse(f), collapse = " "))
}

#' Helper function to add timing information
#'
#' This function adds an element with name as specified in parameter part and
#' the duration of the interval from start.time upto now thereafter the element
#' start.time is set to now (prepare for next call) the adapted list is returned
#'
#' This function is adapter from the `ldw_add_timing()` helper in the `{lavaan}`
#' package. Original implementation copyright the lavaan project, 2010-2025,
#' GPL-3.
#'
#' @param timing List with element `start.time`
#' @param part Character string with name of part to add timing for
#'
#' @author Adapted by Haziq Jamil. Original author: Luc De Wilde (lavaan).
add_timing <- function(timing, part) {
  timenow <- proc.time()[3]
  timing[[part]] <- (timenow - timing$start.time)
  timing$start.time <- timenow

  timing
}

