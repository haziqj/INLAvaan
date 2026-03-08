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

#' Helper function to check if two functions are the same
#'
#' @param f,g Functions to compare.
#'
#' @returns Logical.
#' @export
#'
#' @examples
#' f1 <- function(x) { x^2 + 1 }
#' f2 <- function(x) { x^2 + 1 }
#' is_same_function(f1, f2)  # TRUE
is_same_function <- function(f, g) {
  identical(deparse(body(f)), deparse(body(g)))
}

#' Convert function to single string
#'
#' @param f Function to convert.
#' @returns A single character vector representing the function.
#' @export
#'
#' @examples
#' f <- function(x) { x^2 + 1 }
#' as_fun_string(f)
as_fun_string <- function(f) {
  gsub("\\s+", " ", paste(deparse(f), collapse = " "))
}

# Check if matrix is a bad covariance (not PD, or contains NA/NaN/Inf)
is_bad_cov <- function(mat) {
  if (any(!is.finite(mat))) return(TRUE)
  tryCatch({ chol(mat); FALSE }, error = function(e) TRUE)
}

# Nearest PD via eigenvalue clamping
make_pd <- function(X, tol = 1e-8) {
  e <- eigen(X, symmetric = TRUE)
  vals <- pmax(e$values, tol * max(e$values))
  e$vectors %*% (vals * t(e$vectors))
}

# Get internal inlavaan object
get_inlavaan_internal <- function(object) {
  if (!inherits(object, "INLAvaan")) {
    cli_abort("Object must be of class {.var INLAvaan}")
  }
  object@external$inlavaan_internal
}

# Helper function to add timing information. Adapted by Haziq Jamil. Original
# author: Luc De Wilde (lavaan).
add_timing <- function(timing, part) {
  timenow <- proc.time()[3]
  timing[[part]] <- (timenow - timing$start.time)
  timing$start.time <- timenow

  timing
}
is_lavaan <- function(object) {
  is(object, "lavaan") & attr(class(object), "package") == "lavaan"
}

is_blavaan <- function(object) {
  is(object, "blavaan") & attr(class(object), "package") == "blavaan"
}

is_INLAvaan <- function(object) {
  is(object, "INLAvaan") & attr(class(object), "package") == "INLAvaan"
}

is_inlavaan <- function(object) {
  is(object, "INLAvaan") & attr(class(object), "package") == "INLAvaan"
}

dmode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NA)
  }
  if (length(x) == 1) {
    return(x)
  }
  if (stats::sd(x) < .Machine$double.eps^0.5) {
    return(mean(x))
  }
  d <- stats::density(x)
  d$x[which.max(d$y)]
}
