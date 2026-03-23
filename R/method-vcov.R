#' Variance-Covariance Matrix for INLAvaan Models
#'
#' Extract the posterior variance-covariance matrix of model parameters from a
#' fitted \code{INLAvaan} model.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. \code{"lavaan"} (default) returns the posterior
#'   covariance matrix of the model parameters computed from posterior
#'   samples (matching lavaan output). \code{"theta"} returns the Laplace
#'   approximation covariance in the internal parameterisation.
#' @param ... Currently unused.
#'
#' @returns A square numeric matrix.
#'
#' @name vcov
#' @export
setMethod("vcov", "INLAvaan", function(object, type = c("lavaan", "theta"), ...) {
  type <- match.arg(type)
  if (type == "lavaan") {
    object@vcov$vcov
  } else {
    object@vcov$vcov_theta
  }
})
