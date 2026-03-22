#' @rdname INLAvaan-class
#' @param type Character. `"lavaan"` (default) returns the posterior
#'   covariance matrix of the model parameters computed from posterior
#'   samples (matching lavaan output). `"theta"` returns the Laplace
#'   approximation covariance in the internal parameterisation.
#' @export
setMethod("vcov", "INLAvaan", function(object, type = c("lavaan", "theta"), ...) {
  type <- match.arg(type)
  if (type == "lavaan") {
    object@vcov$vcov
  } else {
    object@vcov$vcov_theta
  }
})
