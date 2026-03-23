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
#' @seealso [summary()], [coef()], [standardisedsolution()]
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
#'             test = "none", verbose = FALSE)
#'
#' # Default: posterior covariance of lavaan parameters
#' vcov(fit)
#'
#' # Internal parameterisation (Laplace approximation)
#' vcov(fit, type = "theta")
#' }
#'
#' @name vcov
#' @aliases vcov,INLAvaan-method
#' @export
setMethod("vcov", "INLAvaan", function(object, type = c("lavaan", "theta"), ...) {
  type <- match.arg(type)
  if (type == "lavaan") {
    object@vcov$vcov
  } else {
    object@vcov$vcov_theta
  }
})
