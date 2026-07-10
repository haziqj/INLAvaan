#' Model-Implied Moments for INLAvaan Models
#'
#' Extract the model-implied (fitted) sample statistics from a fitted
#' \code{INLAvaan} model. As in \pkg{lavaan} and \pkg{blavaan}, the moments are
#' the model-implied covariance matrix (and mean vector, when a mean structure
#' is present) evaluated at the parameter estimates -- here the posterior means.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. \code{"moments"} (default) returns the model-implied
#'   variance-covariance matrix and, when relevant, the mean vector (plus
#'   thresholds for ordinal data). \code{"casewise"} (aliases \code{"obs"},
#'   \code{"ov"}) returns the model-predicted values for each observation.
#' @param labels Logical. Attach variable names to the output. Default
#'   \code{TRUE}.
#' @param ... Currently unused.
#'
#' @returns For \code{type = "moments"}, a list (or list of lists, for
#'   multiple groups) with elements such as \code{cov}, \code{mean}, and
#'   \code{th}. For \code{type = "casewise"}, a numeric matrix of predicted
#'   observed-variable values.
#'
#' @details
#' This delegates to \pkg{lavaan}'s own \code{fitted()} machinery, so the return
#' structure matches lavaan exactly. Because INLAvaan stores the posterior means
#' as the point estimates of the fitted object, the implied moments are the
#' posterior-mean model-implied moments (mirroring \pkg{blavaan}).
#'
#' @seealso [predict()], [coef()], [fitMeasures()][lavaan::fitMeasures]
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
#' # Model-implied covariance matrix (posterior means)
#' fitted(fit)
#'
#' # Casewise model-predicted observed values
#' head(fitted(fit, type = "ov"))
#' }
#'
#' @importFrom stats fitted
#' @name fitted
#' @aliases fitted,INLAvaan-method
#' @export
setMethod("fitted", "INLAvaan", function(object, type = "moments", labels = TRUE, ...) {
  # Delegate to lavaan's implementation so the output structure (moments,
  # casewise) stays identical; the posterior means already live in the object.
  lavaan::fitted(as(object, "lavaan"), type = type, labels = labels, ...)
})

#' @importFrom stats fitted.values
#' @rdname fitted
#' @aliases fitted.values,INLAvaan-method
#' @export
setMethod("fitted.values", "INLAvaan", function(object, type = "moments", labels = TRUE, ...) {
  lavaan::fitted.values(as(object, "lavaan"), type = type, labels = labels, ...)
})
