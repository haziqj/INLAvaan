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
#' @param per_cluster Logical. For a random-slope model, return the moments
#'   of each cluster instead of their average. Default \code{FALSE}.
#' @param ... Currently unused.
#'
#' @returns For \code{type = "moments"}, a list (or list of lists, for
#'   multiple groups) with elements such as \code{cov}, \code{mean}, and
#'   \code{th}. For \code{type = "casewise"}, a numeric matrix of predicted
#'   observed-variable values. With \code{per_cluster = TRUE}, a list with one
#'   element per cluster, each holding \code{cov} and \code{mean}.
#'
#' @details
#' This delegates to \pkg{lavaan}'s own \code{fitted()} machinery, so the return
#' structure matches lavaan exactly. Because INLAvaan stores the posterior means
#' as the point estimates of the fitted object, the implied moments are the
#' posterior-mean model-implied moments (mirroring \pkg{blavaan}).
#'
#' For a model with random slopes (lavaan's \code{rv()}), the moments are
#' averaged over the covariates and include the mean and the variance of each
#' slope. With \code{per_cluster = TRUE}, each cluster gets the expected
#' cluster mean and within-cluster covariance (divisor \eqn{n_j}) of its
#' outcomes and covariates at its own covariate values. This is not available
#' for a slope on a latent or split covariate. \code{type = "casewise"} is not
#' available for random-slope models.
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
setMethod(
  "fitted",
  "INLAvaan",
  function(object, type = "moments", labels = TRUE, per_cluster = FALSE, ...) {
    check_rs_moments(object, "fitted", type)
    check_per_cluster(object, per_cluster)
    if (has_random_slopes(object@Model)) {
      return(rs_fitted(object, labels = labels, per_cluster = per_cluster))
    }
    # Delegate to lavaan's implementation so the output structure (moments,
    # casewise) stays identical; the posterior means already live in the object.
    lavaan::fitted(as(object, "lavaan"), type = type, labels = labels, ...)
  }
)

#' @importFrom stats fitted.values
#' @rdname fitted
#' @aliases fitted.values,INLAvaan-method
#' @export
setMethod(
  "fitted.values",
  "INLAvaan",
  function(object, type = "moments", labels = TRUE, per_cluster = FALSE, ...) {
    check_rs_moments(object, "fitted.values", type)
    check_per_cluster(object, per_cluster)
    if (has_random_slopes(object@Model)) {
      return(rs_fitted(object, labels = labels, per_cluster = per_cluster))
    }
    lavaan::fitted.values(
      as(object, "lavaan"),
      type = type,
      labels = labels,
      ...
    )
  }
)
