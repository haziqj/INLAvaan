#' Model Residuals for INLAvaan Models
#'
#' Extract the difference between the observed and model-implied (fitted)
#' sample statistics from a fitted \code{INLAvaan} model. As in \pkg{lavaan}
#' and \pkg{blavaan}, residuals are computed at the parameter estimates --
#' here the posterior means -- not as a posterior distribution over residuals.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. \code{"raw"} (default) returns the unscaled
#'   difference between the observed and model-implied covariance matrix (and
#'   mean vector, when a mean structure is present). \code{"cor"} (or
#'   \code{"cor.bollen"}) first rescales both matrices to a correlation
#'   matrix. \code{"cor.bentler"} rescales both by the observed variances
#'   (the basis of the SRMR). \code{"normalized"} and \code{"standardized"}
#'   divide the raw residuals by their asymptotic standard errors.
#'   \code{"casewise"} (aliases \code{"case"}, \code{"obs"},
#'   \code{"observations"}, \code{"ov"}) returns observed-minus-fitted values
#'   for each observation.
#' @param labels Logical. Attach variable names to the output. Default
#'   \code{TRUE}.
#' @param per_cluster Logical. For a random-slope model, return the residuals
#'   of each cluster instead of the averaged ones. Default \code{FALSE}.
#' @param ... Currently unused.
#'
#' @returns For moment-based \code{type}s, a list with elements
#'   \code{type}, \code{cov}, and (when relevant) \code{mean}. For
#'   \code{type = "casewise"}, a numeric matrix of observed-minus-fitted
#'   values.
#'
#' @details
#' This delegates to \pkg{lavaan}'s own \code{residuals()} machinery, so the
#' return structure matches lavaan exactly. Because INLAvaan stores the
#' posterior means as the point estimates of the fitted object, the residuals
#' are the observed statistics minus the posterior-mean model-implied
#' statistics (mirroring \pkg{blavaan}, which likewise inherits lavaan's
#' \code{residuals()} without overriding it).
#'
#' For a model with random slopes (lavaan's \code{rv()}), the residuals
#' compare the sample moments with the averaged moments of [fitted()]. With
#' \code{per_cluster = TRUE}, they compare each cluster's sample mean and
#' within-cluster covariance with its expected ones. A covariance of two
#' variables observed together in fewer than two rows of a cluster is
#' \code{NA}. The raw, correlation and casewise types are available.
#'
#' @seealso [fitted()], [predict()], [fitMeasures()][lavaan::fitMeasures]
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
#' # Raw residual covariance matrix (posterior means)
#' residuals(fit)
#'
#' # SRMR-basis residuals
#' residuals(fit, type = "cor.bentler")
#'
#' # Casewise observed-minus-fitted values
#' head(residuals(fit, type = "casewise"))
#' }
#'
#' @importFrom stats residuals
#' @name residuals
#' @aliases residuals,INLAvaan-method
#' @export
setMethod(
  "residuals",
  "INLAvaan",
  function(object, type = "raw", labels = TRUE, ..., per_cluster = FALSE) {
    check_rs_moments(object, "residuals", type)
    check_per_cluster(object, per_cluster)
    if (has_random_slopes(object@Model)) {
      return(rs_residuals(
        object,
        type = type,
        labels = labels,
        per_cluster = per_cluster
      ))
    }
    # Delegate to lavaan's implementation so the output structure (moments,
    # casewise) stays identical; the posterior means already live in the object.
    lavaan::residuals(as(object, "lavaan"), type = type, labels = labels, ...)
  }
)

#' @importFrom stats resid
#' @rdname residuals
#' @aliases resid,INLAvaan-method
#' @export
setMethod(
  "resid",
  "INLAvaan",
  function(object, type = "raw", ..., per_cluster = FALSE) {
    check_rs_moments(object, "resid", type)
    check_per_cluster(object, per_cluster)
    if (has_random_slopes(object@Model)) {
      return(rs_residuals(object, type = type, per_cluster = per_cluster))
    }
    lavaan::resid(as(object, "lavaan"), type = type, ...)
  }
)
