#' Log-Likelihood for INLAvaan Models
#'
#' Extract a log-likelihood-flavoured summary from a fitted \code{INLAvaan}
#' model. Two distinct quantities are available, deliberately not conflated:
#' the Bayesian marginal log-likelihood (the default) and the classical
#' log-likelihood evaluated at the posterior mean.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. \code{"marginal"} (default) returns the
#'   Laplace-approximated marginal log-likelihood (log evidence), the same
#'   quantity [compare()] uses for Bayes factors. \code{"plugin"} returns the
#'   classical log-likelihood evaluated at the posterior mean point estimate,
#'   with \code{df}/\code{nobs} attributes and class \code{"logLik"} so it
#'   supports \code{\link[stats]{AIC}}/\code{\link[stats]{BIC}} at the point
#'   estimate. Requires the model to have been fitted with
#'   \code{test != "none"}.
#' @param ... Currently unused.
#'
#' @returns For \code{type = "marginal"}, a length-one numeric of class
#'   \code{inlavaan_logLik} that prints with a note on its interpretation.
#'   For \code{type = "plugin"}, a standard \code{"logLik"} object.
#'
#' @details
#' The marginal log-likelihood already integrates over the (Laplace-
#' approximated) posterior, so it is not on the same scale as a classical
#' log-likelihood and should not be passed to \code{AIC()}/\code{BIC()} --
#' doing so would double-penalise model complexity that the evidence has
#' already accounted for. Use [compare()] to compare models via Bayes
#' factors, DIC, or LOO/WAIC. The plug-in variant exists for users who
#' specifically want a point-estimate-based classical comparison.
#'
#' @seealso [deviance()], [compare()]
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
#'             test = "standard", verbose = FALSE)
#'
#' # Marginal log-likelihood (log evidence)
#' logLik(fit)
#'
#' # Classical log-likelihood at the posterior mean, AIC/BIC-compatible
#' ll <- logLik(fit, type = "plugin")
#' AIC(ll)
#' }
#'
#' @importFrom stats logLik
#' @name logLik
#' @aliases logLik,INLAvaan-method
#' @export
setMethod("logLik", "INLAvaan", function(object, type = c("marginal", "plugin"), ...) {
  type <- match.arg(type)
  int <- get_inlavaan_internal(object)

  if (type == "marginal") {
    return(structure(int$mloglik, class = "inlavaan_logLik"))
  }

  if (is.null(int$DIC)) {
    cli_abort(
      "{.code type = \"plugin\"} requires DIC components. Refit with
       {.code test != \"none\"}."
    )
  }
  val <- -int$DIC$Dhat / 2
  attr(val, "df") <- length(coef(object))
  attr(val, "nobs") <- nobs(object)
  class(val) <- "logLik"
  val
})

#' @exportS3Method print inlavaan_logLik
print.inlavaan_logLik <- function(x, digits = 3L, ...) {
  cat(sprintf("'log Lik.' %s (marginal)\n", format(round(unclass(x), digits))))
  cat(col_grey(paste0(
    "# ", symbol$info,
    " Laplace-approximated log evidence -- not comparable to classical\n"
  )))
  cat(col_grey(paste0(
    "# ", symbol$info,
    " logLik()/AIC()/BIC(). See `compare()` for Bayes-factor comparison.\n"
  )))
  invisible(x)
}
