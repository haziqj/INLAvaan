#' Deviance for INLAvaan Models
#'
#' Extract the (Bayesian) deviance of a fitted \code{INLAvaan} model. Unlike
#' \pkg{lavaan}, which has no \code{deviance()} method, this follows the
#' BUGS/JAGS/Stan convention: "deviance" is \eqn{-2} times the log-likelihood,
#' summarised over the posterior.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character. \code{"mean"} (default) returns the posterior mean
#'   deviance \eqn{\bar{D} = E[-2\log p(y \mid \theta)]}, averaged over
#'   posterior draws. \code{"plugin"} returns the deviance evaluated at the
#'   posterior mean point estimate, \eqn{\hat{D} = -2\log p(y \mid \hat\theta)}
#'   (matching \code{-2 * logLik(object, type = "plugin")}). Both require the
#'   model to have been fitted with \code{test != "none"}.
#' @param ... Currently unused.
#'
#' @returns A length-one numeric of class \code{inlavaan_deviance}, with the
#'   effective number of parameters (\code{pD}) and \code{DIC} attached as
#'   attributes.
#'
#' @details
#' \eqn{\bar{D}} and \eqn{\hat{D}} are the two ingredients of the Deviance
#' Information Criterion, \eqn{DIC = \bar{D} + p_D} where
#' \eqn{p_D = \bar{D} - \hat{D}} is the effective number of parameters. Use
#' [compare()] to compare models by DIC (or Bayes factors, or LOO/WAIC)
#' rather than comparing raw deviances directly.
#'
#' @seealso [logLik()], [compare()]
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
#' deviance(fit)
#' attr(deviance(fit), "DIC")
#' }
#'
#' @importFrom stats deviance
#' @exportS3Method deviance INLAvaan
#' @name deviance
deviance.INLAvaan <- function(object, type = c("mean", "plugin"), ...) {
  type <- match.arg(type)
  int <- get_inlavaan_internal(object)
  if (is.null(int$DIC)) {
    cli_abort(
      "{.fn deviance} requires DIC components. Refit with
       {.code test != \"none\"}."
    )
  }
  val <- if (type == "mean") int$DIC$Dbar else int$DIC$Dhat
  structure(
    val,
    pD = int$DIC$pD,
    DIC = int$DIC$dic,
    class = "inlavaan_deviance"
  )
}

#' @exportS3Method print inlavaan_deviance
print.inlavaan_deviance <- function(x, digits = 3L, ...) {
  cat(sprintf("Deviance: %s\n", format(round(unclass(x), digits))))
  cat(col_grey(paste0(
    "# ", symbol$info,
    " pD = ", format(round(attr(x, "pD"), digits)),
    ", DIC = ", format(round(attr(x, "DIC"), digits)), "\n"
  )))
  invisible(x)
}
