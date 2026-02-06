#' Specify priors for a SEM
#'
#' Specify priors for a SEM, similar to how [blavaan::dpriors()] works.
#'
#' This function provides a convenient way to specify prior distributions for
#' different types of parameters in a structural equation model (SEM). It uses a
#' registry of default priors for common lavaan parameter types (e.g., loadings,
#' regressions, residuals, etc.) and allows users to override these defaults by
#' passing named arguments.
#'
#' The parameter names, and default settings, are:
#' \itemize{
#'   \item \code{nu = "normal(0,32)"}: Observed variable intercepts
#'   \item \code{alpha = "normal(0,10)"}: Latent variable intercepts
#'   \item \code{lambda = "normal(0,10)"}: Factor loadings
#'   \item \code{beta = "normal(0,10)"}: Regression coefficients
#'   \item \code{theta = "gamma(1,.5)[sd]"}: Residual precisions
#'   \item \code{psi = "gamma(1,.5)[sd]"}: Latent variable precisions
#'   \item \code{rho = "beta(1,1)"}: Correlations (both latent and observed)
#'   \item \code{tau = "normal(0,1.5)"}: Thresholds for ordinal variables
#' }
#'
#' Note that the normal distributions are parameterised using standard
#' deviations, and not variances. For example, \code{normal(0,10)} means a
#' normal distribution with mean 0 and standard deviation 10 (not variance 10).
#'
#' @param ... Named arguments specifying prior distributions for lavaan
#'   parameter types.
#'
#' @returns A named character vector of prior specifications, where names
#'   correspond to lavaan parameter types (e.g., "lambda", "beta", "theta",
#'   etc.) and values are character strings specifying the prior distribution
#'   (e.g., \code{"normal(0,10)"}, \code{"gamma(1,0.5)[sd]"}, etc.).
#' @export
#'
#' @examples
#' dpriors(nu = "normal(0,10)", lambda = "normal(0,1)", rho = "beta(3,3)")
priors_for <- function(...) {
  userspec <- list(...)

  # Ensure the user didn't just pass strings without names
  if (length(userspec) > 0 && is.null(names(userspec))) {
    cli::cli_abort(
      "priors_for ERROR: All arguments must be named (e.g., `lambda = 'normal(0,1)'`)"
    )
  }

  # The prior distribution dictionary
  # fmt: skip
  out <- list(
    nu     = "normal(0,32)",     # Intercepts
    alpha  = "normal(0,10)",     # Latent intercepts
    lambda = "normal(0,10)",     # Loadings
    beta   = "normal(0,10)",     # Regressions
    theta  = "gamma(1,.5)[sd]",  # Residual precisions
    psi    = "gamma(1,.5)[sd]",  # Latent precisions
    rho    = "beta(1,1)",        # Correlations
    tau    = "normal(0,1.5)"     # Thresholds
  )

  # Merge: user specifications overwrite the registry
  if (length(userspec) > 0) {
    for (name in names(userspec)) {
      out[[name]] <- userspec[[name]]
    }
  }

  unlist(out)
}
