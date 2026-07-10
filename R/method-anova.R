#' Model Comparison Is Not Supported via anova() for INLAvaan Models
#'
#' \pkg{lavaan}'s \code{anova()} performs a classical likelihood-ratio test
#' (\code{\link[lavaan]{lavTestLRT}}), calibrated against a chi-square
#' reference distribution under maximum-likelihood asymptotics. There is no
#' direct Bayesian analogue of that test, so this method is disabled for
#' \code{INLAvaan} fits: it always throws an error pointing to [compare()].
#'
#' @param object An object of class [INLAvaan].
#' @param ... Currently unused.
#'
#' @returns Never returns; always throws an error.
#'
#' @details
#' Unlike \code{fitted()}/\code{residuals()}/\code{predict()}, this is a
#' deliberate departure from \pkg{blavaan}, which inherits \pkg{lavaan}'s
#' \code{anova()} unchanged (it performs the same LRT on the posterior-mean
#' point estimate, without comment on its interpretation). INLAvaan instead
#' redirects to [compare()], which reports Bayes factors, DIC/pD, and
#' (optionally) LOO/WAIC -- the Bayesian tools for asking "is this model
#' better than that one", including for models that are not nested, which
#' the classical LRT cannot handle at all.
#'
#' @seealso [compare()]
#'
#' @importFrom stats anova
#' @name anova
#' @aliases anova,INLAvaan-method
#' @export
setMethod("anova", "INLAvaan", function(object, ...) {
  cli_abort(
    "{.fn anova} performs a classical likelihood-ratio test, which has no
     direct Bayesian analogue under the Laplace/posterior-mean approximation
     used here. Use {.fn compare} instead, which reports Bayes factors,
     DIC/pD, and (optionally) LOO/WAIC comparisons."
  )
})
