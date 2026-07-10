#' @importFrom stats anova
#' @name anova
#' @rdname compare
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
