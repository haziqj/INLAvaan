call_next_lavaan_method <- function(object, ...) {
  class(object) <- "lavaan"
  callNextMethod()
}

#' @name coef
#' @rdname INLAvaan-class
#' @aliases coef,INLAvaan-method
#' @export
setMethod("coef", "INLAvaan", call_next_lavaan_method)

#' @name nobs
#' @rdname INLAvaan-class
#' @aliases nobs,INLAvaan-method
#' @export
setMethod("nobs", "INLAvaan", call_next_lavaan_method)
