call_next_lavaan_method <- function(object, ...) {
  class(object) <- "lavaan"
  callNextMethod()
}

# lavaan's own coef() method for class "lavaan" delegates to an unexported
# helper that has already been renamed once this year (lav_object_inspect_coef
# -> lav_inspect_coef, argument names dot.case -> snake_case), so calling it
# via callNextMethod() ties INLAvaan's coef() to lavaan's internal churn.
# Resolve and call the internal directly instead (positional args only, see
# lavaan-unexported.R), same as the log-likelihood/gradient call sites.
coef_inlavaan <- function(object, ...) {
  dots <- list(...)
  type <- dots$type %||% "free"
  lavaan___lav_inspect_coef(object, type, TRUE, TRUE)
}

#' @name coef
#' @rdname INLAvaan-class
#' @aliases coef,INLAvaan-method
#' @export
setMethod("coef", "INLAvaan", coef_inlavaan)

#' @name nobs
#' @rdname INLAvaan-class
#' @aliases nobs,INLAvaan-method
#' @export
setMethod("nobs", "INLAvaan", call_next_lavaan_method)
