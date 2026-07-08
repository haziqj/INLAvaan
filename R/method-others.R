call_next_lavaan_method <- function(object, ...) {
  class(object) <- "lavaan"
  callNextMethod()
}

# Loading blavaan (Suggests; used side-by-side with INLAvaan in compare_mcmc()
# workflows and in this package's own tests) overwrites lavaan's own
# coef,lavaan-method in the live session with a stale copy that calls the
# unexported lav_object_inspect_coef()/add.labels()/add.class() signature,
# even when a newer lavaan (with lav_inspect_coef()/add_labels()/add_class())
# is installed and was in effect before blavaan's namespace loaded. This is a
# cross-package method-table collision, not a version-drift issue on the
# INLAvaan object itself, so no amount of faithful fit0 recomposition avoids
# it: callNextMethod() would dispatch to whichever definition is currently
# registered for "lavaan", which blavaan can silently replace out from under
# us. Resolve and call the internal directly instead (positional args only,
# see lavaan-unexported.R), same as the log-likelihood/gradient call sites.
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
