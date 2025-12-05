#' @rdname INLAvaan-class
#' @param object An object of class [INLAvaan].
#' @export
setMethod("coef", "INLAvaan", function(object) {
  class(object) <- "lavaan"
  callNextMethod()
})
