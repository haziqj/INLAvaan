#' Class For Representing a (Fitted) Latent Variable Model
#'
#' This is a class that extends the [lavaan-class] class. Several S4 methods are
#' available.
#'
#' @slot external A list containing an `inlavaan_internal` object.
#'
#' @seealso [lavaan-class]
#'
#' @name INLAvaan-class
#' @rdname INLAvaan-class
#'
#' @importClassesFrom lavaan lavaan
#' @export
setClass(
  Class = "INLAvaan",
  contains = "lavaan"
)
