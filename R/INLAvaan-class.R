#' Class For Representing a (Fitted) Latent Variable Model
#'
#' This is a class that extends the [lavaan-class] class. Several S4 methods are
#' available.
#'
#' @slot external A list containing an `inlavaan_internal` object.
#'
#' @param object An object of class \code{INLAvaan}.
#' @param ... Additional arguments passed to methods.
#'
#' @seealso [lavaan-class], [inlavaan()], [acfa()], [asem()], [agrowth()]
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
#'             test = "none", verbose = FALSE)
#'
#' # Print basic info
#' fit
#'
#' # Detailed summary
#' summary(fit)
#'
#' # Extract coefficients
#' coef(fit)
#' }
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
