#' INLAvaan Class
#'
#' This is a class that extends the lavaan class.
#'
#' @importFrom lavaan lavaan
#' @export
# @slot mySlot A character vector.
setClass("INLAvaan",
         # slots = list(
         #   mySlot = "character"  # Replace with your actual slot definitions
         # ),
         contains = "lavaan"
)
