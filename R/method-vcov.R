#' @rdname INLAvaan-class
#' @export
setMethod("vcov", "INLAvaan", function(object) {
  class(object) <- "lavaan"
  cli::cli_alert_info(
    "{.fun vcov} is showing the covariance matrix of the Laplace approximation of the joint posterior."
  )
  callNextMethod()
})
