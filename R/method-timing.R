#' @rdname INLAvaan-class
#' @param what Character vector of timing segment names to return, or
#'   `"all"` to return every segment. Defaults to `"total"`. Available
#'   segments (depending on model options): `"init"`, `"optim"`, `"vb"`,
#'   `"loglik"`, `"marginals"`, `"norta"`, `"sampling"`, `"covariances"`,
#'   `"definedpars"`, `"deltapars"`, `"test"`, `"total"`.
#' @export
setGeneric("timing", function(object, ...) standardGeneric("timing"))

#' @rdname INLAvaan-class
#' @aliases timing,INLAvaan-method
#' @export
setMethod(
  "timing",
  "INLAvaan",
  function(object, what = "total", ...) {
    t <- object@timing
    available <- names(t)

    if (identical(what, "all")) {
      what <- available
    } else {
      unknown <- setdiff(what, available)
      if (length(unknown) > 0L) {
        cli_abort(c(
          "Unknown timing segment{?s}: {.val {unknown}}.",
          "i" = "Available: {.val {available}}."
        ))
      }
    }

    out <- unlist(t[what])
    names(out) <- what
    class(out) <- c("timing.INLAvaan", "numeric")
    out
  }
)

#' @exportS3Method print timing.INLAvaan
print.timing.INLAvaan <- function(x, ...) {
  fmt_time <- function(s) {
    if (is.na(s)) return("NA")
    if (s < 60)   return(sprintf("%.2f s", s))
    if (s < 3600) return(sprintf("%.1f min", s / 60))
    sprintf("%.2f hr", s / 3600)
  }
  vals <- vapply(x, fmt_time, character(1))
  names(vals) <- names(x)
  print(vals, quote = FALSE, right = TRUE)
  invisible(x)
}
