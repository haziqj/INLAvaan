#' Timing Information for INLAvaan Models
#'
#' Extract wall-clock timings for individual computation stages of a fitted
#' \code{INLAvaan} model.
#'
#' @param object An object of class [INLAvaan].
#' @param what Character vector of timing segment names to return, or
#'   \code{"all"} to return every segment. Defaults to \code{"total"}.
#'   Available segments (depending on model options): \code{"init"},
#'   \code{"optim"}, \code{"vb"}, \code{"loglik"}, \code{"marginals"},
#'   \code{"norta"}, \code{"sampling"}, \code{"covariances"},
#'   \code{"definedpars"}, \code{"deltapars"}, \code{"test"}, \code{"total"}.
#' @param ... Currently unused.
#'
#' @returns A named numeric vector (class \code{c("timing.INLAvaan",
#'   "numeric")}) of elapsed times in seconds. Printing formats short
#'   durations as seconds, longer ones as minutes or hours.
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
#' # Total elapsed time
#' timing(fit)
#'
#' # All stages
#' timing(fit, what = "all")
#'
#' # Specific stages
#' timing(fit, what = c("optim", "marginals"))
#' }
#'
#' @seealso [diagnostics()], [summary()]
#'
#' @export
setGeneric("timing", function(object, ...) standardGeneric("timing"))

#' @name timing
#' @rdname timing
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
    if (is.na(s)) {
      return("NA")
    }
    if (s < 60) {
      return(sprintf("%.2f s", s))
    }
    if (s < 3600) { # nocov start
      return(sprintf("%.1f min", s / 60))
    }
    sprintf("%.2f hr", s / 3600) # nocov end
  }
  vals <- vapply(x, fmt_time, character(1))
  names(vals) <- names(x)
  print(vals, quote = FALSE, right = TRUE)
  invisible(x)
}
