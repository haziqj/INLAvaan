#' Timing Information for INLAvaan Models
#'
#' Extract wall-clock timings for individual computation stages of a fitted
#' \code{INLAvaan} model.
#'
#' @param object An object of class [INLAvaan].
#' @param what Character vector of timing segment names to return, or
#'   \code{"all"} to return every segment. Defaults to \code{"total"}.
#'   Available segments (depending on model options): \code{"init"} (which
#'   includes the lavaan model setup), \code{"optim"}, \code{"vb"},
#'   \code{"loglik"}, \code{"marginals"}, \code{"norta"}, \code{"sampling"},
#'   \code{"covariances"}, \code{"definedpars"}, \code{"deltapars"},
#'   \code{"test"}, \code{"loo"}, \code{"waic"}, \code{"total"}. The
#'   segments are disjoint and \code{"total"} is their sum. \code{"loo"} and
#'   \code{"waic"} are only recorded when the fit-time LOO actually ran (see
#'   [loo()]); requesting either when it did not gives an error explaining
#'   why, distinct from requesting a misspelled segment name.
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

# "loo" and "waic" are the only segments not always recorded: they are timed
# only when the fit-time LOO actually runs (test != "none", a casewise-
# supported model, and the predicted cost within budget -- see inlavaan.R).
# Requesting one that is absent therefore needs a message distinct from a
# genuinely unknown/misspelled segment name.
timing_conditional_segments <- c("loo", "waic")

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
        not_run <- intersect(unknown, timing_conditional_segments)
        misspelled <- setdiff(unknown, not_run)
        msg <- c(
          if (length(misspelled) > 0L) {
            c(
              "x" = "Unknown timing segment{qty(length(misspelled))}{?s}: {.val {misspelled}}."
            )
          },
          if (length(not_run) > 0L) {
            c(
              "x" = "{.val {not_run}} {qty(length(not_run))}{?was/were} not
                     computed for this fit.",
              "i" = "LOO/WAIC only run at fit time when {.code test != \"none\"},
                     the model supports casewise LOO, and the predicted cost
                     fits the time budget; compute {qty(length(not_run))}{?it/them}
                     post hoc with {.fn loo}/{.fn waic} or {.fn add_loo}."
            )
          },
          "i" = "Available: {.val {available}}."
        )
        cli_abort(msg)
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
