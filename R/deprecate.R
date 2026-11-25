# Deprecated arguments and values. INLAvaan keeps an old argument or value
# working for one minor version: it warns once per session for each, and the
# help marks it with a "deprecated" badge. In the next minor version it stops.
# This follows the lifecycle package of the tidyverse, but uses cli alone, so
# that INLAvaan does not depend on lifecycle or rlang.

## ----- Help badge ------------------------------------------------------------

# The Rd markup of the badge, the same as lifecycle::badge("deprecated") writes.
# Start the text of a deprecated @param with `r badge_deprecated()`. Roxygen
# evaluates inline code at document() time in an environment whose parent is
# the package namespace, so the helper need not be exported. HTML help and the
# pkgdown site show the image man/figures/lifecycle-deprecated.svg, and text
# help shows "[Deprecated]". The image is copied unchanged from the lifecycle
# package (MIT licence), at
# https://github.com/r-lib/lifecycle/blob/main/man/figures/lifecycle-deprecated.svg,
# the file that usethis::use_lifecycle() copies into a package.
badge_deprecated <- function() {
  paste0(
    "\\ifelse{html}{\\href{https://lifecycle.r-lib.org/articles/stages.html",
    "#deprecated}{\\figure{lifecycle-deprecated.svg}",
    "{options: alt='[Deprecated]'}}}{\\strong{[Deprecated]}}"
  )
}

## ----- Warning ---------------------------------------------------------------

# The ids of the deprecations that have warned in this session
deprecation_seen <- new.env(parent = emptyenv())

# Warn that an argument or a value is deprecated, once per session for each
# `id`. The condition has the class inlavaan_deprecated, after any more
# specific `class`. `message` is a cli template, filled in from `.envir`. With
# options(inlavaan.deprecation = "error"), every call is an error of the same
# classes instead, so that tests find old calls. Under `silent = TRUE` the fit
# muffles the warning, so it is not marked as seen, and it warns again at the
# next call that is not silent. Returns TRUE, invisibly, when it warns.
warn_deprecated <- function(
  id,
  message,
  class = NULL,
  silent = FALSE,
  .envir = parent.frame()
) {
  class <- c(class, "inlavaan_deprecated")
  if (identical(getOption("inlavaan.deprecation"), "error")) {
    cli_abort(message, class = class, call = NULL, .envir = .envir)
  }
  if (isTRUE(deprecation_seen[[id]])) {
    return(invisible(FALSE))
  }
  if (!isTRUE(silent)) {
    deprecation_seen[[id]] <- TRUE
  }
  cli_warn(
    c(message, "i" = "This warning is shown once per session."),
    class = class,
    .envir = .envir
  )
  invisible(TRUE)
}
