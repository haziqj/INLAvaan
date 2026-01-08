.onLoad <- function(libname, pkgname) {
  # This warms up the qrng/spacefillr connection quietly
  if (requireNamespace("qrng", quietly = TRUE)) {
    suppressPackageStartupMessages({
      loadNamespace("qrng")
      if (requireNamespace("spacefillr", quietly = TRUE)) {
        loadNamespace("spacefillr")
      }
    })
  }

  invisible()
}
