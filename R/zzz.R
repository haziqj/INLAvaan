.onLoad <- function(libname, pkgname) {
  # Resolve unexported lavaan internals once per session; see
  # R/lavaan-unexported.R for why this cannot happen at install time.
  resolve_lavaan_internals(asNamespace(pkgname))

  # Resolve which spelling the installed lavaan uses for every renamed
  # argument INLAvaan sets, once per session; see R/lavaan-argnames.R for why
  # this cannot happen at install time either. Must precede the fitMeasures
  # registration below, which consumes the resolved map.
  resolve_lavaan_argnames(asNamespace(pkgname))

  # Register fitMeasures()/fitmeasures() against whichever lavaan generic is
  # active this session; see R/method-fitmeasures.R for why this cannot
  # happen at install time either.
  register_fitmeasures_methods(asNamespace(pkgname))
}

# nocov start
# .onLoad_qrng_warmup <- function(libname, pkgname) {
#   # This warms up the qrng/spacefillr connection quietly
#   if (requireNamespace("qrng", quietly = TRUE)) {
#     suppressPackageStartupMessages({
#       loadNamespace("qrng")
#       if (requireNamespace("spacefillr", quietly = TRUE)) {
#         loadNamespace("spacefillr")
#       }
#     })
#   }
#
#   invisible()
# }
# nocov end
