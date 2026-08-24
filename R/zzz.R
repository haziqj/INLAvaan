.onLoad <- function(libname, pkgname) {
  # Bind the unexported lavaan internals once per session; see
  # R/lavaan-unexported.R for why this happens at load time rather than
  # through `:::` at the call sites.
  resolve_lavaan_internals(asNamespace(pkgname))
}
