# A deprecated argument or value is an error in the tests, so that no test
# relies on an old name by accident. A test of a deprecation calls
# local_deprecation_warnings() first. The option is restored when the tests
# end.
old_deprecation <- options(inlavaan.deprecation = "error")
withr::defer(options(old_deprecation), testthat::teardown_env())

# Deprecations warn, as they do for users, until the calling test ends
local_deprecation_warnings <- function(frame = parent.frame()) {
  old <- options(inlavaan.deprecation = NULL)
  do.call(on.exit, list(bquote(options(.(old))), add = TRUE), envir = frame)
  invisible()
}
