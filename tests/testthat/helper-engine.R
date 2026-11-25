# inlavaan() fits with the engine copies (R/engine-adapter.R) when the option
# inlavaan.engine is TRUE, and with its own older stages when it is FALSE.
# local_engine() sets the option until the calling frame ends (a test_that()
# block, say), and with_engine() while `expr` is evaluated.
local_engine <- function(on = TRUE, envir = parent.frame()) {
  old <- options(inlavaan.engine = on)
  do.call(on.exit, list(bquote(options(.(old))), add = TRUE), envir = envir)
  invisible(old)
}

with_engine <- function(expr, on = TRUE) {
  old <- options(inlavaan.engine = on)
  on.exit(options(old))
  expr
}
