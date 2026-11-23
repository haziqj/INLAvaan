# The public interface: what NAMESPACE exports and registers, and the arguments
# of each exported function. Any change to it changes a snapshot, so it shows
# up in review. Accept a deliberate change with
# testthat::snapshot_accept("interface"). Sorting is by radix, so the order
# does not depend on the locale.

# The NAMESPACE file sits in the package root, both in an installed package
# and in a source tree under devtools::load_all(). The root folder of a source
# tree can have another name (a worktree, for example).
pkg_namespace_info <- function() {
  root <- dirname(system.file("NAMESPACE", package = "INLAvaan"))
  parseNamespaceFile(basename(root), dirname(root))
}

cat_sorted <- function(title, x) {
  cat(title, sort(x, method = "radix"), sep = "\n  ")
  cat("\n")
}

test_that("the exports and method registrations are stable", {
  info <- pkg_namespace_info()
  s3 <- paste0(info$S3methods[, 1], ".", info$S3methods[, 2])
  expect_snapshot({
    cat_sorted("Exported objects:", info$exports)
    cat_sorted("S3 methods:", s3)
    cat_sorted("S4 classes:", info$exportClasses)
    cat_sorted("S4 methods for:", info$exportMethods)
  })
})

test_that("the arguments of the exported functions are stable", {
  ns <- asNamespace("INLAvaan")
  exports <- sort(pkg_namespace_info()$exports, method = "radix")
  is_fun <- vapply(
    exports,
    function(x) is.function(get(x, envir = ns)),
    logical(1)
  )
  signature <- function(name) {
    fmls <- formals(get(name, envir = ns))
    args <- vapply(
      names(fmls),
      function(arg) {
        if (identical(fmls[[arg]], quote(expr = ))) {
          return(arg)
        }
        paste(arg, "=", paste(deparse(fmls[[arg]]), collapse = " "))
      },
      character(1)
    )
    paste0(name, "(", paste(args, collapse = ", "), ")")
  }
  expect_snapshot(
    cat(vapply(exports[is_fun], signature, character(1)), sep = "\n")
  )
})
