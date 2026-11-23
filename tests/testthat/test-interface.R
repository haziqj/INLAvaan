# The public interface: what NAMESPACE exports and registers, and the arguments
# of each exported function and of each S4 and S3 method. Any change to it
# changes a snapshot, so it shows up in review. Accept a deliberate change with
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

# One line per function: name(arg, arg = default, ...)
signature_line <- function(name, fmls) {
  args <- vapply(
    names(fmls),
    function(arg) {
      if (identical(fmls[[arg]], quote(expr = ))) {
        return(arg)
      }
      paste(arg, "=", paste(trimws(deparse(fmls[[arg]])), collapse = " "))
    },
    character(1)
  )
  paste0(name, "(", paste(args, collapse = ", "), ")")
}

# An S4 method with arguments beyond those of its generic keeps them in an
# inner function `.local`. Those are the arguments a user sees.
s4_method_formals <- function(def) {
  b <- body(def)
  if (is.call(b) && identical(b[[1]], as.name("{")) && length(b) >= 2) {
    first <- b[[2]]
    if (
      is.call(first) &&
        identical(first[[1]], as.name("<-")) &&
        identical(first[[2]], as.name(".local"))
    ) {
      return(formals(eval(first[[3]])))
    }
  }
  formals(def)
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
  lines <- vapply(
    exports[is_fun],
    function(x) signature_line(x, formals(get(x, envir = ns))),
    character(1)
  )
  expect_snapshot(cat(lines, sep = "\n"))
})

test_that("the arguments of the S4 methods for INLAvaan are stable", {
  generics <- sort(pkg_namespace_info()$exportMethods, method = "radix")
  lines <- vapply(
    generics,
    function(g) {
      def <- methods::selectMethod(g, "INLAvaan")
      signature_line(g, s4_method_formals(def))
    },
    character(1)
  )
  expect_snapshot(cat(lines, sep = "\n"))
})

test_that("the arguments of the S3 methods are stable", {
  ns <- asNamespace("INLAvaan")
  s3 <- pkg_namespace_info()$S3methods
  names <- paste0(s3[, 1], ".", s3[, 2])
  funs <- ifelse(is.na(s3[, 3]), names, s3[, 3])
  ord <- order(names, method = "radix")
  lines <- vapply(
    ord,
    function(i) signature_line(names[i], formals(get(funs[i], envir = ns))),
    character(1)
  )
  expect_snapshot(cat(lines, sep = "\n"))
})
