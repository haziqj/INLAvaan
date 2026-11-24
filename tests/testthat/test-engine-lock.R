# The engine lock. INLAvaan holds byte copies of inlavanilla's engine files,
# R/vanilla-*.R, listed with their MD5 hashes in inst/inlavanilla.lock. The
# tools that check and sync the copies live in inst/dev, which R CMD build
# leaves out, so these tests run only in a source tree (devtools::test()).
# Each check is shown to pass on a good fixture and to fail on a broken one.
# The git fixtures stand in for a clone of inlavanilla and its remote. `kit`
# is the environment of the tools.

## ----- Fixtures --------------------------------------------------------------

engine_tools <- function() {
  root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  path <- file.path(root, "inst", "dev", "engine-tools.R")
  skip_if_not(
    file.exists(path) && file.exists(file.path(root, "DESCRIPTION")),
    "the engine tools need the source tree"
  )
  env <- new.env(parent = baseenv())
  sys.source(path, envir = env)
  env$root <- root
  env
}

write_text <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(text), path)
}

fake_header <- function(
  kit,
  commit = strrep("a", 40),
  upstream = "https://example.org/inlavanilla"
) {
  c(
    Upstream = upstream,
    Release = "v26.11.23",
    Commit = commit,
    Version = "26.11.23",
    Synced = "2026-11-24",
    Hash = kit$engine_hash
  )
}

# A package folder with engine copies, other R files and a lock that lists the
# copies with their MD5 hashes
make_pkg <- function(kit, copies, others = list(), header = fake_header(kit)) {
  pkg <- tempfile("pkg-")
  write_text(
    file.path(pkg, "DESCRIPTION"),
    "Package: fixturepkg\nVersion: 0.0.0.9000\n"
  )
  write_text(file.path(pkg, "NAMESPACE"), "export(own)\n")
  for (f in names(copies)) {
    write_text(file.path(pkg, f), copies[[f]])
  }
  for (f in names(others)) {
    write_text(file.path(pkg, f), others[[f]])
  }
  files <- data.frame(
    file = as.character(names(copies)),
    md5 = kit$md5_lf(file.path(pkg, as.character(names(copies))))
  )
  kit$write_engine_lock(file.path(pkg, kit$engine_lock_file), header, files)
  pkg
}

engine_a <- "fa <- function(x) x + 1\n\nconst_a <- 2\n"
engine_b <- "fb <- function(x) fa(x) * 2\n"

skip_if_no_git <- function() {
  skip_if(!nzchar(Sys.which("git")), "needs git")
}

fixture_git <- function(kit, dir, ...) {
  kit$git(
    dir,
    c(
      "-c",
      "user.name=Fixture",
      "-c",
      "user.email=fixture@example.org",
      "-c",
      "commit.gpgsign=false",
      "-c",
      "tag.gpgsign=false",
      ...
    )
  )
}

fixture_commit <- function(kit, dir, files, message, tag = NULL) {
  for (f in names(files)) {
    write_text(file.path(dir, f), files[[f]])
  }
  fixture_git(kit, dir, "add", "--all")
  fixture_git(kit, dir, "commit", "--quiet", "-m", message)
  if (!is.null(tag)) {
    fixture_git(kit, dir, "tag", tag)
  }
  kit$git_commit_of(dir, "HEAD")
}

# A clone of a fixture of inlavanilla, with a bare repository as its remote
# origin, released as v26.11.23
make_upstream <- function(kit) {
  clone <- tempfile("upstream-")
  remote <- paste0(clone, ".git")
  dir.create(clone)
  dir.create(remote)
  kit$git(remote, c("init", "--quiet", "--bare"))
  kit$git(remote, c("symbolic-ref", "HEAD", "refs/heads/main"))
  kit$git(clone, c("init", "--quiet"))
  kit$git(clone, c("symbolic-ref", "HEAD", "refs/heads/main"))
  fixture_commit(
    kit,
    clone,
    list(
      "DESCRIPTION" = "Package: upstream\nVersion: 26.11.23\n",
      "R/vanilla-a.R" = engine_a,
      "R/vanilla-b.R" = engine_b,
      "R/methods.R" = "print_fit <- function(x) x\n"
    ),
    "Release 26.11.23",
    tag = "v26.11.23"
  )
  kit$git(clone, c("remote", "add", "origin", remote))
  kit$git(clone, c("push", "--quiet", "origin", "main", "--tags"))
  kit$git(clone, c("fetch", "--quiet", "origin"))
  list(clone = clone, remote = remote)
}

# The package under git, with a lock at v26.11.23 of the fixture upstream that
# lists no file yet
make_git_pkg <- function(kit, up, others = list()) {
  header <- fake_header(
    kit,
    kit$git_commit_of(up$clone, "v26.11.23"),
    up$remote
  )
  pkg <- make_pkg(kit, list(), others = others, header = header)
  kit$git(pkg, c("init", "--quiet"))
  fixture_commit(kit, pkg, list(), "Start")
  pkg
}

quietly <- function(expr) {
  out <- utils::capture.output(value <- expr)
  list(value = value, output = out)
}

## ----- The lock of the package -----------------------------------------------

test_that("the lock of the package is well formed", {
  kit <- engine_tools()
  lock <- kit$read_engine_lock(file.path(kit$root, kit$engine_lock_file))
  expect_identical(names(lock$header), kit$engine_header_fields)
  expect_match(lock$header[["Upstream"]], "inlavanilla$")
  expect_match(lock$header[["Release"]], kit$engine_tag_pattern)
  expect_match(lock$header[["Commit"]], "^[0-9a-f]{40}$")
  expect_true(all(grepl(kit$engine_file_pattern, lock$files$file)))
})

test_that("the engine copies of the package match the lock (check 1a)", {
  kit <- engine_tools()
  expect_identical(kit$check_engine_copy(kit$root), character())
})

test_that("no object of an engine file is defined twice in R/", {
  kit <- engine_tools()
  expect_identical(kit$check_engine_names(kit$root), character())
})

## ----- Check 1a: copy = lock -------------------------------------------------

test_that("check 1a passes on good copies and fails on broken ones", {
  kit <- engine_tools()
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a, "R/vanilla-b.R" = engine_b)
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  expect_identical(kit$check_engine_copy(pkg), character())

  # A checkout with CRLF line endings passes
  write_text(file.path(pkg, "R/vanilla-a.R"), gsub("\n", "\r\n", engine_a))
  expect_identical(kit$check_engine_copy(pkg), character())

  # An edited copy, a missing copy and an unlisted copy fail
  write_text(file.path(pkg, "R/vanilla-a.R"), sub("1", "2", engine_a))
  unlink(file.path(pkg, "R/vanilla-b.R"))
  write_text(file.path(pkg, "R/vanilla-c.R"), "fc <- function() 3\n")
  problems <- kit$check_engine_copy(pkg)
  expect_length(problems, 3)
  expect_match(
    problems[1],
    "R/vanilla-b.R is in the lock but missing",
    fixed = TRUE
  )
  expect_match(problems[2], "R/vanilla-a.R differs from the lock", fixed = TRUE)
  expect_match(problems[3], "R/vanilla-c.R is not in the lock", fixed = TRUE)
})

test_that("the MD5 on LF text drops every carriage return, as tr -d '\\r'", {
  kit <- engine_tools()
  crlf <- tempfile()
  lf <- tempfile()
  on.exit(unlink(c(crlf, lf)), add = TRUE)
  write_text(crlf, "a\r\nb\rc\n")
  write_text(lf, "a\nbc\n")
  expect_identical(kit$md5_lf(crlf), unname(tools::md5sum(lf)))
  write_text(crlf, "")
  expect_identical(kit$md5_lf(crlf), "d41d8cd98f00b204e9800998ecf8427e")
})

test_that("check 1a in the shell passes and fails as the R check does", {
  kit <- engine_tools()
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")), "needs bash")
  script <- file.path(kit$root, "inst", "dev", "engine-check-copy.sh")
  run <- function(pkg) {
    out <- suppressWarnings(system2(
      "bash",
      shQuote(c(script, pkg)),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else status, output = out)
  }
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a, "R/vanilla-b.R" = engine_b)
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  expect_identical(run(pkg)$status, 0L)
  write_text(file.path(pkg, "R/vanilla-a.R"), gsub("\n", "\r\n", engine_a))
  expect_identical(run(pkg)$status, 0L)
  write_text(file.path(pkg, "R/vanilla-a.R"), sub("1", "2", engine_a))
  res <- run(pkg)
  expect_identical(res$status, 1L)
  expect_match(
    res$output,
    "R/vanilla-a.R differs from the lock",
    fixed = TRUE,
    all = FALSE
  )
  write_text(file.path(pkg, "R/vanilla-a.R"), engine_a)
  unlink(file.path(pkg, "R/vanilla-b.R"))
  res <- run(pkg)
  expect_identical(res$status, 1L)
  expect_match(
    res$output,
    "R/vanilla-b.R is in the lock but missing",
    fixed = TRUE,
    all = FALSE
  )
  write_text(file.path(pkg, "R/vanilla-b.R"), engine_b)
  write_text(file.path(pkg, "R/vanilla-c.R"), "fc <- function() 3\n")
  res <- run(pkg)
  expect_identical(res$status, 1L)
  expect_match(
    res$output,
    "R/vanilla-c.R is not in the lock",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("a malformed lock is refused", {
  kit <- engine_tools()
  pkg <- make_pkg(kit, list("R/vanilla-a.R" = engine_a))
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  path <- file.path(pkg, kit$engine_lock_file)
  good <- readLines(path)
  writeLines(good[!startsWith(good, "Commit:")], path)
  expect_error(kit$read_engine_lock(path), "lacks the header field")
  writeLines(sub("^Hash: .*$", "Hash: sha256", good), path)
  expect_error(kit$read_engine_lock(path), "do not know")
  writeLines(sub("^File: R/vanilla-a.R$", "File: R/a.R", good), path)
  expect_error(kit$read_engine_lock(path), "not R/vanilla")
})

## ----- Duplicate names -------------------------------------------------------

test_that("the duplicate-name check finds an engine object defined twice", {
  kit <- engine_tools()
  others <- list("R/own.R" = "own <- function() fa(1)\n")
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a, "R/vanilla-b.R" = engine_b),
    others = others
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  expect_identical(kit$check_engine_names(pkg), character())

  # A function, a constant and an S4 generic of the same names as engine
  # objects, the first in a file that R loads after the engine file
  write_text(
    file.path(pkg, "R/zz-own.R"),
    paste(
      "fa = function(x) x",
      "x <- const_a <- 3",
      "setGeneric(\"fb\", function(x) standardGeneric(\"fb\"))",
      sep = "\n"
    )
  )
  problems <- kit$check_engine_names(pkg)
  expect_identical(
    problems,
    c(
      "`const_a` is defined in R/vanilla-a.R, R/zz-own.R",
      "`fa` is defined in R/vanilla-a.R, R/zz-own.R",
      "`fb` is defined in R/vanilla-b.R, R/zz-own.R"
    )
  )
})

test_that("the names check finds an engine object that masks an import", {
  kit <- engine_tools()
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a, "R/vanilla-b.R" = engine_b)
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  write_text(
    file.path(pkg, "NAMESPACE"),
    paste(
      "export(own)",
      "import(utils)",
      "import(tools, except = c(fa, fb))",
      "importFrom(stats, median, setNames)",
      "importFrom(graphics, fb = plot)",
      "importFrom(grDevices, fa, const_a)",
      "",
      sep = "\n"
    )
  )
  expect_identical(
    kit$check_engine_names(pkg),
    sprintf(
      "`%s` is defined in R/vanilla-%s.R and imported from %s in NAMESPACE",
      c("const_a", "fa", "fb"),
      c("a", "a", "b"),
      c("grDevices", "grDevices", "graphics")
    )
  )
  write_text(file.path(pkg, "NAMESPACE"), "importFrom(stats, median)\n")
  expect_identical(kit$check_engine_names(pkg), character())
})

test_that("the duplicate-name check reads bindings in blocks and calls", {
  kit <- engine_tools()
  engine_c <- paste(
    "gc <- gd <- ge <- gf <- gg <- gh <- gi <- gj <- gk <- 0",
    "gl <- function() 1",
    sep = "\n"
  )
  pkg <- make_pkg(kit, list("R/vanilla-c.R" = engine_c))
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

  # A generic called by its namespace, bindings inside { and in either branch
  # of an if, assign() and delayedAssign() with a literal name, and an
  # assignment whose value is a block. Bindings inside a function or local()
  # do not reach the namespace.
  write_text(
    file.path(pkg, "R/own.R"),
    paste(
      "methods::setGeneric(\"gc\", function(x) standardGeneric(\"gc\"))",
      "if (TRUE) {",
      "  gd <- 1",
      "} else ge <- 2",
      "{",
      "  gf <- 3",
      "}",
      "assign(\"gg\", 4)",
      "base::delayedAssign(value = 5, x = \"gh\")",
      "own <- {",
      "  gi <- 6",
      "}",
      "local({",
      "  gj <- 7",
      "})",
      "f <- function() {",
      "  gk <- 8",
      "}",
      "assign(paste0(\"g\", \"l\"), 9)",
      sep = "\n"
    )
  )
  problems <- kit$check_engine_names(pkg)
  expect_identical(
    problems,
    sprintf(
      "`%s` is defined in R/own.R, R/vanilla-c.R",
      c("gc", "gd", "ge", "gf", "gg", "gh", "gi")
    )
  )
})

## ----- Check 1b: lock = upstream ---------------------------------------------

test_that("check 1b passes on a true lock and fails on a false one", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  commit <- kit$git_commit_of(up$clone, "v26.11.23")
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a, "R/vanilla-b.R" = engine_b),
    header = fake_header(kit, commit)
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  lock_path <- file.path(pkg, kit$engine_lock_file)
  lock <- kit$read_engine_lock(lock_path)
  expect_identical(kit$check_engine_upstream(lock, up$clone), character())

  # A hash that differs from the file at Commit
  bad <- lock
  bad$files$md5[1] <- strrep("0", 32)
  expect_match(
    kit$check_engine_upstream(bad, up$clone),
    "R/vanilla-a.R has MD5 .* but the lock says 0{32}"
  )

  # A Release that resolves to another commit, and a Commit not in the clone
  bad <- lock
  fixture_commit(
    kit,
    up$clone,
    list("R/methods.R" = "print_fit <- NULL\n"),
    "Next"
  )
  bad$header[["Commit"]] <- kit$git_commit_of(up$clone, "HEAD")
  expect_match(
    kit$check_engine_upstream(bad, up$clone),
    "v26.11.23 resolves to",
    all = FALSE
  )
  bad$header[["Commit"]] <- strrep("b", 40)
  expect_match(
    kit$check_engine_upstream(bad, up$clone),
    "is not in",
    all = FALSE
  )

  # A Version that is not the upstream one
  bad <- lock
  bad$header[["Version"]] <- "26.11.24"
  expect_match(kit$check_engine_upstream(bad, up$clone), "Version is 26.11.24")
})

## ----- Check 2: not stale ----------------------------------------------------

test_that("check 2 flags a newer release that changes a copied file", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  commit <- kit$git_commit_of(up$clone, "v26.11.23")
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a),
    header = fake_header(kit, commit, up$remote)
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  lock <- kit$read_engine_lock(file.path(pkg, kit$engine_lock_file))
  check <- function(release) {
    kit$run_engine_check(pkg, up$clone, branch = "main", release = release)
  }

  # No newer release: not stale, and a file that is not copied is listed
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$stale, character())
  expect_identical(stale$not_copied, "R/vanilla-b.R")

  # A newer release that changes only an uncopied file is not stale. Newer
  # commits on main are listed only.
  fixture_commit(
    kit,
    up$clone,
    list("R/vanilla-b.R" = "fb <- NULL\n"),
    "B",
    "v26.11.30"
  )
  fixture_commit(
    kit,
    up$clone,
    list("R/vanilla-a.R" = "fa <- NULL\n"),
    "Unreleased A"
  )
  kit$git(up$clone, c("push", "--quiet", "origin", "main", "--tags"))
  kit$git(up$clone, c("fetch", "--quiet", "origin"))
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$tags, "v26.11.30")
  expect_identical(stale$stale, character())
  expect_match(stale$unreleased, "Unreleased A$")
  expect_true(quietly(check(FALSE))$value)
  expect_true(quietly(check(TRUE))$value)

  # A tag only in the clone is listed and ignored
  fixture_git(kit, up$clone, "tag", "v26.12.01")
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$tags, "v26.11.30")
  expect_identical(stale$local_only, "v26.12.01")
  expect_identical(stale$stale, character())
  res <- quietly(check(TRUE))
  expect_true(res$value)
  expect_match(
    res$output,
    "v26.12.01 is in the clone but not on Upstream: ignored",
    fixed = TRUE,
    all = FALSE
  )

  # A newer release that changes a copied file is stale: a report during
  # development, a failure at release unless NEWS.md names the tag
  kit$git(up$clone, c("push", "--quiet", "origin", "--tags"))
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$local_only, character())
  expect_identical(stale$tags, c("v26.11.30", "v26.12.01"))
  expect_identical(stale$stale, "v26.12.01")
  expect_identical(stale$changed[["v26.12.01"]], "R/vanilla-a.R")
  res <- quietly(check(FALSE))
  expect_true(res$value)
  expect_match(res$output, "^note +2 +stale: v26.12.01", all = FALSE)
  res <- quietly(check(TRUE))
  expect_false(res$value)
  expect_match(res$output, "^FAIL +2 +stale: v26.12.01", all = FALSE)
  write_text(
    file.path(pkg, "NEWS.md"),
    paste0(
      "# fixturepkg 0.4.0\n\n* The engine stays at v26.11.23: v26.12.01 ",
      "waits for a check.\n\n# fixturepkg 0.3.0\n"
    )
  )
  expect_true(quietly(check(TRUE))$value)

  # NEWS.md counts only in its first section
  write_text(
    file.path(pkg, "NEWS.md"),
    "# fixturepkg 0.4.0\n\n# fixturepkg 0.3.0\n\n* v26.12.01\n"
  )
  expect_false(quietly(check(TRUE))$value)
})

test_that("check 2 works on a lock that lists no file", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  commit <- kit$git_commit_of(up$clone, "v26.11.23")
  pkg <- make_pkg(kit, list(), header = fake_header(kit, commit, up$remote))
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  lock <- kit$read_engine_lock(file.path(pkg, kit$engine_lock_file))
  fixture_commit(
    kit,
    up$clone,
    list("R/vanilla-a.R" = "fa <- 0\n"),
    "A",
    "v26.12.01"
  )
  kit$git(up$clone, c("push", "--quiet", "origin", "--tags"))
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$tags, "v26.12.01")
  expect_identical(stale$stale, character())
  expect_identical(stale$not_copied, c("R/vanilla-a.R", "R/vanilla-b.R"))
  expect_true(
    quietly(kit$run_engine_check(pkg, up$clone, release = TRUE))$value
  )
})

test_that("check 2 cannot judge a release that the clone has not fetched", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  commit <- kit$git_commit_of(up$clone, "v26.11.23")
  pkg <- make_pkg(
    kit,
    list("R/vanilla-a.R" = engine_a),
    header = fake_header(kit, commit, up$remote)
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  lock <- kit$read_engine_lock(file.path(pkg, kit$engine_lock_file))

  # Another clone pushes a release that this clone has not fetched
  other <- tempfile("other-")
  on.exit(unlink(other, recursive = TRUE), add = TRUE)
  kit$git(tempdir(), c("clone", "--quiet", up$remote, other))
  fixture_commit(
    kit,
    other,
    list("R/vanilla-a.R" = "fa <- 0\n"),
    "A",
    "v26.12.01"
  )
  kit$git(other, c("push", "--quiet", "origin", "main", "--tags"))
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$tags, character())
  expect_identical(stale$unfetched, "v26.12.01")
  res <- quietly(kit$run_engine_check(pkg, up$clone, release = FALSE))
  expect_true(res$value)
  expect_match(res$output, "^note +2 +cannot judge v26.12.01", all = FALSE)
  res <- quietly(kit$run_engine_check(pkg, up$clone, release = TRUE))
  expect_false(res$value)
  expect_match(res$output, "^FAIL +2 +cannot judge v26.12.01", all = FALSE)

  # Once fetched, the release is judged
  kit$git(up$clone, c("fetch", "--quiet", "--tags", "origin"))
  stale <- kit$check_engine_stale(lock, up$clone)
  expect_identical(stale$stale, "v26.12.01")
  expect_identical(stale$unfetched, character())
})

test_that("release mode follows the version in DESCRIPTION", {
  kit <- engine_tools()
  expect_false(kit$is_release_version("0.3.99.9000"))
  expect_false(kit$is_release_version("0.4.0.9001"))
  expect_true(kit$is_release_version("0.4.0"))
  expect_true(kit$is_release_version("0.4.0.1"))
})

## ----- Sync ------------------------------------------------------------------

test_that("the sync copies files from a pushed tag and writes the lock", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  fixture_commit(
    kit,
    up$clone,
    list("R/vanilla-a.R" = paste0(engine_a, "ga <- function() 0\n")),
    "Add ga",
    "v26.12.01"
  )
  kit$git(up$clone, c("push", "--quiet", "origin", "main", "--tags"))
  pkg <- make_git_pkg(
    kit,
    up,
    others = list("R/own.R" = "own <- function() 1\n")
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  namespace <- tools::md5sum(file.path(pkg, "NAMESPACE"))
  sync <- function(...) {
    quietly(kit$run_engine_sync(
      ...,
      pkg = pkg,
      repo = up$clone,
      date = "2026-12-02"
    ))
  }

  # One file from v26.12.01
  res <- sync("v26.12.01", "vanilla-a.R")
  lock <- res$value$lock
  expect_identical(lock$header[["Release"]], "v26.12.01")
  expect_identical(
    lock$header[["Commit"]],
    kit$git_commit_of(up$clone, "v26.12.01")
  )
  expect_identical(lock$header[["Synced"]], "2026-12-02")
  expect_identical(lock$files$file, "R/vanilla-a.R")
  expect_identical(res$value$copy, character())
  expect_identical(res$value$names, character())
  expect_identical(kit$check_engine_upstream(lock, up$clone), character())
  expect_identical(tools::md5sum(file.path(pkg, "NAMESPACE")), namespace)
  expect_match(res$output, "Upstream log since v26.11.23", all = FALSE)
  expect_match(res$output, "Add ga$", all = FALSE)
  expect_match(res$output, "DESCRIPTION: the Imports and Suggests", all = FALSE)

  # The tree is not clean now
  expect_error(sync("v26.12.01"), "not clean")
  fixture_commit(kit, pkg, list(), "Sync engine to upstream v26.12.01")

  # Without files, every engine file at the tag
  res <- sync("v26.12.01")
  expect_identical(
    res$value$lock$files$file,
    c("R/vanilla-a.R", "R/vanilla-b.R")
  )
  expect_match(res$output, "The lock was at v26.12.01 already", all = FALSE)
  fixture_commit(kit, pkg, list(), "Add vanilla-b.R")
})

test_that("the sync refuses a dirty tree, bad tags and NAMESPACE tags", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  pkg <- make_git_pkg(kit, up)
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  sync <- function(...) {
    quietly(kit$run_engine_sync(..., pkg = pkg, repo = up$clone))
  }

  # A tag only in the clone, and a tag moved after the push
  fixture_commit(
    kit,
    up$clone,
    list("R/vanilla-b.R" = "fb <- NULL\n"),
    "B",
    "v26.12.01"
  )
  expect_error(sync("v26.12.01"), "is not on")
  kit$git(up$clone, c("push", "--quiet", "origin", "--tags"))
  fixture_commit(kit, up$clone, list("R/vanilla-b.R" = "fb <- 1\n"), "B again")
  fixture_git(kit, up$clone, "tag", "--force", "v26.12.01")
  expect_error(sync("v26.12.01"), "in the clone but")
  expect_error(sync("v99"), "no tag")

  # A tag on the remote that is not a release tag
  fixture_git(kit, up$clone, "tag", "snapshot")
  kit$git(up$clone, c("push", "--quiet", "origin", "snapshot"))
  expect_error(sync("snapshot"), "not a release tag")
  expect_false(file.exists(file.path(pkg, "R/vanilla-a.R")))

  # A file that is not an engine file
  kit$git(up$clone, c("push", "--quiet", "--force", "origin", "--tags"))
  expect_error(sync("v26.12.01", "R/methods.R"), "Not an engine file")

  # A dirty tree
  write_text(file.path(pkg, "R/new.R"), "x <- 1\n")
  expect_error(sync("v26.12.01"), "not clean")
  unlink(file.path(pkg, "R/new.R"))

  # A file with an @export tag
  fixture_commit(
    kit,
    up$clone,
    list("R/vanilla-b.R" = "#' @export\nfb <- 1\n"),
    "Export fb",
    "v26.12.02"
  )
  kit$git(up$clone, c("push", "--quiet", "origin", "--tags"))
  expect_error(sync("v26.12.02"), "would change NAMESPACE")
  expect_false(file.exists(file.path(pkg, "R/vanilla-b.R")))
})

test_that("the sync reports a name that the package still defines", {
  kit <- engine_tools()
  skip_if_no_git()
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  pkg <- make_git_pkg(
    kit,
    up,
    others = list("R/own.R" = "fa <- function() 1\n")
  )
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  res <- quietly(kit$run_engine_sync("v26.11.23", pkg = pkg, repo = up$clone))
  expect_identical(res$value$names, "`fa` is defined in R/own.R, R/vanilla-a.R")
  expect_match(res$output, "^FAIL +names", all = FALSE)
})

## ----- The scripts -----------------------------------------------------------

test_that("engine-check.R and engine-sync.R run from the shell", {
  kit <- engine_tools()
  skip_if_no_git()
  rscript <- file.path(R.home("bin"), "Rscript")

  # The scripts, the package and the clone sit in folders whose names have a
  # space, as a Windows home folder may
  spaced <- file.path(tempfile("spaced-"), "a folder")
  dev <- file.path(spaced, "inst dev")
  dir.create(dev, recursive = TRUE)
  on.exit(unlink(dirname(spaced), recursive = TRUE), add = TRUE)
  file.copy(
    file.path(
      kit$root,
      "inst",
      "dev",
      c("engine-tools.R", "engine-check.R", "engine-sync.R")
    ),
    dev
  )
  run <- function(script, args) {
    out <- suppressWarnings(system2(
      rscript,
      shQuote(c(file.path(dev, script), args)),
      stdout = TRUE,
      stderr = TRUE
    ))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else status, output = out)
  }
  up <- make_upstream(kit)
  on.exit(unlink(c(up$clone, up$remote), recursive = TRUE), add = TRUE)
  pkg <- make_git_pkg(kit, up)
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  file.rename(up$clone, file.path(spaced, "upstream clone"))
  up$clone <- file.path(spaced, "upstream clone")
  file.rename(pkg, file.path(spaced, "fixture pkg"))
  pkg <- file.path(spaced, "fixture pkg")

  res <- run(
    "engine-sync.R",
    c(
      "v26.11.23",
      "vanilla-a.R",
      paste0("--pkg=", pkg),
      paste0("--repo=", up$clone)
    )
  )
  expect_identical(res$status, 0L)
  expect_match(res$output, "Synced 1 file", all = FALSE)
  res <- run(
    "engine-check.R",
    c(paste0("--pkg=", pkg), paste0("--repo=", up$clone))
  )
  expect_identical(res$status, 0L)
  expect_match(res$output, "^ok +1b", all = FALSE)

  # An edited copy fails, and so do checks 1b and 2 asked for without a clone
  write_text(file.path(pkg, "R/vanilla-a.R"), "fa <- 0\n")
  res <- run("engine-check.R", c(paste0("--pkg=", pkg), "--checks=copy"))
  expect_identical(res$status, 1L)
  expect_match(res$output, "^FAIL +1a", all = FALSE)
  res <- run("engine-check.R", c(paste0("--pkg=", pkg), "--checks=upstream"))
  expect_identical(res$status, 1L)
})
