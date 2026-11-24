################################################################################
#
# Tools for a package that holds byte copies of engine files of inlavanilla
# (https://github.com/haziqj/inlavanilla). The copies are the files
# R/vanilla-*.R, each listed with its MD5 hash in inst/inlavanilla.lock. The
# tools read and write the lock, check the copies against it (check 1a), check
# that no object of an engine file is also defined in another file of R/, check
# the lock against a clone of inlavanilla (check 1b), report whether a newer
# release of inlavanilla changes a copied file (check 2), and sync the copies
# from a release tag. engine-check.R and engine-sync.R run them from the shell,
# and tests/testthat/test-engine-lock.R sources this file. They need base R
# (>= 4.1.0) and git, and read the name of the package that holds the copies
# from its DESCRIPTION, so any package can use them.
#
################################################################################

## ----- Constants -------------------------------------------------------------

engine_lock_file <- file.path("inst", "inlavanilla.lock")
engine_file_pattern <- "^R/vanilla-[^/]+[.]R$"
engine_hash <- "md5 of the text with LF line endings"
engine_header_fields <- c(
  "Upstream",
  "Release",
  "Commit",
  "Version",
  "Synced",
  "Hash"
)
engine_repo_env <- "INLAVANILLA_REPO"
engine_tag_pattern <- "^v?[0-9]+([.-][0-9]+)*$"

# Roxygen tags that would change NAMESPACE, or the collation order through
# @include, if a copied file held them
engine_roxygen_ns <- paste0(
  "^\\s*#'.*@",
  "(export|import|useDynLib|rawNamespace|S3method|include)"
)

## ----- The lock file ---------------------------------------------------------

# The lock is DCF: a header record, then one record per copied file with the
# fields File and MD5. Returns list(header = named character, files =
# data.frame(file, md5)).
read_engine_lock <- function(path) {
  if (!file.exists(path)) {
    stop("There is no lock file at ", path, call. = FALSE)
  }
  bad <- function(...) stop("The lock ", path, " ", ..., call. = FALSE)
  dcf <- read.dcf(path)
  header <- dcf[1, ]
  header <- header[!is.na(header)]
  missing <- setdiff(engine_header_fields, names(header))
  if (length(missing) > 0) {
    bad("lacks the header field(s) ", paste(missing, collapse = ", "))
  }
  if (any(c("File", "MD5") %in% names(header))) {
    bad("has a File or MD5 field in its header record")
  }
  if (!grepl("^[0-9a-f]{40}$", header[["Commit"]])) {
    bad("has a Commit that is not a 40-character SHA")
  }
  if (!identical(header[["Hash"]], engine_hash)) {
    bad("has 'Hash: ", header[["Hash"]], "', which these tools do not know")
  }
  files <- data.frame(file = character(), md5 = character())
  records <- dcf[-1, , drop = FALSE]
  if (nrow(records) > 0) {
    if (!all(c("File", "MD5") %in% colnames(records))) {
      bad("has a file record without File or MD5")
    }
    in_header <- intersect(engine_header_fields, colnames(records))
    if (any(!is.na(records[, in_header]))) {
      bad("has a header field in a file record")
    }
    files <- data.frame(
      file = unname(records[, "File"]),
      md5 = unname(records[, "MD5"])
    )
    if (anyNA(files$file) || anyNA(files$md5)) {
      bad("has a file record without File or MD5")
    }
    if (!all(grepl(engine_file_pattern, files$file))) {
      bad("lists a file that is not R/vanilla-*.R")
    }
    if (!all(grepl("^[0-9a-f]{32}$", files$md5))) {
      bad("has an MD5 that is not 32 hexadecimal characters")
    }
    if (anyDuplicated(files$file) > 0) {
      bad("lists a file twice")
    }
  }
  list(header = header[engine_header_fields], files = files)
}

# Written with LF line endings on every platform, files in radix order
write_engine_lock <- function(path, header, files) {
  files <- files[order(files$file, method = "radix"), , drop = FALSE]
  lines <- paste0(engine_header_fields, ": ", header[engine_header_fields])
  for (i in seq_len(nrow(files))) {
    lines <- c(
      lines,
      "",
      paste0("File: ", files$file[i]),
      paste0("MD5: ", files$md5[i])
    )
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(lines, con, sep = "\n")
  invisible(path)
}

upstream_name <- function(lock) {
  sub("[.]git$", "", basename(lock$header[["Upstream"]]))
}

short_sha <- function(sha) substr(sha, 1, 7)

## ----- Hashes on LF text -----------------------------------------------------

# The MD5 of a file with every carriage return removed, as the shell step
# `tr -d '\r' < file | md5sum` computes it, so that a checkout with CRLF line
# endings passes. tools::md5sum() reads files only (its `bytes` argument needs
# R 4.5.0), hence the temporary copy.
md5_lf <- function(paths) {
  vapply(paths, md5_lf_one, character(1), USE.NAMES = FALSE)
}

md5_lf_one <- function(path) {
  bytes <- readBin(path, "raw", n = file.size(path))
  tmp <- tempfile("md5-lf-")
  on.exit(unlink(tmp))
  writeBin(bytes[bytes != as.raw(13L)], tmp)
  unname(tools::md5sum(tmp))
}

## ----- Package helpers -------------------------------------------------------

find_package_root <- function(path = getwd()) {
  path <- normalizePath(path, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop(
        "There is no DESCRIPTION file here or above: run from the package ",
        "root or pass --pkg",
        call. = FALSE
      )
    }
    path <- parent
  }
}

package_desc <- function(pkg) {
  desc <- read.dcf(
    file.path(pkg, "DESCRIPTION"),
    fields = c("Package", "Version")
  )
  desc[1, ]
}

engine_copies <- function(pkg) {
  files <- list.files(file.path(pkg, "R"), pattern = "^vanilla-.+[.]R$")
  file.path("R", sort(files, method = "radix"))
}

## ----- Check 1a: copy = lock -------------------------------------------------

# Fails when a listed file is missing or differs from its MD5, or when an
# R/vanilla-*.R file is not listed. Returns the problems, character(0) if none.
check_engine_copy <- function(pkg, lock = NULL) {
  if (is.null(lock)) {
    lock <- read_engine_lock(file.path(pkg, engine_lock_file))
  }
  listed <- lock$files$file
  present <- file.exists(file.path(pkg, listed))
  problems <- sprintf("%s is in the lock but missing", listed[!present])
  if (any(present)) {
    got <- md5_lf(file.path(pkg, listed[present]))
    want <- lock$files$md5[present]
    differ <- got != want
    problems <- c(
      problems,
      sprintf(
        "%s differs from the lock (MD5 %s, lock %s)",
        listed[present][differ],
        got[differ],
        want[differ]
      )
    )
  }
  unlisted <- setdiff(engine_copies(pkg), listed)
  c(problems, sprintf("%s is not in the lock", unlisted))
}

## ----- Duplicate names -------------------------------------------------------

# Names bound at the top level of an R file: assignments with <-, = or <<-
# (chains such as a <- b <- f included), and setGeneric("name", ...).
top_level_names <- function(path) {
  exprs <- parse(path, keep.source = FALSE, encoding = "UTF-8")
  assign_ops <- c("<-", "=", "<<-")
  out <- character()
  for (e in exprs) {
    while (is.call(e) && as.character(e[[1]])[1] %in% assign_ops) {
      lhs <- e[[2]]
      if (is.name(lhs) || (is.character(lhs) && length(lhs) == 1)) {
        out <- c(out, as.character(lhs))
      }
      e <- e[[3]]
    }
    if (is.call(e) && identical(e[[1]], as.name("setGeneric"))) {
      args <- as.list(e)[-1]
      name <- if (!is.null(args$name)) args$name else args[[1]]
      if (is.character(name) && length(name) == 1) {
        out <- c(out, name)
      }
    }
  }
  unique(out)
}

# Without a Collate field, R loads R/ in alphabetical order, and an object
# defined in two files silently takes the later definition. Fails when a name
# bound in an R/vanilla-*.R file is bound in any other file of R/.
check_engine_names <- function(pkg) {
  files <- list.files(file.path(pkg, "R"), pattern = "[.][RrSsQq]$")
  defs <- lapply(file.path(pkg, "R", files), top_level_names)
  where <- data.frame(
    name = as.character(unlist(defs)),
    file = rep(file.path("R", files), lengths(defs))
  )
  engine_names <- unique(where$name[grepl(engine_file_pattern, where$file)])
  problems <- character()
  for (name in sort(engine_names, method = "radix")) {
    in_files <- where$file[where$name == name]
    if (length(in_files) > 1) {
      problems <- c(
        problems,
        sprintf("`%s` is defined in %s", name, paste(in_files, collapse = ", "))
      )
    }
  }
  problems
}

## ----- Git helpers -----------------------------------------------------------

# Runs git in `repo` and returns its standard output. A failure stops, or
# returns NULL with error = FALSE.
git <- function(repo, args, error = TRUE) {
  err <- tempfile("git-stderr-")
  on.exit(unlink(err))
  out <- suppressWarnings(system2(
    "git",
    shQuote(c("-C", repo, args)),
    stdout = TRUE,
    stderr = err
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    if (!error) {
      return(NULL)
    }
    stop(
      "git ",
      paste(args, collapse = " "),
      " failed in ",
      repo,
      ":\n",
      paste(readLines(err, warn = FALSE), collapse = "\n"),
      call. = FALSE
    )
  }
  out
}

git_commit_of <- function(repo, rev) {
  rev <- paste0(rev, "^{commit}")
  out <- git(repo, c("rev-parse", "--verify", "--quiet", rev), error = FALSE)
  if (length(out) == 0) NA_character_ else out[1]
}

git_blob_id <- function(repo, rev, path) {
  rev <- paste0(rev, ":", path)
  out <- git(repo, c("rev-parse", "--verify", "--quiet", rev), error = FALSE)
  if (length(out) == 0) NA_character_ else out[1]
}

# The bytes of a file at a commit, as stored, into `dest`. TRUE on success.
git_blob_to_file <- function(repo, rev, path, dest) {
  status <- suppressWarnings(system2(
    "git",
    shQuote(c("-C", repo, "cat-file", "blob", paste0(rev, ":", path))),
    stdout = dest,
    stderr = FALSE
  ))
  identical(as.integer(status), 0L)
}

git_files_at <- function(repo, rev, dir = "R/") {
  git(repo, c("ls-tree", "--name-only", rev, dir))
}

upstream_version <- function(repo, rev) {
  tmp <- tempfile("DESCRIPTION-")
  on.exit(unlink(tmp))
  if (!git_blob_to_file(repo, rev, "DESCRIPTION", tmp)) {
    return(NA_character_)
  }
  unname(read.dcf(tmp, fields = "Version")[1, 1])
}

# The commit that a tag of a remote names: the peeled entry of an annotated
# tag, else the plain one. NA if the remote does not have the tag.
remote_tag_commit <- function(repo, url, tag) {
  out <- git(repo, c("ls-remote", "--tags", url))
  ref <- paste0("refs/tags/", tag)
  sha <- sub("\t.*$", "", out)
  refs <- sub("^[^\t]*\t", "", out)
  peeled <- sha[refs == paste0(ref, "^{}")]
  plain <- sha[refs == ref]
  if (length(peeled) > 0) {
    peeled[1]
  } else if (length(plain) > 0) {
    plain[1]
  } else {
    NA_character_
  }
}

## ----- Check 1b: lock = upstream ---------------------------------------------

# Fails when Release does not resolve to Commit in the clone, when Version is
# not the Version of DESCRIPTION at Commit, or when a listed MD5 differs from
# the file at Commit.
check_engine_upstream <- function(lock, repo) {
  h <- lock$header
  problems <- character()
  tag_commit <- git_commit_of(repo, paste0("refs/tags/", h[["Release"]]))
  if (is.na(tag_commit)) {
    problems <- sprintf("the tag %s is not in %s", h[["Release"]], repo)
  } else if (tag_commit != h[["Commit"]]) {
    problems <- sprintf(
      "%s resolves to %s, but the lock says %s",
      h[["Release"]],
      tag_commit,
      h[["Commit"]]
    )
  }
  if (is.na(git_commit_of(repo, h[["Commit"]]))) {
    return(c(
      problems,
      sprintf("the commit %s is not in %s", h[["Commit"]], repo)
    ))
  }
  version <- upstream_version(repo, h[["Commit"]])
  if (!identical(version, h[["Version"]])) {
    problems <- c(
      problems,
      sprintf(
        "Version is %s, but DESCRIPTION at %s says %s",
        h[["Version"]],
        short_sha(h[["Commit"]]),
        version
      )
    )
  }
  for (i in seq_len(nrow(lock$files))) {
    file <- lock$files$file[i]
    tmp <- tempfile("blob-")
    if (!git_blob_to_file(repo, h[["Commit"]], file, tmp)) {
      problems <- c(
        problems,
        sprintf("%s is not in the clone at %s", file, short_sha(h[["Commit"]]))
      )
    } else {
      md5 <- md5_lf(tmp)
      if (md5 != lock$files$md5[i]) {
        problems <- c(
          problems,
          sprintf(
            "%s has MD5 %s at %s, but the lock says %s",
            file,
            md5,
            short_sha(h[["Commit"]]),
            lock$files$md5[i]
          )
        )
      }
    }
    unlink(tmp)
  }
  problems
}

## ----- Check 2: not stale ----------------------------------------------------

# Release tags of the clone with a version above `release`, oldest first. A
# release tag is a version, with or without a leading v (v26.11.23,
# v26.11.23-1).
newer_release_tags <- function(repo, release) {
  if (!grepl(engine_tag_pattern, release)) {
    stop("The release ", release, " is not a version tag", call. = FALSE)
  }
  tags <- git(repo, c("tag", "--list"))
  tags <- tags[grepl(engine_tag_pattern, tags)]
  if (length(tags) == 0) {
    return(character())
  }
  versions <- numeric_version(sub("^v", "", tags))
  newer <- versions > numeric_version(sub("^v", "", release))
  tags <- tags[newer]
  tags[order(versions[newer])]
}

# The lock is stale when a newer release tag changes a listed file. Newer
# commits on the branch after the newest release are listed, not judged.
check_engine_stale <- function(lock, repo, branch = "main") {
  commit <- lock$header[["Commit"]]
  files <- lock$files$file
  tags <- newer_release_tags(repo, lock$header[["Release"]])
  locked_blobs <- vapply(
    files,
    function(f) git_blob_id(repo, commit, f),
    character(1)
  )
  changed <- lapply(tags, function(tag) {
    blobs <- vapply(files, function(f) git_blob_id(repo, tag, f), character(1))
    same <- vapply(
      seq_along(files),
      function(i) identical(blobs[[i]], locked_blobs[[i]]),
      logical(1)
    )
    files[!same]
  })
  names(changed) <- tags
  newest <- if (length(tags) > 0) {
    tags[length(tags)]
  } else {
    lock$header[["Release"]]
  }
  newest_commit <- git_commit_of(repo, paste0("refs/tags/", newest))
  if (is.na(newest_commit)) {
    newest_commit <- commit
  }
  at_newest <- git_files_at(repo, newest_commit)
  not_copied <- setdiff(at_newest[grepl(engine_file_pattern, at_newest)], files)
  refs <- paste0(c("refs/remotes/origin/", "refs/heads/"), branch)
  ref_commits <- vapply(refs, function(r) git_commit_of(repo, r), character(1))
  ref <- refs[!is.na(ref_commits)][1]
  unreleased <- NULL
  if (!is.na(ref)) {
    range <- paste0(newest_commit, "..", ref)
    unreleased <- git(repo, c("log", "--oneline", "--no-decorate", range))
  }
  list(
    tags = tags,
    changed = changed,
    stale = tags[lengths(changed) > 0],
    newest = newest,
    not_copied = not_copied,
    branch = branch,
    branch_found = !is.na(ref),
    unreleased = unreleased
  )
}

## ----- Release mode ----------------------------------------------------------

# "At release" means a Version in DESCRIPTION without a development component:
# 0.4.0 or 0.4.0.1 is a release, 0.3.99.9000 is not (the usethis convention).
is_release_version <- function(version) {
  parts <- unclass(numeric_version(version))[[1]]
  !(length(parts) >= 4 && parts[4] >= 9000)
}

# TRUE when the first section of NEWS.md (from its first level-one heading to
# the next) contains `text`
news_names <- function(pkg, text) {
  path <- file.path(pkg, "NEWS.md")
  if (!file.exists(path)) {
    return(FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  heads <- grep("^# ", lines)
  if (length(heads) > 0) {
    end <- if (length(heads) > 1) heads[2] - 1 else length(lines)
    lines <- lines[heads[1]:end]
  }
  any(grepl(text, lines, fixed = TRUE))
}

## ----- Reports ---------------------------------------------------------------

report_line <- function(status, label, details = character()) {
  cat(sprintf("%-5s %s\n", status, label))
  if (length(details) > 0) {
    cat(paste0("      ", details, "\n"), sep = "")
  }
}

# Prints check 2 and returns TRUE when it fails: at release, a stale lock
# fails unless the first section of NEWS.md names the newest stale tag.
report_stale <- function(stale, pkg, release, max_commits = 20) {
  details <- character()
  for (tag in stale$tags) {
    files <- stale$changed[[tag]]
    details <- c(
      details,
      if (length(files) > 0) {
        sprintf("%s changes %s", tag, paste(files, collapse = ", "))
      } else {
        sprintf("%s changes no copied file", tag)
      }
    )
  }
  if (length(stale$not_copied) > 0) {
    details <- c(
      details,
      sprintf(
        "%s has %d engine file(s) that are not copied: %s",
        stale$newest,
        length(stale$not_copied),
        paste(stale$not_copied, collapse = ", ")
      )
    )
  }
  if (!stale$branch_found) {
    details <- c(
      details,
      sprintf("there is no branch %s in the clone", stale$branch)
    )
  } else {
    n <- length(stale$unreleased)
    details <- c(
      details,
      sprintf(
        "%d commit(s) on %s after %s",
        n,
        stale$branch,
        stale$newest
      ),
      if (n > 0) paste0("  ", stale$unreleased[seq_len(min(n, max_commits))]),
      if (n > max_commits) sprintf("  ... and %d more", n - max_commits)
    )
  }
  if (length(stale$stale) == 0) {
    report_line(
      "ok",
      "2   not stale: no newer release changes a copied file",
      details
    )
    return(FALSE)
  }
  tag <- stale$stale[length(stale$stale)]
  label <- sprintf(
    "2   stale: %s changes a copied file (sync: engine-sync.R %s)",
    tag,
    tag
  )
  if (!release) {
    report_line("note", label, details)
    return(FALSE)
  }
  if (news_names(pkg, tag)) {
    report_line("note", label, c(details, sprintf("NEWS.md names %s", tag)))
    return(FALSE)
  }
  report_line(
    "FAIL",
    label,
    c(
      details,
      sprintf("At release, sync, or name %s in the first section", tag),
      "of NEWS.md and say why the sync waits."
    )
  )
  TRUE
}

## ----- Engine check ----------------------------------------------------------

# Runs the checks and prints a report. Returns TRUE when none fails. Checks 1b
# and 2 need a clone of inlavanilla: without one they are skipped, or fail
# with require_repo = TRUE. `release` NA reads release mode from DESCRIPTION.
run_engine_check <- function(
  pkg = find_package_root(),
  repo = Sys.getenv(engine_repo_env),
  checks = c("copy", "names", "upstream", "stale"),
  branch = "main",
  release = NA,
  require_repo = FALSE
) {
  checks <- match.arg(checks, several.ok = TRUE)
  pkg <- normalizePath(pkg, mustWork = TRUE)
  desc <- package_desc(pkg)
  lock <- read_engine_lock(file.path(pkg, engine_lock_file))
  if (is.na(release)) {
    release <- is_release_version(desc[["Version"]])
  }
  upstream <- upstream_name(lock)
  cat(sprintf(
    "Engine check of %s %s (%s): %s %s at %s, %d file(s) in the lock\n",
    desc[["Package"]],
    desc[["Version"]],
    if (release) "release" else "development",
    upstream,
    lock$header[["Release"]],
    short_sha(lock$header[["Commit"]]),
    nrow(lock$files)
  ))
  failed <- FALSE
  if ("copy" %in% checks) {
    problems <- check_engine_copy(pkg, lock)
    if (length(problems) > 0) {
      report_line(
        "FAIL",
        "1a  copies = lock",
        c(
          problems,
          "Fix an engine file upstream and sync it: never edit a copy."
        )
      )
      failed <- TRUE
    } else {
      report_line(
        "ok",
        sprintf("1a  copies = lock: %d file(s) match", nrow(lock$files))
      )
    }
  }
  if ("names" %in% checks) {
    problems <- check_engine_names(pkg)
    if (length(problems) > 0) {
      report_line(
        "FAIL",
        "names: an object of an engine file is defined twice in R/",
        problems
      )
      failed <- TRUE
    } else {
      report_line("ok", "names: no object of an engine file is defined twice")
    }
  }
  if (any(c("upstream", "stale") %in% checks)) {
    if (!nzchar(repo)) {
      report_line(
        if (require_repo) "FAIL" else "skip",
        sprintf(
          "1b and 2 need a clone of %s: set %s or pass --repo",
          upstream,
          engine_repo_env
        )
      )
      failed <- failed || require_repo
    } else {
      repo <- normalizePath(repo, mustWork = TRUE)
      if ("upstream" %in% checks) {
        problems <- check_engine_upstream(lock, repo)
        if (length(problems) > 0) {
          report_line("FAIL", "1b  lock = upstream", problems)
          failed <- TRUE
        } else {
          report_line(
            "ok",
            sprintf(
              "1b  lock = upstream: %s is %s, %d hash(es) match",
              lock$header[["Release"]],
              short_sha(lock$header[["Commit"]]),
              nrow(lock$files)
            )
          )
        }
      }
      if ("stale" %in% checks) {
        stale <- check_engine_stale(lock, repo, branch)
        failed <- report_stale(stale, pkg, release) || failed
      }
    }
  }
  invisible(!failed)
}

## ----- Engine sync -----------------------------------------------------------

# Copies engine files from a release tag of a clone of inlavanilla into R/,
# byte for byte, and writes the lock. `files` adds to the files already
# locked, and NULL takes every R/vanilla-*.R file at the tag. All locked files
# are copied from the one tag, so the lock names one commit. To drop a file,
# delete the copy and its record from the lock by hand.
run_engine_sync <- function(
  tag,
  files = NULL,
  pkg = find_package_root(),
  repo = Sys.getenv(engine_repo_env),
  upstream = NULL,
  date = Sys.Date()
) {
  if (!grepl(engine_tag_pattern, tag)) {
    stop(
      tag,
      " is not a release tag: a version such as v26.11.23",
      call. = FALSE
    )
  }
  if (!nzchar(repo)) {
    stop(
      "Set ",
      engine_repo_env,
      " or pass --repo: the path of a clone of inlavanilla",
      call. = FALSE
    )
  }
  repo <- normalizePath(repo, mustWork = TRUE)
  pkg <- normalizePath(pkg, mustWork = TRUE)
  desc <- package_desc(pkg)
  lock_path <- file.path(pkg, engine_lock_file)
  old <- if (file.exists(lock_path)) read_engine_lock(lock_path) else NULL
  if (is.null(upstream)) {
    if (is.null(old)) {
      stop("There is no lock yet: pass --upstream=<URL>", call. = FALSE)
    }
    upstream <- old$header[["Upstream"]]
  }

  # A clean tree, so that the sync is one commit of its own
  if (
    is.null(git(pkg, c("rev-parse", "--is-inside-work-tree"), error = FALSE))
  ) {
    stop(pkg, " is not in a git working tree", call. = FALSE)
  }
  dirty <- git(pkg, c("status", "--porcelain"))
  if (length(dirty) > 0) {
    stop(
      "The working tree of ",
      pkg,
      " is not clean. Commit or stash first:\n",
      paste(dirty, collapse = "\n"),
      call. = FALSE
    )
  }

  # A tag that the upstream remote has, at the same commit
  commit <- git_commit_of(repo, paste0("refs/tags/", tag))
  if (is.na(commit)) {
    stop(
      "There is no tag ",
      tag,
      " in ",
      repo,
      ": fetch it first",
      call. = FALSE
    )
  }
  remote_commit <- remote_tag_commit(repo, upstream, tag)
  if (is.na(remote_commit)) {
    stop(
      "The tag ",
      tag,
      " is not on ",
      upstream,
      ": push it first",
      call. = FALSE
    )
  }
  if (remote_commit != commit) {
    stop(
      "The tag ",
      tag,
      " is ",
      short_sha(commit),
      " in the clone but ",
      short_sha(remote_commit),
      " on ",
      upstream,
      call. = FALSE
    )
  }

  # The files: those asked for (or all engine files at the tag) and those
  # already locked
  at_tag <- git_files_at(repo, commit)
  engine_at_tag <- at_tag[grepl(engine_file_pattern, at_tag)]
  if (length(files) == 0) {
    wanted <- engine_at_tag
    if (length(wanted) == 0) {
      stop("There is no R/vanilla-*.R file at ", tag, call. = FALSE)
    }
  } else {
    wanted <- ifelse(grepl("^R/", files), files, file.path("R", files))
    unknown <- setdiff(wanted, engine_at_tag)
    if (length(unknown) > 0) {
      stop(
        "Not an engine file R/vanilla-*.R at ",
        tag,
        ": ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
  }
  locked <- if (is.null(old)) character() else old$files$file
  gone <- setdiff(locked, engine_at_tag)
  if (length(gone) > 0) {
    stop(
      "The lock lists file(s) that are not at ",
      tag,
      ": ",
      paste(gone, collapse = ", "),
      ". Delete each copy and its record from the lock first.",
      call. = FALSE
    )
  }
  wanted <- sort(unique(c(wanted, locked)), method = "radix")

  # Stage the files, and refuse any that would change NAMESPACE
  stage <- tempfile("engine-sync-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  staged <- file.path(stage, basename(wanted))
  for (i in seq_along(wanted)) {
    if (!git_blob_to_file(repo, commit, wanted[i], staged[i])) {
      stop("Could not read ", wanted[i], " at ", tag, call. = FALSE)
    }
    lines <- readLines(staged[i], warn = FALSE, encoding = "UTF-8")
    ns <- grep(engine_roxygen_ns, lines, value = TRUE)
    if (length(ns) > 0) {
      stop(
        wanted[i],
        " at ",
        tag,
        " has roxygen tags that would change NAMESPACE or the collation:\n",
        paste(ns, collapse = "\n"),
        call. = FALSE
      )
    }
  }

  # Copy and write the lock
  dest <- file.path(pkg, wanted)
  dir.create(file.path(pkg, "R"), showWarnings = FALSE)
  if (!all(file.copy(staged, dest, overwrite = TRUE))) {
    stop("Could not copy the files into ", file.path(pkg, "R"), call. = FALSE)
  }
  new_files <- data.frame(file = wanted, md5 = md5_lf(dest))
  header <- c(
    Upstream = upstream,
    Release = tag,
    Commit = commit,
    Version = upstream_version(repo, commit),
    Synced = format(as.Date(date), "%Y-%m-%d"),
    Hash = engine_hash
  )
  write_engine_lock(lock_path, header, new_files)
  lock <- read_engine_lock(lock_path)

  # Report the files and the upstream log since the last sync
  old_md5 <- if (is.null(old)) {
    character()
  } else {
    stats::setNames(old$files$md5, old$files$file)
  }
  state <- ifelse(
    !wanted %in% names(old_md5),
    "new",
    ifelse(old_md5[wanted] == new_files$md5, "unchanged", "changed")
  )
  cat(sprintf(
    "Synced %d file(s) of %s %s (%s) into %s:\n",
    length(wanted),
    upstream_name(lock),
    tag,
    short_sha(commit),
    desc[["Package"]]
  ))
  cat(sprintf("  %-9s %s\n", state, wanted), sep = "")
  if (!is.null(old)) {
    old_commit <- old$header[["Commit"]]
    if (identical(old_commit, commit)) {
      cat("The lock was at ", tag, " already.\n", sep = "")
    } else if (is.na(git_commit_of(repo, old_commit))) {
      cat(
        "The last synced commit ",
        old_commit,
        " is not in the clone.\n",
        sep = ""
      )
    } else {
      args <- c("merge-base", "--is-ancestor", old_commit, commit)
      if (is.null(git(repo, args, error = FALSE))) {
        cat(tag, " does not descend from the last synced commit.\n", sep = "")
      }
      range <- paste0(old_commit, "..", commit)
      log <- git(repo, c("log", "--oneline", "--no-decorate", range))
      cat(sprintf(
        "Upstream log since %s (%s), %d commit(s):\n",
        old$header[["Release"]],
        short_sha(old_commit),
        length(log)
      ))
      cat(paste0("  ", log, "\n"), sep = "")
      stat <- git(repo, c("diff", "--stat", old_commit, commit, "--", wanted))
      if (length(stat) > 0) {
        cat("Changes to the copied files:\n")
        cat(paste0(stat, "\n"), sep = "")
      }
    }
  }

  # The checks after a sync: 1a must pass. A duplicate name means the package
  # still defines an object that the engine now defines.
  copy_problems <- check_engine_copy(pkg, lock)
  if (length(copy_problems) > 0) {
    report_line("FAIL", "1a  copies = lock", copy_problems)
  } else {
    report_line(
      "ok",
      sprintf("1a  copies = lock: %d file(s) match", nrow(lock$files))
    )
  }
  name_problems <- check_engine_names(pkg)
  if (length(name_problems) > 0) {
    report_line(
      "FAIL",
      "names: delete the package's own definitions of these objects",
      name_problems
    )
  } else {
    report_line("ok", "names: no object of an engine file is defined twice")
  }
  report_stale(check_engine_stale(lock, repo), pkg, release = FALSE)
  cat(
    "Next: run devtools::document() (NAMESPACE must not change), the tests\n",
    "and `air format . --check`, add a NEWS entry, and make it all one\n",
    "commit, \"Sync engine to ",
    upstream_name(lock),
    " ",
    tag,
    "\".\n",
    sep = ""
  )
  invisible(list(
    lock = lock,
    copy = copy_problems,
    names = name_problems
  ))
}

## ----- Command line ----------------------------------------------------------

# --key=value and --flag options, and positional arguments
parse_cli_args <- function(args, known) {
  opts <- list()
  pos <- character()
  for (a in args) {
    if (grepl("^--[^=]+=", a)) {
      opts[[sub("^--([^=]+)=.*$", "\\1", a)]] <- sub("^--[^=]+=", "", a)
    } else if (grepl("^--", a)) {
      opts[[sub("^--", "", a)]] <- TRUE
    } else {
      pos <- c(pos, a)
    }
  }
  unknown <- setdiff(names(opts), known)
  if (length(unknown) > 0) {
    stop(
      "Unknown option(s): --",
      paste(unknown, collapse = ", --"),
      call. = FALSE
    )
  }
  list(opts = opts, pos = pos)
}
