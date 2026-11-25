# The deprecation helper (R/deprecate.R): the help badge and the warning once
# per session. Each test uses ids of its own and removes them at the end, so
# that the record of this session does not leak into other tests.

forget_deprecations <- function(ids) {
  rm(list = intersect(ids, ls(deprecation_seen)), envir = deprecation_seen)
}

test_that("the badge is lifecycle's Rd markup, with text for text help", {
  badge <- badge_deprecated()
  expect_length(badge, 1L)
  expect_match(badge, "\\figure{lifecycle-deprecated.svg}", fixed = TRUE)
  expect_match(
    badge,
    "https://lifecycle.r-lib.org/articles/stages.html#deprecated",
    fixed = TRUE
  )
  expect_match(badge, "\\strong{[Deprecated]}", fixed = TRUE)
  # The markup parses as Rd
  rd <- tempfile(fileext = ".Rd")
  writeLines(
    c("\\name{x}", "\\title{x}", paste0("\\description{", badge, " Old.}")),
    rd
  )
  expect_no_error(tools::checkRd(rd))
})

test_that("the badge image is lifecycle's file, unchanged", {
  path <- test_path("..", "..", "man", "figures", "lifecycle-deprecated.svg")
  skip_if_not(file.exists(path), "needs the source tree")
  expect_identical(
    unname(tools::md5sum(path)),
    "391f696f961e28914508628a7af31b74"
  )
})

test_that("a deprecation warns once per session for each id", {
  ids <- c("test-once-a", "test-once-b")
  forget_deprecations(ids)
  w <- expect_warning(
    warn_deprecated(ids[[1]], "Old a.", class = "inlavaan_deprecated_a"),
    class = "inlavaan_deprecated_a"
  )
  expect_s3_class(w, "inlavaan_deprecated")
  expect_match(conditionMessage(w), "Old a.", fixed = TRUE)
  expect_match(conditionMessage(w), "once per session", fixed = TRUE)
  expect_no_warning(shown <- warn_deprecated(ids[[1]], "Old a."))
  expect_false(shown)
  # Another id warns on its own
  expect_warning(
    shown <- warn_deprecated(ids[[2]], "Old b."),
    class = "inlavaan_deprecated"
  )
  expect_true(shown)
  forget_deprecations(ids)
})

test_that("the option inlavaan.deprecation = \"error\" makes every call an error", {
  id <- "test-error"
  forget_deprecations(id)
  old_opt <- options(inlavaan.deprecation = "error")
  for (i in 1:2) {
    err <- expect_error(
      warn_deprecated(id, "Old c.", class = "inlavaan_deprecated_c"),
      class = "inlavaan_deprecated_c"
    )
    expect_s3_class(err, "inlavaan_deprecated")
    expect_null(conditionCall(err))
    expect_no_match(conditionMessage(err), "once per session", fixed = TRUE)
  }
  options(old_opt)
  # The errors did not mark the id as seen
  expect_warning(warn_deprecated(id, "Old c."), class = "inlavaan_deprecated")
  forget_deprecations(id)
})

test_that("a deprecation raised under silent = TRUE warns again later", {
  id <- "test-silent"
  forget_deprecations(id)
  expect_warning(
    warn_deprecated(id, "Old d.", silent = TRUE),
    class = "inlavaan_deprecated"
  )
  expect_warning(
    warn_deprecated(id, "Old d.", silent = TRUE),
    class = "inlavaan_deprecated"
  )
  expect_warning(warn_deprecated(id, "Old d."), class = "inlavaan_deprecated")
  expect_no_warning(warn_deprecated(id, "Old d."))
  forget_deprecations(id)
})

test_that("the message is filled in from the caller's environment", {
  id <- "test-envir"
  forget_deprecations(id)
  caller <- function() {
    old <- "n_qmc"
    warn_deprecated(id, "{.arg {old}} is deprecated.")
  }
  w <- expect_warning(caller(), class = "inlavaan_deprecated")
  expect_match(cli::ansi_strip(conditionMessage(w)), "`n_qmc` is deprecated")
  forget_deprecations(id)
})
