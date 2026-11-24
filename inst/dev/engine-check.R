################################################################################
#
# Checks of the engine copies R/vanilla-*.R, run from the package root:
#
#   Rscript inst/dev/engine-check.R [--checks=copy,names,upstream,stale]
#     [--repo=PATH] [--branch=main] [--release] [--pkg=PATH]
#
# copy (1a): each file in inst/inlavanilla.lock has its MD5, and every
# R/vanilla-*.R file is listed. names: no object of an engine file is also
# defined in another file of R/. upstream (1b): the release tag of the lock
# resolves to its Commit, and each MD5 equals that of the file at Commit.
# stale (2): no newer release tag of inlavanilla changes a copied file. Newer
# commits on --branch are listed only.
#
# Checks 1b and 2 read a clone of inlavanilla, at --repo or in the environment
# variable INLAVANILLA_REPO. Fetch it first. Without a clone they are skipped,
# unless --checks names them.
#
# Check 2 reports during development and fails at release, unless the first
# section of NEWS.md names the newest stale tag and says why the sync waits.
# A release is a Version in DESCRIPTION without a development component (0.4.0,
# not 0.3.99.9000), or --release. The exit status is 1 when a check fails.
#
################################################################################

## ----- Configuration ---------------------------------------------------------

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) == 0) {
  stop("Run this script with Rscript", call. = FALSE)
}
script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
source(file.path(script_dir, "engine-tools.R"))

cli <- parse_cli_args(
  commandArgs(trailingOnly = TRUE),
  known = c("checks", "repo", "branch", "release", "pkg")
)
opts <- cli$opts

## ----- Run the checks --------------------------------------------------------

checks <- c("copy", "names", "upstream", "stale")
if (!is.null(opts$checks)) {
  checks <- strsplit(opts$checks, ",", fixed = TRUE)[[1]]
}
passed <- run_engine_check(
  pkg = if (is.null(opts$pkg)) find_package_root() else opts$pkg,
  repo = if (is.null(opts$repo)) Sys.getenv(engine_repo_env) else opts$repo,
  checks = checks,
  branch = if (is.null(opts$branch)) "main" else opts$branch,
  release = if (isTRUE(opts$release)) TRUE else NA,
  require_repo = !is.null(opts$checks)
)
quit(save = "no", status = if (passed) 0 else 1)
