################################################################################
#
# Sync the engine copies from a release tag of inlavanilla, run from the
# package root:
#
#   Rscript inst/dev/engine-sync.R <tag> [files] [--repo=PATH] [--pkg=PATH]
#     [--upstream=URL]
#
# The clone of inlavanilla is --repo or the environment variable
# INLAVANILLA_REPO. The script refuses a working tree with changes, a tag that
# is not a version (v26.11.23), and a tag that is not on the upstream remote
# (Upstream in inst/inlavanilla.lock, or --upstream for a first lock) at the
# same commit. It copies the files byte for byte to R/, writes the lock, and
# prints the upstream log since the last sync, check 1a, the duplicate names
# and the staleness report.
#
# The files are named as R/vanilla-<name>.R or vanilla-<name>.R. Without them
# the sync takes every R/vanilla-*.R file at the tag. With them it adds them to
# the files already locked, and copies all from the one tag. To drop a file,
# delete the copy and its record from the lock by hand. The sync leaves
# NAMESPACE unchanged and refuses a file with roxygen tags that would change
# it. Then run devtools::document(), add the dependencies of the copied files
# to DESCRIPTION, run the tests, R CMD check and `air format . --check`, and
# commit the sync with its NEWS entry as one commit.
#
################################################################################

## ----- Configuration ---------------------------------------------------------

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) == 0) {
  stop("Run this script with Rscript", call. = FALSE)
}
# Rscript writes each space in the path of the script as ~+~
script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg[1]), fixed = TRUE)
script_dir <- dirname(normalizePath(script_path))
source(file.path(script_dir, "engine-tools.R"))

cli <- parse_cli_args(
  commandArgs(trailingOnly = TRUE),
  known = c("repo", "pkg", "upstream")
)
opts <- cli$opts
if (length(cli$pos) == 0) {
  stop("Usage: Rscript inst/dev/engine-sync.R <tag> [files]", call. = FALSE)
}

## ----- Sync ------------------------------------------------------------------

run_engine_sync(
  tag = cli$pos[1],
  files = cli$pos[-1],
  pkg = if (is.null(opts$pkg)) find_package_root() else opts$pkg,
  repo = if (is.null(opts$repo)) Sys.getenv(engine_repo_env) else opts$repo,
  upstream = opts$upstream
)
