#' Tuning of an INLAvaan fit
#'
#' @description
#' Builds the `control` list of [inlavaan()], [acfa()], [asem()] and
#' [agrowth()]: the limits of the search for the posterior mode, of the VB
#' correction and of the skew-normal scans, and three settings for checks and
#' development. The defaults suit most fits, so most users never need it. A
#' plain list with some of these names works too, and is checked by the same
#' rules: `control = list(n_grid = 31)` is the same as
#' `control = inlavaan_control(n_grid = 31)`.
#'
#' @param mode_maxit,mode_maxeval The largest number of iterations, and of
#'   evaluations of the log-posterior, of the search for the posterior mode by
#'   [stats::nlminb()] (its `iter.max` and `eval.max`). When the search stops
#'   short of the mode, it starts once more from where it stopped, with each
#'   parameter scaled by its curvature and the same limits.
#' @param mode_tol The relative tolerance of the search for the posterior mode
#'   (the `rel.tol` of [stats::nlminb()]).
#' @param vb_maxit,vb_tol The largest number of iterations, and the tolerance
#'   in posterior SDs, of the fixed-point iteration of the VB correction. They
#'   act with `vb_correction = "full"`, and when the default one-step
#'   correction takes a step so large that it must iterate.
#' @param n_grid The number of points of the scan of each skew-normal
#'   marginal, over 4 posterior SDs on each side of the mode. At least 7. The
#'   same as the argument `sn_fit_ngrid` of [inlavaan()].
#' @param log_threshold Points of the scan whose log-posterior falls more than
#'   this below the maximum are left out of the skew-normal fit. The five
#'   highest points are always kept. A single negative number. The same as the
#'   argument `sn_fit_logthresh` of [inlavaan()].
#' @param nlminb A named list of other controls of [stats::nlminb()] for the
#'   search for the posterior mode, such as `trace = 1`, which prints its
#'   iterations, or `abs.tol`. Its `iter.max`, `eval.max` and `rel.tol` stand
#'   in for `mode_maxit`, `mode_maxeval` and `mode_tol`, which must then not
#'   be set too. It does not reach the optimiser that the VB correction falls
#'   back to.
#' @param hessian How the Hessian at the posterior mode is differenced from the
#'   analytic gradient. `"central"` (default) costs two gradient evaluations
#'   per parameter. `"forward"` costs about half as much but is less accurate,
#'   and suits models with many parameters and large samples. The same as the
#'   argument `hessian_method` of [inlavaan()].
#' @param debug Logical. With `TRUE`, the fit returns its internal list (see
#'   [get_inlavaan_internal()]) instead of an [INLAvaan] object, for
#'   development. Default `FALSE`. The same as the argument `debug` of
#'   [inlavaan()].
#' @param add_priors Logical. With `FALSE`, the log-posterior leaves out the
#'   priors, so that the posterior mode is the maximum likelihood estimate.
#'   Only the mode is meaningful then. Default `TRUE`. The same as the argument
#'   `add_priors` of [inlavaan()].
#'
#' @details Give each setting in one place only: an entry and the argument of
#'   [inlavaan()] that does the same (for example `n_grid` and `sn_fit_ngrid`)
#'   cannot both be set, and neither can an entry and its twin in `nlminb`.
#'
#'   Up to INLAvaan 0.3.5, `control` was a list of controls of
#'   [stats::nlminb()], such as `control = list(iter.max = 2000)`. These names
#'   still work in `control`, with a deprecation warning, and stop from
#'   INLAvaan 0.5.0: `iter.max`, `eval.max` and `rel.tol` set `mode_maxit`,
#'   `mode_maxeval` and `mode_tol`, and the others go to `nlminb`.
#'
#' @returns A list of the entries that were set, checked, for the `control`
#'   argument of [inlavaan()]. An entry left out takes its default. An
#'   `iter.max`, `eval.max` or `rel.tol` given in `nlminb` is returned as
#'   `mode_maxit`, `mode_maxeval` or `mode_tol`.
#'
#' @seealso [inlavaan()], [stats::nlminb()]
#'
#' @example inst/examples/ex-inlavaan-control.R
#'
#' @export
inlavaan_control <- function(
  mode_maxit = 5000L,
  mode_maxeval = 5000L,
  mode_tol = 1e-10,
  vb_maxit = 25L,
  vb_tol = 1e-3,
  n_grid = 21L,
  log_threshold = -6,
  nlminb = list(),
  hessian = c("central", "forward"),
  debug = FALSE,
  add_priors = TRUE
) {
  given <- names(match.call())[-1L] %||% character()
  out <- list()

  # The engine's entries. Only the entries given go to the engine's
  # vanilla_control(), which tells an entry given from one left at its
  # default. Given all, it would stop on a twin in `nlminb`. A twin it moves
  # to its named entry is added to the result, or a later check of the
  # result would set that entry back to its default.
  engine_given <- intersect(names(formals(vanilla_control)), given)
  args <- mget(engine_given, envir = environment())
  ctrl <- do.call(vanilla_control, args)
  out[engine_given] <- ctrl[engine_given]
  if ("nlminb" %in% engine_given) {
    for (entry in names(nlminb_twins)) {
      if (!is.null(args$nlminb[[nlminb_twins[[entry]]]])) {
        out[[entry]] <- ctrl[[entry]]
      }
    }
  }

  # INLAvaan's own entries
  if ("hessian" %in% given) {
    out$hessian <- check_choice(hessian, c("central", "forward"), "hessian")
  }
  if ("debug" %in% given) {
    check_flag(debug, "debug")
    out$debug <- debug
  }
  if ("add_priors" %in% given) {
    check_flag(add_priors, "add_priors")
    out$add_priors <- add_priors
  }

  if (length(out) == 0L) {
    return(list())
  }
  out[intersect(names(formals(inlavaan_control)), names(out))]
}

# The controls of stats::nlminb(), as its help lists them. Up to INLAvaan
# 0.3.5, `control` was a list of these.
nlminb_controls <- c(
  "eval.max",
  "iter.max",
  "trace",
  "abs.tol",
  "rel.tol",
  "x.tol",
  "xf.tol",
  "step.min",
  "step.max",
  "sing.tol",
  "scale.init",
  "diff.g"
)

# The `control` of inlavaan(), from inlavaan_control() or a plain list, as the
# entries set, checked by inlavaan_control(). A control of nlminb() at the top
# level, as `control` took up to 0.3.5, moves into `nlminb` with a deprecation
# warning, so that iter.max, eval.max and rel.tol set mode_maxit, mode_maxeval
# and mode_tol. `silent` is that of the fit (see warn_deprecated()).
resolve_control <- function(control, silent = FALSE) {
  valid <- names(formals(inlavaan_control))
  nms <- names(control)
  if (!is.list(control)) {
    cli_abort(
      "{.arg control} must be a list, as {.fn inlavaan_control} makes."
    )
  }
  if (length(control) > 0L && (is.null(nms) || any(is.na(nms) | nms == ""))) {
    cli_abort(c(
      "Every {.arg control} entry must be named.",
      "i" = "Valid entries are {.val {valid}}."
    ))
  }
  old_names <- intersect(nms, nlminb_controls)
  if (length(old_names) > 0L && is.null(control$nlminb)) {
    control$nlminb <- list()
  }
  # The entries that the old names of the iterations, the evaluations and the
  # tolerance set, such as mode_maxit for iter.max
  twin_of <- stats::setNames(names(nlminb_twins), nlminb_twins)
  for (old in old_names) {
    if (is.list(control$nlminb) && !is.null(control$nlminb[[old]])) {
      cli_abort(c(
        "Set {.code control${old}} or {.code control$nlminb${old}}, not
         both.",
        "i" = "They are the same control of the search for the mode."
      ))
    }
    if (old %in% names(twin_of) && twin_of[[old]] %in% nms) {
      entry <- twin_of[[old]]
      cli_abort(c(
        "Set {.code control${old}} or {.code control${entry}}, not both.",
        "i" = "They are the same control of the search for the mode."
      ))
    }
    new <- if (old %in% names(twin_of)) {
      paste0(twin_of[[old]], " = ")
    } else {
      paste0("nlminb = list(", old, " = )")
    }
    warn_deprecated(
      paste0("control$", old),
      c(
        "{.code control${old}} is deprecated, and stops from INLAvaan 0.5.0.",
        "i" = "Use {.code control = inlavaan_control({new})}, which gives the
               same fit."
      ),
      silent = silent
    )
    # A `nlminb` that is not a list stops in inlavaan_control()
    if (is.list(control$nlminb)) {
      control$nlminb[[old]] <- control[[old]]
    }
    control[[old]] <- NULL
  }
  unknown <- setdiff(names(control), valid)
  if (length(unknown) > 0L) {
    cli_abort(c(
      "Unknown {.arg control} entr{?y/ies}: {.val {unknown}}.",
      "i" = "Valid entries are {.val {valid}}. See {.fn inlavaan_control}."
    ))
  }
  # A bad value of an old name is checked under the entry that it sets, so
  # the error says which old name that was
  moved <- intersect(old_names, names(twin_of))
  if (length(moved) == 0L) {
    return(do.call(inlavaan_control, control))
  }
  tryCatch(
    do.call(inlavaan_control, control),
    inlavanilla_error = function(e) {
      msg <- conditionMessage(e)
      hit <- moved[vapply(
        twin_of[moved],
        function(entry) grepl(entry, msg, fixed = TRUE),
        logical(1)
      )]
      if (length(hit) == 0L) {
        stop(e)
      }
      old <- hit[[1L]]
      entry <- twin_of[[old]]
      cli_abort(
        c("{msg}", "i" = "{.code control${old}} sets {.code {entry}}."),
        call = NULL
      )
    }
  )
}
