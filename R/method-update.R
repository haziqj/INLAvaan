#' Update and re-fit an INLAvaan model
#'
#' Re-fit an [INLAvaan] model with modified arguments, in the style of
#' [stats::update()]. This is convenient for prior-sensitivity analyses
#' (vary `dp` or the `prior()` modifiers), iterative respecification (swap the
#' `model` or `add` syntax), and swapping approximation settings (e.g.
#' `marginal_method`, `nsamp`) without retyping the whole call.
#'
#' @details
#' The method edits the recorded [inlavaan()] call, replacing `model` (or
#' extending it via `add`) and overriding any argument supplied through `...`,
#' then re-evaluates it. Because [INLAvaan] extends the \pkg{lavaan} class, a
#' dedicated method is required: without it, `update()` would dispatch to
#' \pkg{lavaan}'s method and silently return a frequentist \pkg{lavaan} fit
#' instead of a Bayesian one.
#'
#' When the parameter structure is preserved (no change to `model`, `add`, or
#' `model.type`), the previous posterior mode is passed as the optimiser's
#' starting value (a warm start), so mode-finding is typically much faster than
#' a cold fit. This makes prior sweeps particularly cheap. The warm start only
#' changes the optimiser's starting point, not the fitted result. It is used
#' only when the refitted model has the same free parameters, by name and in
#' the same order. Otherwise, as after `std.lv = TRUE` or
#' `meanstructure = TRUE`, the fit starts from the usual values. Supply
#' `start` explicitly to override it.
#'
#' When the data object of the original call is no longer found, as after
#' `rm(dd)` or in a new session, `update()` stops with an error of class
#' `inlavaan_update_data_not_found`. Give the data again with `data`, as in
#' `update(fit, data = dd)`, or fit the model again with [inlavaan()]. Data
#' named after a function, such as `df` or `data` in a new session, count as
#' not found.
#'
#' `update(fit, arg = NULL)` drops `arg` from the recorded call, as
#' [stats::update()] does. An argument given under another spelling that
#' lavaan reads as the same, such as `std_ov` for `std.ov`, replaces the
#' recorded one.
#'
#' `verbose = TRUE` and `silent = TRUE` cannot go together. So
#' `update(fit, silent = TRUE)` drops `verbose` from the recorded call, and
#' `update(fit, verbose = TRUE)` drops `silent`.
#'
#' Fits produced by INLAvaan versions that did not record their call cannot be
#' updated; re-fit once with [inlavaan()] (or [acfa()], [asem()], [agrowth()])
#' and `update()` will work on the new object.
#'
#' @param object An object of class [INLAvaan].
#' @param model Optional replacement model, in the same form accepted by
#'   [inlavaan()] (lavaan model syntax or a parameter table).
#' @param add Optional lavaan syntax appended to the original model. Requires
#'   the original model to have been specified as a syntax string, and is
#'   ignored when `model` is supplied.
#' @param ... Further arguments passed to [inlavaan()], overriding the values
#'   in the original call (e.g. `dp`, `marginal_method`, `nsamp`, `data`).
#' @param evaluate Logical. If `TRUE` (default) the updated call is evaluated
#'   and the re-fitted [INLAvaan] object is returned; if `FALSE` the updated,
#'   unevaluated call is returned for inspection.
#'
#' @returns A re-fitted [INLAvaan] object, or the updated call when
#'   `evaluate = FALSE`.
#'
#' @seealso [inlavaan()], [compare()]
#'
#' @importFrom stats update
#' @name update
#' @aliases update,INLAvaan-method
#' @export
#'
#' @examples
#' \dontrun{
#' model <- "visual =~ x1 + x2 + x3"
#' fit <- acfa(model, data = lavaan::HolzingerSwineford1939)
#'
#' # Prior sensitivity: refit under a tighter loading prior (warm-started)
#' fit2 <- update(fit, dp = priors_for(lambda = "normal(0,1)"))
#'
#' # Respecify: add a residual covariance
#' fit3 <- update(fit, add = "x1 ~~ x2")
#' compare(fit, fit3)
#'
#' # Swap approximation settings only
#' fit4 <- update(fit, marginal_method = "gaussian", nsamp = 4000)
#' }
setMethod(
  "update",
  "INLAvaan",
  function(object, model, add, ..., evaluate = TRUE) {
    internal <- get_inlavaan_internal(object)
    cl <- internal$call
    if (is.null(cl)) {
      cli_abort(c(
        "This fit was produced by an INLAvaan version that did not record its
         call, so it cannot be updated.",
        "i" = "Re-fit once with {.fn inlavaan} (or {.fn acfa}, {.fn asem},
               {.fn agrowth}) and {.fn update} will work on the new object."
      ))
    }

    # Resolve the caller's environment once: symbols in the recorded call
    # (`model`, `data`) and the re-fit must evaluate there, not in the
    # auto-generated `.local` wrapper frame that `setMethod()` inserts.
    caller <- parent.frame(2L)

    # Override any inlavaan() argument passed through ..., preserving the
    # supplied expressions. Inside an S4 method `match.call()` sees the
    # `.local` wrapper, so match against this method's formals and the
    # original generic call one frame up.
    outer_call <- match.call(
      definition = sys.function(),
      call = sys.call(-1L),
      expand.dots = FALSE
    )
    extras <- outer_call[["..."]]
    for (nm in names(extras)) {
      # lavaan reads `std_ov` as `std.ov`. So the argument under another
      # spelling goes, or lavaan would stop on a name given twice.
      cl <- drop_other_spellings(cl, nm)
      if (is.null(extras[[nm]]) && !nm %in% names(cl)) {
        # As in stats::update(), `arg = NULL` drops an argument of the call,
        # or gives it as NULL when the call does not have it.
        cl[nm] <- list(NULL)
      } else {
        cl[[nm]] <- extras[[nm]]
      }
    }
    # verbose = TRUE and silent = TRUE cannot go together, so asking for one
    # drops the other from the recorded call, unless it is asked for too:
    # update(fit, silent = TRUE) then works on a fit made with verbose = TRUE.
    asked <- names(extras)
    for (pair in list(c("silent", "verbose"), c("verbose", "silent"))) {
      one <- pair[[1L]]
      other <- pair[[2L]]
      drop_other <- is.element(one, asked) &&
        !is.element(other, asked) &&
        is.element(other, names(cl)) &&
        isTRUE(eval(extras[[one]], caller))
      if (drop_other) {
        cl[[other]] <- NULL
      }
    }

    model_given <- !missing(model)
    add_given <- !model_given && !missing(add)

    # Replace or extend the model
    if (model_given) {
      cl$model <- model
    } else if (add_given) {
      orig <- cl$model
      if (is.symbol(orig) || is.call(orig)) {
        orig <- eval(orig, caller)
      }
      if (!is.character(orig)) {
        cli_abort(c(
          "{.arg add} requires the original model to be lavaan syntax.",
          "i" = "Supply a full replacement via {.arg model} instead."
        ))
      }
      cl$model <- paste(paste(orig, collapse = "\n"), add, sep = "\n")
    }

    if (!evaluate) {
      return(cl)
    }

    # The refit evaluates the recorded call again, so it needs the data that
    # the call names.
    data_missing <- !"data" %in% names(extras) &&
      !is.null(missing_object(cl$data, caller))
    if (data_missing) {
      cli_abort(
        c(
          "{.fn update} cannot find the data {.code {deparse1(cl$data)}} of
           the fit.",
          "i" = "Give the data with {.arg data}, as in
                 {.code update(fit, data = dd)}, or fit the model again with
                 {.fn inlavaan}."
        ),
        class = "inlavaan_update_data_not_found",
        call = NULL
      )
    }

    # Warm start: the search for the mode starts at the mode of the fit. This
    # changes where the search starts, not the fit. It is skipped when the
    # model or its type changes, or when `start` is given.
    structure_changed <- model_given ||
      add_given ||
      "model.type" %in% names(extras)
    if (!structure_changed && is.null(cl$start)) {
      # inlavaan() checks the names before it uses the mode as the start
      cl$start <- structure(
        internal$theta_star_novbc,
        names = names(internal$coefficients),
        class = "inlavaan_warm_start"
      )
    }

    eval(cl, caller)
  }
)

# Argument names as lavaan reads them: dot or snake case, upper or lower case
arg_key <- function(x) {
  tolower(gsub("_", ".", x, fixed = TRUE))
}

# Drop the arguments of the call `cl` that lavaan reads as the argument `nm`
# under another spelling, such as `std.ov` for `std_ov`. R matches the
# arguments of inlavaan() itself by their exact names, so these are kept.
drop_other_spellings <- function(cl, nm) {
  own <- names(formals(inlavaan))
  args <- names(cl)
  other <- which(
    arg_key(args) == arg_key(nm) & args != nm & !args %in% own
  )
  if (nm %in% own || length(other) == 0L) {
    return(cl)
  }
  cl[-other]
}

# The name of the object that the expression `expr` of a recorded call reads
# and that is no longer found in `env`, or NULL. The expression is not
# evaluated: a call such as read.csv() or lavaan::simulateData() would run
# twice, and an error inside it would read as "not found". So only the object
# at the root is checked: `dd` in `dd`, `dd$x` or `dd[dd$g == 1, ]`, and `m`
# in `simulateData(m, sample.nobs = n)`. Other symbols of a call are left to
# the refit, since a call such as subset(dd, g == 1) can use symbols that are
# not objects. For an object read directly or by `$`, `[[`, `[` or `@`, a
# function counts as not found: in a new session, data named `df` or `data`
# find the functions stats::df() and utils::data(), which lavaan cannot read.
missing_object <- function(expr, env) {
  extract <- c("$", "[[", "[", "@")
  direct <- TRUE
  while (is.call(expr)) {
    fn <- expr[[1L]]
    if (
      length(expr) < 2L ||
        identical(fn, as.name("::")) ||
        identical(fn, as.name(":::"))
    ) {
      return(NULL)
    }
    if (!(is.symbol(fn) && as.character(fn) %in% extract)) {
      direct <- FALSE
    }
    expr <- expr[[2L]]
  }
  if (!is.symbol(expr) || identical(as.character(expr), "")) {
    return(NULL)
  }
  name <- as.character(expr)
  if (!exists(name, envir = env)) {
    return(name)
  }
  if (direct && is.function(get(name, envir = env))) {
    return(name)
  }
  NULL
}
