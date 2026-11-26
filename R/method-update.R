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
#' When the parameter structure and the data are preserved (no change to
#' `model`, `add`, `model.type`, `data` or the sample statistics), the previous
#' posterior mode is passed as the optimiser's starting value (a warm start),
#' so mode-finding is typically much faster than a cold fit. This makes prior
#' sweeps particularly cheap. The warm start changes the optimiser's starting
#' point, so the mode it finds agrees with that of a fresh fit only up to the
#' optimiser's tolerance (differences of order `1e-5` on the internal scale).
#' A fit that fails the check of the one-step VB correction is the exception.
#' The warm search for the mode costs less, and so lowers the cap that
#' `vb_budget` sets on the VB solve (see [inlavaan_control()]). The solve can
#' then stop further short of the VB mean than in a fresh fit, and the VB mean
#' and the log marginal likelihood differ more. Give `start = NULL` for the
#' fit of a fresh call, or `control = list(vb_budget = Inf)` to lift the cap.
#' It is used only when the refitted model has the same free parameters, by
#' name and in the same order. Otherwise, as after `std.lv = TRUE` or
#' `meanstructure = TRUE`, the fit starts from the usual values. It is not
#' used when the recorded data are a function call, such as
#' `lavaan::simulateData(...)` or `read.csv(...)`, which can give other data
#' each time, rather than an object or a part of one (`dd`, `dd[dd$g == 1, ]`).
#' Supply `start` explicitly to override it, or `start = NULL` to start from
#' the usual values and get the fit of a fresh call.
#'
#' The refit needs the objects that the recorded call names: the data, the
#' model, and the sample statistics of a fit to summary data (`sample.cov`,
#' `sample.mean`, `sample.nobs`). When one of them is no longer found, as
#' after `rm(dd)` or in a new session, `update()` stops with an error of class
#' `inlavaan_update_data_not_found` that names it. Give it again, as in
#' `update(fit, data = dd)` or `update(fit, model = mm)`, or fit the model
#' again with [inlavaan()]. An object named after a function, such as `df` or
#' `data` in a new session, counts as not found. A data call, such as
#' `simulateData(mm, sample.nobs = 100)`, is not run before the refit: only
#' the object at its root (`mm`) is checked. An error inside the call is the
#' error of the refit.
#'
#' `update(fit, arg = NULL)` drops `arg` from the recorded call, as
#' [stats::update()] does. An argument given under another spelling that
#' lavaan reads as the same, such as `std_ov` for `std.ov`, replaces the
#' recorded one. An abbreviated argument name, such as `dat` for `data`, is
#' not completed: `update()` stops with an error of class
#' `inlavaan_update_abbreviated` that names the arguments it may mean.
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
    check_update_names(names(extras))
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

    # The refit evaluates the recorded call again, so it needs the objects
    # that the call names. `add` reads the recorded model at once.
    given <- c(names(extras), if (model_given) "model")
    not_found <- objects_not_found(cl, given, caller)
    if (length(not_found) > 0L) {
      model_lost <- add_given && "model" %in% names(not_found)
      if (evaluate || model_lost) {
        abort_not_found(not_found)
      }
    }

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

    # Warm start: the search for the mode starts at the mode of the fit. The
    # search then ends close to, not at, the mode of a fresh fit. So the warm
    # start is used only when the structure and the data stay the same. It is
    # skipped when the model or its type changes, when the data or the sample
    # statistics are given, when the data are a call such as simulateData()
    # that can give other data, and when `start` is given: `start = NULL`
    # asks for the usual starting values.
    data_keys <- arg_key(setdiff(needed_args, "model"))
    warm <- !model_given &&
      !add_given &&
      !"model.type" %in% names(extras) &&
      !any(arg_key(names(extras)) %in% data_keys) &&
      reads_object(cl$data) &&
      !"start" %in% names(extras)
    if (warm) {
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

# Stop when a name given to update() is an abbreviation: not an argument of
# inlavaan(), lavaan() or an option of lavaan, under any spelling, but the start
# of one or more of them. R would complete it to an argument of inlavaan() only
# where that is unique, and lavaan stops on a name it does not know, so the
# refit would fail with an error that does not say why. Names that start none
# of them pass, so that lavaan reports them as it does for inlavaan().
check_update_names <- function(nms) {
  nms <- nms[nzchar(nms)]
  known <- setdiff(
    unique(c(
      names(formals(inlavaan)),
      names(formals(lavaan::lavaan)),
      names(lavaan::lavOptions())
    )),
    "..."
  )
  keys <- arg_key(known)
  # The deprecated arguments are known, but never suggested
  deprecated <- c(
    names(inlavaan_moved_args),
    names(inlavaan_ended_args),
    "marginal_correction"
  )
  for (nm in nms) {
    key <- arg_key(nm)
    if (key %in% keys) {
      next
    }
    full <- unique(known[startsWith(keys, key) & !known %in% deprecated])
    full <- full[!duplicated(arg_key(full))]
    if (length(full) > 0L) {
      cli_abort(
        c(
          "{.fn update} does not complete abbreviated argument names.",
          "x" = "{.arg {nm}} is not an argument of {.fn inlavaan} or of
                 lavaan.",
          "i" = "Did you mean {.or {.arg {full}}}?"
        ),
        class = "inlavaan_update_abbreviated"
      )
    }
  }
  invisible(nms)
}

# The arguments of a recorded call that name objects which the refit needs:
# the model, the data, and the sample statistics of a fit to summary data
needed_args <- c(
  "model",
  "data",
  "sample.cov",
  "sample.mean",
  "sample.nobs",
  "sample.th",
  "NACOV",
  "WLS.V"
)

# The arguments of the call `cl` that name an object no longer found in
# `env`, as a named list: the name of the object and the expression of the
# argument. The arguments in `given`, which update() was given, are not
# checked.
objects_not_found <- function(cl, given, env) {
  args <- names(cl)[-1L]
  keys <- arg_key(args)
  args <- args[keys %in% arg_key(needed_args) & !keys %in% arg_key(given)]
  out <- list()
  for (arg in args) {
    name <- missing_object(cl[[arg]], env)
    if (!is.null(name)) {
      out[[arg]] <- list(name = name, expr = cl[[arg]])
    }
  }
  out
}

# Stop update() on the objects in `not_found`, from objects_not_found()
abort_not_found <- function(not_found) {
  args <- names(not_found)
  label <- paste("the", args)
  items <- character(length(args))
  given <- character(length(args))
  for (i in seq_along(args)) {
    name <- not_found[[i]]$name
    expr <- deparse1(not_found[[i]]$expr)
    if (is.symbol(not_found[[i]]$expr)) {
      items[i] <- paste0(label[i], " `", name, "`")
      given[i] <- paste(args[i], "=", name)
    } else {
      items[i] <- paste0("`", name, "` in ", label[i], " `", expr, "`")
      given[i] <- paste(args[i], "= ...")
    }
  }
  example <- paste0("update(fit, ", paste(given, collapse = ", "), ")")
  n <- length(items)
  cli_abort(
    c(
      "{.fn update} cannot find {items} of the fit.",
      "i" = "Give {cli::qty(n)}{?it/them} again, as in {.code {example}}, or
             fit the model again with {.fn inlavaan}."
    ),
    class = "inlavaan_update_data_not_found",
    call = NULL
  )
}

# The functions that read a part of an object, as in `dd$x` or `dd[1:10, ]`
extract_fns <- c("$", "[[", "[", "@")

# Whether the expression `expr` reads an object or a part of it, as `dd`,
# `dd[dd$g == 1, ]` and `lavaan::HolzingerSwineford1939` do. A call to another
# function, such as simulateData() or read.csv(), can give other data each
# time.
reads_object <- function(expr) {
  while (is.call(expr)) {
    fn <- expr[[1L]]
    if (
      identical(fn, as.name("::")) || identical(fn, as.name(strrep(":", 3L)))
    ) {
      return(TRUE)
    }
    if (!(is.symbol(fn) && as.character(fn) %in% extract_fns)) {
      return(FALSE)
    }
    expr <- expr[[2L]]
  }
  TRUE
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
  direct <- TRUE
  while (is.call(expr)) {
    fn <- expr[[1L]]
    if (
      length(expr) < 2L ||
        identical(fn, as.name("::")) ||
        identical(fn, as.name(strrep(":", 3L)))
    ) {
      return(NULL)
    }
    if (!(is.symbol(fn) && as.character(fn) %in% extract_fns)) {
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
