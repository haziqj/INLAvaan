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
#' `rm(dd)` or in a new session, `update()` uses the data stored in the fit
#' and says so. lavaan has read these data already, with the model of the fit
#' and its options on how the data are read (for example, it has dropped the
#' incomplete rows under listwise deletion, and it has put the observed
#' variables in the order of the model). So the refit then uses the model and
#' these options with the values that the fit was made with, unless the update
#' gives them. The fit records these values, and the objects that the original
#' call names for them are not looked up again. A model `m` or a value
#' `missing = mm` that has changed since the fit, or that no longer exists,
#' has no effect, and `add` extends the model of the fit. The other arguments
#' of the original call, such as `dp`, are evaluated again as usual.
#'
#' On the stored data, `update()` stops with an error of class
#' `inlavaan_update_data_not_found` when the update gives a new value to an
#' option on how the data are read, such as `group`, `cluster`, `missing`,
#' `ordered`, `fixed.x`, `sampling.weights` or `ov.order`. It also stops when
#' the new model has other observed variables, or gives them other roles
#' (other exogenous or ordinal variables, or other variables on a level), or
#' puts them in another order (unless the fit has `ov.order = "data"`, under
#' which lavaan puts them in the order of the data). To make such a change,
#' give `data` again.
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

    # A refit on the stored data (see below) records them in its call as
    # `slotData`, which lavaan reads before `data`. So data given again
    # replace them.
    data_given <- "data" %in% names(extras)
    slot_args <- names(cl)[arg_key(names(cl)) %in% c("slotdata", "slot.data")]
    if (data_given) {
      for (nm in slot_args) {
        cl[[nm]] <- NULL
      }
    }

    # When the data object of the recorded call is gone from the caller's
    # environment, the refit uses the data stored in the fit, with a message
    # (unless the refit is silent). A refit on the stored data uses them
    # again. lavaan has read the stored data already, with the model of the
    # fit and its options on how the data are read, and would ignore new
    # values of those options. So the refit keeps the values that the fit was
    # made with (see pin_stored_call()), and an update that changes them stops
    # (see check_stored_data_update()).
    model_given <- !missing(model)
    add_given <- !model_given && !missing(add)
    data_lost <- (evaluate || add_given) &&
      !data_given &&
      data_not_found(cl$data, caller)
    stored <- data_lost || (!data_given && length(slot_args) > 0L)
    read_with <- if (stored) fit_read_values(object)

    # Replace or extend the model. On the stored data, `add` extends the model
    # that the fit was made with.
    if (model_given) {
      cl$model <- model
    } else if (add_given) {
      orig <- if (stored) read_with$model else cl$model
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

    if (stored) {
      new_model <- if (model_given || add_given) cl$model
      check_stored_data_update(object, extras, new_model, env = caller)
      cl <- pin_stored_call(cl, read_with, names(extras), is.null(new_model))
    }
    if (data_lost) {
      silent <- tryCatch(
        isTRUE(eval(cl$silent, caller)),
        error = function(e) FALSE
      )
      if (!silent) {
        cli_inform(
          c(
            "i" = "The data {.code {deparse1(cl$data)}} of the fit
                   are not found, so {.fn update} uses the data stored in the
                   fit, with the model and the options on how the data are
                   read that the fit was made with.",
            " " = "Give {.arg data} to refit with other options on how the
                   data are read, such as {.arg group}, {.arg cluster},
                   {.arg missing} or {.arg ordered}."
          ),
          class = "inlavaan_update_stored_data"
        )
      }
      cl$data <- NULL
      cl$slotData <- object@Data
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

# The arguments of lavaan that set how the data are read, as lavaan's
# lav_lavdata() and lav_data_full() read them. With the stored data of a fit,
# lavaan keeps the data as they were read and ignores a new value of any of
# them. The data in another form (`sample.cov` and the like) are ignored too.
# `mimic` sets the default of `missing`. `ov.order` sets the order of the
# observed variables. `ordered`, `fixed.x` and `conditional.x` set the roles
# of the observed variables (see check_stored_data_roles()).
data_read_args <- c(
  "group",
  "group.label",
  "cluster",
  "level.label",
  "ordered",
  "missing",
  "mimic",
  "sampling.weights",
  "sampling.weights.normalization",
  "std.ov",
  "fixed.x",
  "conditional.x",
  "correlation",
  "allow.empty.cell",
  "aux",
  "ov.order",
  "sample.cov",
  "sample.mean",
  "sample.th",
  "sample.nobs",
  "nacov",
  "wls.v"
)

# The arguments of `data_read_args` that set the roles of the observed
# variables.
data_role_args <- c("ordered", "fixed.x", "conditional.x")

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

# Whether the data expression of a recorded call names an object that is no
# longer found in `env`.
data_not_found <- function(data, env) {
  if (!is.symbol(data) && !is.call(data)) {
    return(FALSE)
  }
  tryCatch(
    {
      eval(data, env)
      FALSE
    },
    error = function(e) TRUE
  )
}

# The model and the options on how the data are read, with the values that
# the fit was made with: a list with the model (`model`) and the values of
# the arguments of `data_read_args` that the call gave (`args`, named by
# arg_key()). inlavaan() records them in the field `read_with`. A fit made
# by an older version has them only in lavaan's record of its call, where the
# model may be rewritten (to a parameter table that fits the same model) and
# where `ordered` names the ordinal variables of the fit, whatever the call
# gave. So `ordered_known` is then FALSE when the call gave `ordered` and the
# fit has ordinal variables.
fit_read_values <- function(object) {
  internal <- get_inlavaan_internal(object)
  if (!is.null(internal$read_with)) {
    return(internal$read_with)
  }
  lav <- as.list(object@call)[-1L]
  keys <- arg_key(names(lav))
  keep <- keys %in% data_read_args & keys %in% arg_key(names(internal$call))
  args <- stats::setNames(lav[keep], keys[keep])
  list(
    model = lav$model,
    args = args,
    ordered_known = !"ordered" %in% keys[keep] ||
      length(lavaan::lavNames(object, "ov.ord")) == 0L
  )
}

# Pin the call of a refit on the stored data to the fit. Each argument of
# `data_read_args` in the call that the update does not give (by
# `given_names`) takes the value that the fit was made with, and so does the
# model when `keep_model` is TRUE. So an object that the recorded call names
# for these, such as the model `m` or `missing = mm`, is not looked up again.
# A change to it since the fit cannot reach lavaan, and the refit works when
# it no longer exists. The values that the update gives are checked by
# check_stored_data_update().
pin_stored_call <- function(cl, read_with, given_names, keep_model) {
  keys <- arg_key(names(cl))
  pin <- keys %in% data_read_args & !keys %in% arg_key(given_names)
  for (nm in names(cl)[pin]) {
    cl[nm] <- list(read_with$args[[arg_key(nm)]])
  }
  if (keep_model) {
    cl$model <- read_with$model
  }
  cl
}

# The roles of the observed variables that lavaan fixes when it reads the data
# for a model given as lavaan syntax or as a parameter table, as lavaan()
# finds them in the model before it reads the data (lav_step01_ovnames_*()).
# Per group: all the observed variables (`ov`), the endogenous ones (`ov_nox`)
# and the exogenous ones (`ov_x`), each in the order of the model, and the
# variables of each level (`ov_l`). Also the variables with thresholds in the
# model (`ov_thr`). NULL when the model cannot be parsed here (lavaan then
# reports the problem when it reads the model), and FALSE when the roles
# cannot be read here.
model_read_roles <- function(model, ngroups = 1L) {
  flat <- tryCatch(
    {
      if (is.character(model)) {
        lavaan::lavParseModelString(split_modifiers(model), parser = "old")
      } else {
        model
      }
    },
    error = function(e) NULL
  )
  if (!is.list(flat) || is.null(flat$lhs)) {
    return(NULL)
  }
  tryCatch(flat_read_roles(flat, ngroups), error = function(e) FALSE)
}

flat_read_roles <- function(flat, ngroups) {
  names_of <- function(pt, type, ...) {
    unique(unlist(lavaan::lavNames(pt, type, ...), use.names = FALSE))
  }
  roles_of <- function(pt, ...) {
    list(
      ov = names_of(pt, "ov", ...),
      ov_nox = names_of(pt, "ov.nox", ...),
      ov_x = names_of(pt, "ov.x", ...)
    )
  }
  # Group and level blocks in the syntax are expanded into a parameter table,
  # as lavaan does. A parameter table has them as columns.
  is_block <- flat$op == ":"
  block_values <- function(type) {
    unique(flat$rhs[is_block & tolower(flat$lhs) == type])
  }
  group_values <- block_values("group")
  level_values <- block_values("level")
  pt <- flat
  if (length(group_values) > 0L || length(level_values) > 0L) {
    attr(pt, "modifiers") <- NULL
    attr(pt, "constraints") <- NULL
    pt$lhs[is_block] <- tolower(pt$lhs[is_block])
    pt <- lavaan::lavaanify(
      pt,
      ngroups = max(length(group_values), 1L),
      warn = FALSE
    )
  } else {
    group_values <- partable_values(flat, "group")
    if (length(group_values) < 2L) {
      group_values <- NULL
    }
    level_values <- partable_values(flat, "level")
  }

  # Without groups in the model, every group of the data has the same roles
  if (length(group_values) > 0L) {
    roles <- lapply(group_values, function(g) roles_of(pt, group = g))
  } else {
    roles <- rep(list(roles_of(flat)), ngroups)
  }
  ov_l <- list()
  if (length(level_values) > 1L) {
    per_level <- function(...) {
      lapply(level_values, function(l) names_of(pt, "ov", ..., level = l))
    }
    ov_l <- if (length(group_values) > 0L) {
      lapply(group_values, function(g) per_level(group = g))
    } else {
      rep(list(per_level()), ngroups)
    }
  }
  list(
    ov = lapply(roles, `[[`, "ov"),
    ov_nox = lapply(roles, `[[`, "ov_nox"),
    ov_x = lapply(roles, `[[`, "ov_x"),
    ov_l = ov_l,
    ov_thr = names_of(flat, "ov.ord")
  )
}

# The values of a block column (group or level) of a parameter table
partable_values <- function(pt, column) {
  x <- pt[[column]][!pt$op %in% c("==", "<", ">", ":=")]
  x <- x[!is.na(x)]
  unique(if (is.numeric(x)) x[x > 0] else x[nzchar(x)])
}


# Stop when update() refits on the stored data of `object` and changes how the
# data are read: a new value of an argument of `data_read_args` in `extras`,
# or a model with other observed variables, or with the observed variables in
# other roles or in another order. The new values are evaluated in `env` and
# compared with the values that lavaan read the stored data with.
check_stored_data_update <- function(
  object,
  extras,
  new_model,
  env = parent.frame()
) {
  read_with <- fit_read_values(object)
  keys <- arg_key(names(extras))
  given <- list()
  changed <- character()
  for (i in seq_along(extras)) {
    key <- keys[[i]]
    if (!key %in% data_read_args) {
      next
    }
    value <- tryCatch(
      eval(extras[[i]], env),
      error = function(e) structure(list(), class = "inlavaan_no_value")
    )
    if (key %in% data_role_args) {
      given[key] <- list(value)
    } else if (!stored_read_same(key, value, object, read_with$args)) {
      changed <- c(changed, names(extras)[[i]])
    }
  }
  if (length(changed) > 0L) {
    stop_stored_data_args(changed)
  }
  if (!is.null(new_model) || length(given) > 0L) {
    check_stored_data_roles(object, new_model, given, read_with, names(extras))
  }
  invisible()
}

# Whether lavaan reads the stored data of `object` with `value` for the
# argument `key` (as from arg_key()) of `data_read_args`. `recorded` holds
# the values that the fit was made with (see fit_read_values()). The same
# value is the same, and a default (NULL) is the same when the fit did not
# give the argument either. A value that cannot be compared counts as a
# change.
stored_read_same <- function(key, value, object, recorded) {
  if (inherits(value, "inlavaan_no_value")) {
    return(FALSE)
  }
  old_given <- key %in% names(recorded)
  if (old_given && identical(value, recorded[[key]])) {
    return(TRUE)
  }
  if (is.null(value) && !old_given) {
    return(TRUE)
  }
  data <- object@Data
  opt <- object@Options
  is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
  flag_same <- function(x, stored) {
    x <- x %||% FALSE
    is_flag(x) && identical(x, isTRUE(stored))
  }
  word_same <- function(x, stored) {
    is.character(x) &&
      length(x) == 1L &&
      is.character(stored) &&
      length(stored) == 1L &&
      identical(tolower(x), tolower(stored))
  }

  switch(
    key,
    group = ,
    cluster = ,
    sampling.weights = identical(
      as.character(value %||% character()),
      as.character(methods::slot(data, key))
    ),
    # The labels by default come from the data
    group.label = ,
    level.label = !is.null(value) &&
      identical(as.character(value), as.character(methods::slot(data, key))),
    missing = identical(missing_as_read(value, opt), data@missing),
    mimic = word_same(value %||% "lavaan", opt$mimic),
    sampling.weights.normalization = word_same(
      value %||% lavaan::lavOptions("sampling.weights.normalization")[[1L]],
      opt$sampling.weights.normalization
    ),
    std.ov = flag_same(value, data@std.ov),
    allow.empty.cell = flag_same(value, opt$allow.empty.cell),
    # A character vector names the variables of the correlation structure
    correlation = if (is.character(value)) {
      isTRUE(opt$correlation) && setequal(value, opt$.correlation.ov)
    } else {
      flag_same(value, opt$correlation) &&
        (isFALSE(value %||% FALSE) || length(opt$.correlation.ov) == 0L)
    },
    ov.order = word_same(
      value %||% "model",
      recorded[["ov.order"]] %||% "model"
    ),
    FALSE
  )
}

# `missing` as lav_lavdata() reads it: the synonyms of "ml" and "ml.x" are
# "ml" and "ml.x", and the default is "listwise" (except under mimic =
# "Mplus", where it may be "ml", so NA).
missing_as_read <- function(value, opt) {
  value <- value %||% "default"
  if (!is.character(value) || length(value) != 1L) {
    return(NA_character_)
  }
  value <- tolower(value)
  if (value %in% c("ml", "fiml", "direct")) {
    return("ml")
  }
  if (value %in% c("ml.x", "fiml.x", "direct.x")) {
    return("ml.x")
  }
  if (value == "default") {
    return(if (identical(opt$mimic, "lavaan")) "listwise" else NA_character_)
  }
  value
}

# Stop when the model of the refit, or the values of `ordered`, `fixed.x` or
# `conditional.x`, give the observed variables other roles than those of the
# stored data: other exogenous or ordinal variables, other variables on a
# level, or the variables in another order. lavaan matches the stored data to
# the model by position, not by name, so the refit would mix up the
# variables. `given` holds the evaluated values of the role arguments of the
# update, and `read_with` the values that the fit was made with (see
# fit_read_values()). The refit uses the values of the update, or else those
# of the fit. The new roles come from `new_model`, or from the fit when the
# model stays.
check_stored_data_roles <- function(
  object,
  new_model,
  given,
  read_with,
  extra_names
) {
  data <- object@Data
  opt <- object@Options
  recorded <- read_with$args
  pt_old <- lavaan::parTable(object)
  thr_old <- unique(pt_old$lhs[pt_old$op == "|" & pt_old$user == 1L])
  roles_old <- model_read_roles(pt_old, data@ngroups)
  if (is.list(roles_old)) {
    roles_old$ov_thr <- thr_old
  }
  if (is.null(new_model)) {
    roles <- roles_old
  } else {
    roles <- model_read_roles(new_model, data@ngroups)
    if (is.null(roles)) {
      return(invisible())
    }
  }
  if (is.list(roles) && !setequal(unique(unlist(roles$ov)), data@ov$name)) {
    cli_abort(
      c(
        "The original data of the fit are not found, so {.fn update} cannot
         fit a model with other observed variables.",
        "i" = "The data stored in the fit hold the observed variables of the
               fit. Pass the data again with {.arg data}."
      ),
      class = "inlavaan_update_data_not_found"
    )
  }
  ord_old <- data@ov$name[data@ov$type == "ordered"]
  no_old_x <- length(lavaan::lavNames(object, "ov.x")) == 0L

  # fixed.x and conditional.x of the refit: the value of the update, or else
  # that of the fit. A flag counts as given. The default (NULL or "default")
  # is resolved by lavaan as for the fit, unless the fit gave a flag. Then
  # the default for the new model is unknown (NA). A model without exogenous
  # variables sets both to FALSE, so the default for a model with exogenous
  # variables is then taken as lavaan resolves it: fixed.x TRUE (as for most
  # estimators) and conditional.x TRUE with ordinal variables only.
  role_flag <- function(key, stored) {
    value <- if (key %in% names(given)) given[[key]] else recorded[[key]]
    if (is.null(value) || identical(value, "default")) {
      fit_default <- is.null(recorded[[key]]) ||
        identical(recorded[[key]], "default")
      return(if (fit_default) stored else NA)
    }
    if (is.logical(value) && length(value) == 1L) value else NA
  }
  fixed_x <- role_flag(
    "fixed.x",
    if (no_old_x) TRUE else isTRUE(opt$fixed.x)
  )
  cond_x <- role_flag(
    "conditional.x",
    if (no_old_x) length(ord_old) > 0L else isTRUE(opt$conditional.x)
  )

  same <- is.list(roles) && !is.na(fixed_x) && !is.na(cond_x)
  if (same) {
    ov_read <- if (cond_x) roles$ov_nox else roles$ov
    ov_x_read <- if (fixed_x) roles$ov_x else lapply(roles$ov_x, `[`, 0L)
    ov_l_read <- roles$ov_l
    # Under ov.order = "data", lavaan puts the observed variables in the
    # order of the data, which for the stored data is the order of their
    # names (data@ov.names). When a variable of the model is not among
    # these, lavaan warns and keeps the order of the model.
    stored_order <- unique(unlist(data@ov.names))
    by_data <- identical(
      tolower(recorded[["ov.order"]] %||% "model"),
      "data"
    ) &&
      all(unlist(roles$ov) %in% stored_order)
    if (by_data) {
      in_data_order <- function(x) x[order(match(x, stored_order))]
      ov_read <- lapply(ov_read, in_data_order)
      ov_x_read <- lapply(ov_x_read, in_data_order)
      ov_l_read <- lapply(ov_l_read, lapply, in_data_order)
    }
    same <- identical(unname(data@ov.names), unname(ov_read)) &&
      identical(unname(data@ov.names.x), unname(ov_x_read)) &&
      identical(unname(lapply(data@ov.names.l, unname)), unname(ov_l_read))
  }

  # The ordinal variables of the refit: those that `ordered` gives, those
  # with thresholds in the model, and those that are ordered factors in the
  # data. The last are not known here. But the ordinal variables of the
  # stored data that neither `ordered` nor the thresholds of the fit give
  # must be ordered factors, so they are ordinal in the refit too. `ordered`
  # is read for the new roles, as TRUE names the endogenous variables. With
  # the model and `ordered` of the fit, the ordinal variables stay.
  ordered_given <- "ordered" %in% names(given)
  if (same && (ordered_given || !is.null(new_model))) {
    known <- !isFALSE(read_with$ordered_known)
    from_factors <- character()
    if (known && is.list(roles_old)) {
      declared_old <- ordered_as_read(recorded[["ordered"]], roles_old)
      if (!anyNA(declared_old)) {
        from_factors <- setdiff(ord_old, union(declared_old, thr_old))
      }
    }
    declared <- if (ordered_given) {
      ordered_as_read(given[["ordered"]], roles)
    } else if (known) {
      ordered_as_read(recorded[["ordered"]], roles)
    } else {
      NA_character_
    }
    same <- !anyNA(declared) &&
      setequal(
        union(
          union(intersect(declared, data@ov$name), roles$ov_thr),
          from_factors
        ),
        ord_old
      )
  }
  if (same) {
    return(invisible())
  }
  if (is.null(new_model)) {
    stop_stored_data_args(
      extra_names[arg_key(extra_names) %in% names(given)]
    )
  }
  cli_abort(
    c(
      "The original data of the fit are not found, so {.fn update} cannot
       fit a model that gives the observed variables other roles.",
      "i" = "The data stored in the fit keep the exogenous and ordinal
             variables of the fit, the variables of each level, and their
             order. Pass the data again with {.arg data}."
    ),
    class = "inlavaan_update_data_not_found"
  )
}

# `ordered` as lavaan reads it (lav_step01_ovnames_ordered()): TRUE names the
# endogenous variables, FALSE or "" none. NA when it cannot be read here.
ordered_as_read <- function(value, roles) {
  if (is.null(value) || isFALSE(value)) {
    return(character())
  }
  if (isTRUE(value)) {
    return(unique(unlist(roles$ov_nox)))
  }
  if (!is.character(value) || anyNA(value)) {
    return(NA_character_)
  }
  value[nzchar(value)]
}

stop_stored_data_args <- function(changed) {
  cli_abort(
    c(
      "The original data of the fit are not found, so {.fn update} cannot
       change {.arg {changed}}.",
      "i" = "The data stored in the fit are read with the options of the
             fit. Pass the data again with {.arg data}."
    ),
    class = "inlavaan_update_data_not_found"
  )
}
