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
#' and says so. lavaan has read these data already, with the options of the
#' fit (for example, it has dropped the incomplete rows under listwise
#' deletion). So `update()` then stops with an error of class
#' `inlavaan_update_data_not_found` when the update changes an option on how
#' the data are read, such as `group`, `cluster`, `missing`, `ordered` or
#' `sampling.weights`, or when the new model has other observed variables. To
#' make such a change, give `data` again.
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

    # Replace or extend the model
    if (!missing(model)) {
      cl$model <- model
    } else if (!missing(add)) {
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
      if (is.null(extras[[nm]]) && !nm %in% names(cl)) {
        # As in stats::update(), `arg = NULL` drops an argument of the call,
        # or gives it as NULL when the call does not have it
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

    if (!evaluate) {
      return(cl)
    }

    # When the data object of the recorded call is gone from the caller's
    # environment, the refit uses the data stored in the fit, with a message
    # (unless the refit is silent). lavaan has read them already, with the
    # options of the fit on how the data are read, and would ignore new values
    # of those options. So an update that changes one of them, or the observed
    # variables of the model, stops (see check_stored_data_update()).
    use_stored <- !data_given && length(slot_args) > 0L
    data_lost <- FALSE
    if (!data_given && (is.symbol(cl$data) || is.call(cl$data))) {
      data_lost <- !tryCatch(
        {
          eval(cl$data, caller)
          TRUE
        },
        error = function(e) FALSE
      )
    }
    if (use_stored || data_lost) {
      new_model <- if (!missing(model) || !missing(add)) cl$model
      check_stored_data_update(internal$call, extras, new_model, object)
    }
    if (data_lost) {
      silent <- tryCatch(
        isTRUE(eval(cl$silent, caller)),
        error = function(e) FALSE
      )
      if (!silent) {
        cli_inform(
          c(
            "i" = "The data {.code {deparse1(cl$data)}} of the fit are not
                   found, so {.fn update} uses the data stored in the fit.",
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
    structure_changed <- !missing(model) ||
      !missing(add) ||
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
data_read_args <- c(
  "group",
  "group.label",
  "cluster",
  "level.label",
  "ordered",
  "missing",
  "sampling.weights",
  "sampling.weights.normalization",
  "std.ov",
  "conditional.x",
  "correlation",
  "allow.empty.cell",
  "aux",
  "sample.cov",
  "sample.mean",
  "sample.th",
  "sample.nobs",
  "nacov",
  "wls.v"
)

# Argument names as lavaan reads them: dot or snake case, upper or lower case
arg_key <- function(x) {
  tolower(gsub("_", ".", x, fixed = TRUE))
}

# The observed variables of a model given as lavaan syntax or as a parameter
# table, or NULL when the model cannot be parsed here. lavaan then reports the
# problem when it reads the model.
model_ov_names <- function(model) {
  tryCatch(
    {
      if (is.character(model)) {
        model <- lavaan::lavParseModelString(
          split_modifiers(model),
          parser = "old"
        )
      }
      lavaan::lavNames(model, "ov")
    },
    error = function(e) NULL
  )
}

# Stop when update() refits on the stored data of a fit and changes how the
# data are read: an argument of `data_read_args` that the recorded call does
# not give with the same expression, or a model with other observed variables.
check_stored_data_update <- function(old_call, extras, new_model, object) {
  old_args <- as.list(old_call)[-1L]
  old_keys <- arg_key(names(old_args))
  changed <- character()
  for (nm in names(extras)) {
    key <- arg_key(nm)
    if (!key %in% data_read_args) {
      next
    }
    old <- old_args[old_keys == key]
    if (length(old) != 1L || !identical(old[[1L]], extras[[nm]])) {
      changed <- c(changed, nm)
    }
  }
  if (length(changed) > 0L) {
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
  if (!is.null(new_model)) {
    new_ov <- model_ov_names(new_model)
    if (!is.null(new_ov) && !setequal(new_ov, lavaan::lavNames(object, "ov"))) {
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
  }
  invisible()
}
