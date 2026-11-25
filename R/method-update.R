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
#' and says so. lavaan has processed these data already (for example, it has
#' dropped the incomplete rows under listwise deletion), so this suits an
#' update that keeps the options on how the data are read, such as `group`,
#' `cluster`, `missing` and `ordered`. To change them, give `data` again.
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
#' fit4 <- update(fit, marginal_method = "sampling", nsamp = 4000)
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
      cl[[nm]] <- extras[[nm]]
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

    if (!evaluate) {
      return(cl)
    }

    # When the data object of the recorded call is gone from the caller's
    # environment, the refit uses the data stored in the fit, with a message
    # (unless the refit is silent). lavaan has processed them already, so
    # this suits an update that keeps the options on how the data are read.
    if (
      !"data" %in% names(extras) && (is.symbol(cl$data) || is.call(cl$data))
    ) {
      found <- tryCatch(
        {
          eval(cl$data, caller)
          TRUE
        },
        error = function(e) FALSE
      )
      if (!found) {
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
    }

    # Warm start (Tier A): reuse the previous posterior mode as the optimiser's
    # starting value whenever the parameter structure is preserved. Harmless
    # when only downstream settings changed (the optimiser simply begins at the
    # mode); skipped when the model structure changed or `start` was set.
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
