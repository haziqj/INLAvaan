# The arguments of inlavaan() that INLAvaan 0.4.0 deprecates, and how each
# old argument or value maps to the new ones. A deprecated argument keeps its
# place among the formals, with the default NULL, until 0.5.0, so that old
# calls run, positional ones included. Each warns once per session (see
# warn_deprecated()) and stops from 0.5.0.

## ----- The deprecated arguments --------------------------------------------

# Arguments that are now entries of `control` (see inlavaan_control()), with
# the entry of each. They give the same fit.
inlavaan_moved_args <- c(
  sn_fit_ngrid = "n_grid",
  sn_fit_logthresh = "log_threshold",
  hessian_method = "hessian",
  debug = "debug",
  add_priors = "add_priors"
)

# Arguments that have no effect, with the reason for each, as cli templates
inlavaan_ended_args <- c(
  n_qmc = "The VB correction now takes a deterministic step, with no
           quasi-Monte Carlo nodes.",
  vb_method = "The VB correction now has one rule, and
               {.code vb_correction = \"full\"} iterates it.",
  samp_copula = "The draws always couple the fitted marginals by a Gaussian
                 copula. With {.code marginal_method = \"gaussian\"} they are
                 the draws of the Laplace approximation.",
  samp_norta = "The draws no longer adjust the correlations of the copula.",
  sn_fit_temp = "The skew-normal marginals are fitted with temperature 1.",
  optim_method = "The posterior mode is always found by {.fn stats::nlminb}.",
  numerical_grad = "The fit always uses the analytic gradient."
)

## ----- Resolution --------------------------------------------------------------

# The settings of a fit from the arguments of inlavaan(). `old` is a named
# list of the deprecated arguments (NULL when not given), `scan_given`
# whether `marginal_scan` was given, and `silent` that of the fit (see
# warn_deprecated()). Returns `control`, the entries set (see
# resolve_control()), with the moved arguments in it, and the canonical
# `vb_correction`, `marginal_method` and `marginal_scan`. A moved argument
# and its entry, or `marginal_correction` and `marginal_scan`, together give
# an error, as both set the same thing.
resolve_inlavaan_args <- function(
  old,
  vb_correction,
  marginal_method,
  marginal_scan,
  scan_given,
  control,
  silent = FALSE
) {
  ctrl <- resolve_control(control, silent = silent)

  for (arg in names(inlavaan_moved_args)) {
    value <- old[[arg]]
    if (is.null(value)) {
      next
    }
    entry <- inlavaan_moved_args[[arg]]
    if (entry %in% names(ctrl)) {
      cli_abort(c(
        "Set {.arg {arg}} or {.code control${entry}}, not both.",
        "i" = "They are the same setting."
      ))
    }
    ctrl[[entry]] <- check_moved_arg(value, arg)
    warn_deprecated(
      arg,
      c(
        "{.arg {arg}} is deprecated, and stops from INLAvaan 0.5.0.",
        "i" = "Use {.code control = inlavaan_control({entry} = )}, which gives
               the same fit."
      ),
      silent = silent
    )
  }
  ctrl <- do.call(inlavaan_control, ctrl)

  for (arg in names(inlavaan_ended_args)) {
    if (is.null(old[[arg]])) {
      next
    }
    warn_deprecated(
      arg,
      c(
        "{.arg {arg}} is deprecated and has no effect. It stops from INLAvaan
         0.5.0.",
        "i" = inlavaan_ended_args[[arg]],
        "i" = "INLAvaan 0.3.5 has the old method."
      ),
      silent = silent
    )
  }

  if (is.null(old$marginal_correction)) {
    marginal_scan <- check_choice(
      marginal_scan,
      c("linear", "exact", "laplace", "plain"),
      "marginal_scan"
    )
  } else {
    if (scan_given) {
      cli_abort(c(
        "Set {.arg marginal_correction} or {.arg marginal_scan}, not both.",
        "i" = "{.arg marginal_scan} replaces {.arg marginal_correction}."
      ))
    }
    marginal_scan <- resolve_marginal_correction(
      old$marginal_correction,
      silent
    )
  }

  list(
    control = ctrl,
    vb_correction = check_vb(vb_correction),
    marginal_method = resolve_marginal_method(marginal_method, silent),
    marginal_scan = marginal_scan
  )
}

# The value of a moved argument, checked under its own name. inlavaan_control()
# then checks it again under the name of its entry.
check_moved_arg <- function(value, arg) {
  switch(
    arg,
    sn_fit_ngrid = {
      if (
        !is.numeric(value) ||
          length(value) != 1L ||
          !is.finite(value) ||
          value < 7
      ) {
        cli_abort("{.arg sn_fit_ngrid} must be a single number of at least 7.")
      }
      value
    },
    sn_fit_logthresh = {
      if (
        !is.numeric(value) ||
          length(value) != 1L ||
          !is.finite(value) ||
          value >= 0
      ) {
        cli_abort(
          "{.arg sn_fit_logthresh} must be a single finite negative number."
        )
      }
      value
    },
    hessian_method = check_choice(value, c("central", "forward"), arg),
    {
      check_flag(value, arg)
      value
    }
  )
}

# marginal_scan from the deprecated marginal_correction: "none" and FALSE scan
# with no volume correction ("plain"), which gives the same fit. The other
# values, three ways to compute the linear volume correction, give "linear".
resolve_marginal_correction <- function(value, silent = FALSE) {
  if (isFALSE(value)) {
    value <- "none"
  }
  value <- check_choice(
    value,
    c("shortcut", "shortcut_fd", "hessian", "none"),
    "marginal_correction"
  )
  scan <- if (value == "none") "plain" else "linear"
  advice <- if (value == "none") {
    "Use {.code marginal_scan = \"plain\"}, which gives the same fit."
  } else {
    "The fit now runs {.code marginal_scan = \"linear\"}, the default, which
     computes the linear volume correction for all parameters at once."
  }
  warn_deprecated(
    paste0("marginal_correction = \"", value, "\""),
    c(
      "{.arg marginal_correction} is deprecated, and stops from INLAvaan
       0.5.0.",
      "i" = advice
    ),
    silent = silent
  )
  scan
}

# The canonical marginal_method, "skewnorm" or "gaussian". "skewnormal" is a
# silent alias of "skewnorm". The old values, also abbreviated, are matched
# only when no new value matches: "marggaus" warns and is "gaussian", which
# gives the same fit, "asymgaus" warns and runs "skewnorm", and "sampling",
# kept for a future sampler, stops.
resolve_marginal_method <- function(value, silent = FALSE) {
  choices <- c("skewnorm", "gaussian")
  if (identical(value, choices)) {
    return("skewnorm")
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(check_choice(value, choices, "marginal_method"))
  }
  x <- tolower(value)
  if (identical(x, "skewnormal")) {
    return("skewnorm")
  }
  old <- c("marggaus", "asymgaus", "sampling")
  i <- if (nzchar(x)) pmatch(x, old) else NA_integer_
  if (!is.na(pmatch(x, choices)) || is.na(i)) {
    return(check_choice(value, choices, "marginal_method"))
  }
  switch(
    old[[i]],
    marggaus = {
      warn_deprecated(
        "marginal_method = \"marggaus\"",
        c(
          "{.code marginal_method = \"marggaus\"} is deprecated, and stops from
           INLAvaan 0.5.0.",
          "i" = "Use {.code marginal_method = \"gaussian\"}, which gives the
                 same fit."
        ),
        silent = silent
      )
      "gaussian"
    },
    asymgaus = {
      warn_deprecated(
        "marginal_method = \"asymgaus\"",
        c(
          "{.code marginal_method = \"asymgaus\"} is deprecated, and stops from
           INLAvaan 0.5.0.",
          "i" = "The fit now runs {.code marginal_method = \"skewnorm\"}, the
                 default.",
          "i" = "INLAvaan 0.3.5 has the old method."
        ),
        silent = silent
      )
      "skewnorm"
    },
    sampling = cli_abort(
      c(
        "{.code marginal_method = \"sampling\"} is not available.",
        "i" = "The name is kept for a sampler of the posterior, which is under
               development.",
        "i" = "{.code marginal_method = \"gaussian\"} gives the Gaussian
               marginals of the Laplace approximation. INLAvaan 0.3.5 has the
               old method."
      ),
      class = "inlavaan_unavailable",
      call = NULL
    )
  )
}

## ----- Post-fit functions --------------------------------------------------

# The deprecated `samp_copula` of sampling(), simulate() and bfit_indices().
# Their draws follow the fitted marginals, as the draws of the fit do.
warn_samp_copula <- function(samp_copula) {
  if (is.null(samp_copula)) {
    return(invisible(FALSE))
  }
  warn_deprecated(
    "samp_copula",
    c(
      "{.arg samp_copula} is deprecated and has no effect. It stops from
       INLAvaan 0.5.0.",
      "i" = inlavaan_ended_args[["samp_copula"]],
      "i" = "INLAvaan 0.3.5 has the old method."
    )
  )
}

## ----- Wrappers ----------------------------------------------------------------

# acfa(), asem() and agrowth() set model.type themselves, so a model.type
# given to them, in full or abbreviated to at least "model.", stops. `sc` is
# the call of the wrapper, and `fn` its name.
check_wrapper_call <- function(sc, fn) {
  given <- names(sc)[-1L] %||% character()
  if (any(nchar(given) > 5L & startsWith("model.type", given))) {
    cli_abort(
      c(
        "{.fn {fn}} sets {.arg model.type} itself.",
        "i" = "Use {.fn inlavaan} to choose {.arg model.type}."
      ),
      class = "inlavaan_wrapper_model_type",
      call = NULL
    )
  }
  invisible(sc)
}

## ----- dp ----------------------------------------------------------------------

# Warn on names of `dp` that no parameter type uses, such as a misspelt type,
# whose prior would otherwise be dropped without a word. `types` are the
# names of priors_for(). blavaan::dpriors() also names ibpsi, a prior that
# blavaan alone uses (for blocks of the latent covariance matrix), so
# dp = blavaan::dpriors() gives no warning.
warn_dp_unused <- function(dp, types) {
  unused <- setdiff(names(dp), c(types, "ibpsi"))
  if (length(unused) > 0L) {
    cli_warn(
      c(
        "{.arg dp} has {qty(length(unused))}{?a name/names} that no parameter
         type uses: {.val {unused}}.",
        "i" = "The types are {.val {types}}. See {.fn priors_for}."
      ),
      class = "inlavaan_dp_unused"
    )
  }
  invisible(unused)
}

## ----- model.type --------------------------------------------------------------

# The lavaan functions that inlavaan() can build its model with
inlavaan_model_types <- c("sem", "cfa", "growth", "lavaan")

# model.type must name one of them. Before, any name of a lavaan function
# reached lavaan, so a misspelt name gave "object not found", and "efa" an
# error about the arguments of efa().
check_model_type <- function(model.type, call = parent.frame()) {
  ok <- is.character(model.type) &&
    length(model.type) == 1L &&
    !is.na(model.type) &&
    model.type %in% inlavaan_model_types
  if (!ok) {
    cli_abort(
      c(
        "{.arg model.type} must be one of {.or {.val {inlavaan_model_types}}}.",
        "x" = "It is {.code {deparse1(model.type)}}."
      ),
      class = "inlavaan_model_type",
      call = call
    )
  }
  invisible(model.type)
}

## ----- data ------------------------------------------------------------------

# A fit needs data: `data`, the sample statistics `sample.cov` (with
# `sample.nobs`), or the stored data of a fit (`slotData`, which the baseline
# fits give). Without any of them, lavaan builds the model with no data, and
# the fit stopped late with a misleading error.
check_data_given <- function(data, lavargs, call = parent.frame()) {
  keys <- arg_key(names(lavargs))
  others <- lavargs[keys %in% c("sample.cov", "slotdata", "slot.data")]
  given <- !is.null(data) || !all(vapply(others, is.null, logical(1)))
  if (!given) {
    cli_abort(
      c(
        "There are no data to fit the model to.",
        "i" = "Give {.arg data}, or {.arg sample.cov} and {.arg sample.nobs}."
      ),
      class = "inlavaan_no_data",
      call = call
    )
  }
  invisible()
}
