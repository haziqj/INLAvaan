# The tuning of a fit, one flat list with a prefix for the stage where one is
# needed. vanilla_control() builds and checks it, and check_control() checks a
# plain list by the same function. Its help is in R/control.R.

# The entries of `nlminb` that have a named entry of their own: the iterations,
# the evaluations and the tolerance of the mode search
nlminb_twins <- c(
  mode_maxit = "iter.max",
  mode_maxeval = "eval.max",
  mode_tol = "rel.tol"
)

vanilla_control <- function(
  mode_maxit = 5000L,
  mode_maxeval = 5000L,
  mode_tol = 1e-10,
  vb_maxit = 25L,
  vb_tol = 1e-3,
  n_grid = 21L,
  log_threshold = -6,
  nlminb = list()
) {
  is_num <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!is.list(nlminb) || (length(nlminb) > 0L && !is_named(nlminb))) {
    abort(
      "{.arg nlminb} must be a named list of {.fn stats::nlminb} controls."
    )
  }
  # A twin in `nlminb` stands in for its named entry, which must then be left
  # at its default. The list returned keeps only the named entry, so that it
  # passes this check again.
  given <- c(
    mode_maxit = !missing(mode_maxit),
    mode_maxeval = !missing(mode_maxeval),
    mode_tol = !missing(mode_tol)
  )
  for (entry in names(nlminb_twins)) {
    twin <- nlminb_twins[[entry]]
    if (is.null(nlminb[[twin]])) {
      next
    }
    if (given[[entry]]) {
      abort(c(
        "Set {.arg {entry}} or {.code nlminb${twin}}, not both.",
        "i" = "They are the same control of the mode search."
      ))
    }
    assign(entry, nlminb[[twin]])
    nlminb[[twin]] <- NULL
  }
  if (length(nlminb) == 0L) {
    nlminb <- list()
  }
  if (!is_num(mode_maxit) || mode_maxit < 1) {
    abort("{.arg mode_maxit} must be a single number of at least 1.")
  }
  if (!is_num(mode_maxeval) || mode_maxeval < 1) {
    abort("{.arg mode_maxeval} must be a single number of at least 1.")
  }
  if (!is_num(mode_tol) || mode_tol <= 0) {
    abort("{.arg mode_tol} must be a single positive number.")
  }
  if (!is_num(vb_maxit) || vb_maxit < 1) {
    abort("{.arg vb_maxit} must be a single number of at least 1.")
  }
  if (!is_num(vb_tol) || vb_tol <= 0) {
    abort("{.arg vb_tol} must be a single positive number.")
  }
  if (!is_num(n_grid) || n_grid < 7) {
    abort("{.arg n_grid} must be a single number of at least 7.")
  }
  if (!is_num(log_threshold) || log_threshold >= 0) {
    abort("{.arg log_threshold} must be a single negative number.")
  }
  list(
    mode_maxit = as.integer(round(mode_maxit)),
    mode_maxeval = as.integer(round(mode_maxeval)),
    mode_tol = mode_tol,
    vb_maxit = as.integer(round(vb_maxit)),
    vb_tol = vb_tol,
    n_grid = as.integer(round(n_grid)),
    log_threshold = log_threshold,
    nlminb = nlminb
  )
}

# A list of controls, from vanilla_control() or written by hand, checked and
# completed with the defaults. An unknown entry is an error, as is an entry
# without a name.
check_control <- function(control) {
  if (!is.list(control)) {
    abort("{.arg control} must be a list, as {.fn vanilla_control} makes.")
  }
  valid <- names(formals(vanilla_control))
  if (length(control) > 0L && !is_named(control)) {
    abort(c(
      "Every {.arg control} entry must be named.",
      "i" = "Valid entries are {.val {valid}}."
    ))
  }
  unknown <- setdiff(names(control), valid)
  if (length(unknown) > 0L) {
    abort(c(
      "Unknown {.arg control} entr{?y/ies}: {.val {unknown}}.",
      "i" = "Valid entries are {.val {valid}}."
    ))
  }
  do.call(vanilla_control, control)
}

# The controls of stats::nlminb() for the mode search
mode_nlminb <- function(ctrl) {
  c(
    list(
      iter.max = ctrl$mode_maxit,
      eval.max = ctrl$mode_maxeval,
      rel.tol = ctrl$mode_tol
    ),
    ctrl$nlminb
  )
}

is_named <- function(x) {
  nms <- names(x)
  !is.null(nms) && !any(is.na(nms) | nms == "")
}
