# Finite differences. Every step `h` is per coordinate and recycled to the
# length of `x`, so callers can scale the steps by the posterior SDs once those
# are known.

# Central-difference gradient of a scalar function: 2m calls of `fn`.
fd_grad <- function(fn, x, h) {
  m <- length(x)
  h <- rep_len(h, m)
  g <- numeric(m)
  for (j in seq_len(m)) {
    xp <- xm <- x
    xp[j] <- x[j] + h[j]
    xm[j] <- x[j] - h[j]
    g[j] <- (fn(xp) - fn(xm)) / (2 * h[j])
  }
  g
}

# Central-difference Jacobian of a gradient function: 2m calls of `gr`.
fd_jacobian <- function(gr, x, h) {
  m <- length(x)
  h <- rep_len(h, m)
  H <- matrix(0, m, m)
  for (j in seq_len(m)) {
    xp <- xm <- x
    xp[j] <- x[j] + h[j]
    xm[j] <- x[j] - h[j]
    H[, j] <- (gr(xp) - gr(xm)) / (2 * h[j])
  }
  H
}

# Hessian of a scalar function from function values alone. The diagonal uses
# the usual three-point second difference. Each off-diagonal entry reuses the
# axial points and needs only two new calls,
#   f(x + hi ei + hj ej) + f(x - hi ei - hj ej) - f(x +- hi ei) - f(x +- hj ej)
#     + 2 f(x) = 2 hi hj H_ij + O(h^4),
# so the error is O(h^2), as for the four-point formula, at m^2 + m + 1 calls
# of `fn` rather than 2m^2 + 1.
fd_hessian <- function(fn, x, h, f0 = fn(x)) {
  m <- length(x)
  h <- rep_len(h, m)
  fp <- fm <- numeric(m)
  H <- matrix(0, m, m)
  for (i in seq_len(m)) {
    xp <- xm <- x
    xp[i] <- x[i] + h[i]
    xm[i] <- x[i] - h[i]
    fp[i] <- fn(xp)
    fm[i] <- fn(xm)
    H[i, i] <- (fp[i] - 2 * f0 + fm[i]) / h[i]^2
  }
  if (m > 1) {
    for (i in seq_len(m - 1)) {
      for (j in (i + 1):m) {
        xpp <- xmm <- x
        xpp[c(i, j)] <- x[c(i, j)] + h[c(i, j)]
        xmm[c(i, j)] <- x[c(i, j)] - h[c(i, j)]
        H[i, j] <- H[j, i] <- (fn(xpp) +
          fn(xmm) -
          fp[i] -
          fm[i] -
          fp[j] -
          fm[j] +
          2 * f0) /
          (2 * h[i] * h[j])
      }
    }
  }
  H
}

# Second derivative of a scalar function along direction `v` at `x`, from
# three calls (two when f(x) is supplied).
fd_curvature <- function(fn, x, v, h, f0 = fn(x)) {
  (fn(x + h * v) - 2 * f0 + fn(x - h * v)) / h^2
}

# Format a number of seconds for messages: "593ms", "2.8s", "1m 5s". The
# unit is chosen after rounding, so 0.9996 is "1.0s", not "1000ms", and 119.6
# is "2m 0s", not "1m 60s".
format_time <- function(s) {
  if (round(1000 * s) < 1000) {
    paste0(round(1000 * s), "ms")
  } else if (round(s, 1) < 60) {
    paste0(format(round(s, 1), nsmall = 1), "s")
  } else {
    s <- round(s)
    paste0(s %/% 60, "m ", s %% 60, "s")
  }
}

elapsed <- function() {
  proc.time()[["elapsed"]]
}

# The package's colour in the console: vanilla, the amber of a cured vanilla
# pod, between the bean and the orchid's throat of the logo
# (lab/logo/logo.R). It marks the rules and the values to read first. cli
# shows the nearest colour a console has, or none.
vanilla_hex <- "#B5772E"

col_vanilla <- function(x) {
  cli::make_ansi_style(vanilla_hex)(x)
}

# The width of everything the package prints: the console's, but never more
# than 80 columns. cli::console_width() follows options(width) in knitr, so a
# document can set a narrower width for its output.
out_width <- function() {
  min(cli::console_width(), 80L)
}

# Errors and warnings, formatted by cli but raised with base R. cli_abort()
# and cli_warn() go through rlang, which cli only suggests, so they could
# fail where rlang is not installed. Conditions carry the class
# "inlavanilla_error" or "inlavanilla_warning". An error raised while another
# is being handled keeps it as `parent`, and its message is appended.
abort <- function(
  message,
  class = NULL,
  parent = NULL,
  call = NULL,
  .envir = parent.frame()
) {
  old <- options(cli.condition_width = out_width())
  on.exit(options(old))
  msg <- cli::format_error(message, .envir = .envir)
  if (!is.null(parent)) {
    msg <- paste0(msg, "\nCaused by error: ", conditionMessage(parent))
  }
  stop(structure(
    class = c(class, "inlavanilla_error", "error", "condition"),
    list(message = msg, call = call, parent = parent)
  ))
}

# A warning is wrapped to out_width(), like the rest of the output. It also
# carries its message as plain text, with no colours and no line wrapping,
# for the record of warnings in a fit (see warning_text()).
warn <- function(message, class = NULL, .envir = parent.frame()) {
  force(.envir)
  old <- options(cli.condition_width = out_width())
  on.exit(options(old))
  warning(structure(
    class = c(class, "inlavanilla_warning", "warning", "condition"),
    list(
      message = cli::format_warning(message, .envir = .envir),
      call = NULL,
      text = format_plain(message, .envir)
    )
  ))
}

format_plain <- function(message, .envir) {
  old <- options(
    cli.condition_width = Inf,
    cli.width = Inf,
    cli.num_colors = 1
  )
  on.exit(options(old))
  cli::format_warning(message, .envir = .envir)
}

# The message of a warning as plain text: the package's own from warn(), any
# other with its colours stripped.
warning_text <- function(w) {
  if (inherits(w, "inlavanilla_warning") && is.character(w[["text"]])) {
    w[["text"]]
  } else {
    cli::ansi_strip(conditionMessage(w))
  }
}

# The checks that summary() flags, each TRUE when flagged: the gap between
# grad and logpost, the mode, the VB step and the check of the one step, the
# Hessian condition number, the VB shift, and, for skew-normal
# marginals, NMAD, the unscanned mass, the tails, the ridge bend and the mean
# gap. `dg` is from diagnostics_internal(). The levels are those of the
# fit-time warning (warn_levels), which raises every one of these but the ridge
# bend and the mean gap (summary_levels), and the check of the one step, which
# has a warning of its own (warn_vb_check()). A check that did not run is not
# flagged; a value that is not finite is. print() counts the flags, and
# summary() prints one "!" line for each and colours the flagged cells of its
# table, all from param_flags().
check_flags <- function(dg, gaussian) {
  pf <- param_flags(dg, gaussian)
  c(
    grad_gap = any(pf[, "grad_gap"]),
    mode = any(pf[, "mode_shift_sigma"]),
    vb_step = any(pf[, "vb_step_sigma"]),
    vb_check = vb_solved(dg),
    hess_cond = off_level(
      dg$global[["hess_cond_cor"]],
      warn_levels$hess_cond
    ),
    vb = any(pf[, "vb_shift_sigma"]),
    nmad = any(pf[, "nmad"]),
    unscanned_mass = any(pf[, "unscanned_mass"]),
    tails = any(pf[, "tails"]),
    ridge_bend = any(pf[, "ridge_bend"]),
    mean_gap = any(pf[, "mean_gap"])
  )
}

# Whether the one-step check failed and the VB equation was solved instead.
# The fit is then flagged by `vb_check`, and the step still to go where the
# solve stopped is recorded but is not a flag: the solve stops at a budget
# that is not a setting of the user's.
vb_solved <- function(dg) {
  isTRUE(dg$global[["vb_solve"]] == 1)
}

# The per-parameter flags behind check_flags(): a logical matrix with a row
# per parameter and a column per check, named as in dg$param.
param_flags <- function(dg, gaussian) {
  p <- dg$param
  sn <- !gaussian
  vb <- isTRUE(dg$global[["vb_applied"]] == 1)
  cbind(
    grad_gap = check_ran(p$grad_gap) &
      off_level(p$grad_gap, warn_levels$grad_gap),
    mode_shift_sigma = off_level(p$mode_shift_sigma, warn_levels$mode_shift),
    vb_step_sigma = vb &
      !vb_solved(dg) &
      check_ran(p$vb_step_sigma) &
      off_level(p$vb_step_sigma, warn_levels$vb_step),
    vb_shift_sigma = vb &
      off_level(abs(p$vb_shift_sigma), warn_levels$vb_shift),
    nmad = sn & check_ran(p$nmad) & off_level(p$nmad, warn_levels$nmad),
    unscanned_mass = sn &
      check_ran(p$unscanned_mass) &
      off_level(p$unscanned_mass, warn_levels$unscanned_mass),
    tails = sn & check_ran(p$tails) & !p$tails %in% TRUE,
    ridge_bend = sn &
      check_ran(p$ridge_bend) &
      off_level(p$ridge_bend, summary_levels$ridge_bend) &
      !scanned_laplace(dg),
    mean_gap = sn &
      vb &
      check_ran(p$mean_gap) &
      off_level(abs(p$mean_gap), summary_levels$mean_gap)
  )
}

# The posterior-SD unit, in plain ASCII where the output cannot show UTF-8.
sd_unit <- function() {
  if (cli::is_utf8_output()) "\u03c3" else " SD"
}

fmt_num <- function(x) {
  if (!is.finite(x)) {
    return(format(x))
  }
  format(signif(x, 2), scientific = x != 0 && (abs(x) < 1e-3 || abs(x) >= 1e5))
}

# Checks of arguments, with the package's own errors

# The parameters whose marginals are Gaussian, as indices: given as indices or
# names, or NULL for none.
check_gaussian_marginals <- function(x, parnames) {
  if (is.null(x) || length(x) == 0L) {
    return(integer())
  }
  idx <- if (is.character(x)) {
    match(x, parnames)
  } else if (is.numeric(x) && all(is.finite(x)) && all(x == round(x))) {
    as.integer(x)
  } else {
    NA_integer_
  }
  if (anyNA(idx) || any(idx < 1L | idx > length(parnames))) {
    abort(
      "{.arg gaussian_marginals} must index or name parameters in {.arg init}."
    )
  }
  sort(unique(idx))
}

check_function <- function(f, arg) {
  if (!is.function(f)) {
    abort(
      "{.arg {arg}} must be a function, not {.obj_type_friendly {f}}."
    )
  }
}

# The `cores` argument, as an integer, or NULL to follow the user's plan.
check_cores <- function(cores) {
  if (is.null(cores)) {
    return(NULL)
  }
  ok <- is.numeric(cores) &&
    length(cores) == 1L &&
    is.finite(cores) &&
    cores >= 1 &&
    cores == round(cores)
  if (!ok) {
    abort(
      "{.arg cores} must be a single positive whole number, or {.code NULL}
       to follow {.code future::plan()}."
    )
  }
  as.integer(cores)
}

check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    abort("{.arg {arg}} must be {.code TRUE} or {.code FALSE}.")
  }
}

# The VB method: one of "onestep", "full" and "none", where TRUE is the
# default and FALSE is "none", as in INLAvaan's `vb_correction`
check_vb <- function(vb_correction) {
  if (isTRUE(vb_correction)) {
    return("onestep")
  }
  if (isFALSE(vb_correction)) {
    return("none")
  }
  check_choice(vb_correction, c("onestep", "full", "none"), "vb_correction")
}

# The marginals: "skewnorm" or "gaussian", with "skewnormal" (in any case) a
# permanent alias of "skewnorm"
check_marginals <- function(marginals) {
  if (
    is.character(marginals) &&
      length(marginals) == 1L &&
      identical(tolower(marginals), "skewnormal")
  ) {
    return("skewnorm")
  }
  check_choice(marginals, c("skewnorm", "gaussian"), "marginals")
}

# One of `choices` (lower case), or an unambiguous abbreviation of one in any
# case, as with match.arg(): "skew" and "Gauss" both work. The whole vector
# (the default) means the first. The package's own error otherwise.
check_choice <- function(x, choices, arg) {
  if (identical(x, choices)) {
    return(choices[[1L]])
  }
  i <- NA_integer_
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    i <- pmatch(tolower(x), choices)
  }
  if (is.na(i)) {
    abort("{.arg {arg}} must be one of {.or {.val {choices}}}.")
  }
  choices[[i]]
}
