# Progress reporting, at three levels: `silent = TRUE` shows nothing, the
# default shows one line per stage, and `verbose = TRUE` adds indented grey
# detail lines under each one. Each stage gets a cli spinner line, which
# appears only when the stage runs for longer than the user's
# `cli.progress_show_after` seconds, or half a second when that option is
# unset (cli's own default is 2), and ends with one coloured line. The lines
# are built directly from cli's styles rather than through its glue-based
# formatters, which cost a few milliseconds a call: as long as a whole fit of
# a small model. The detail lines are only built when they are shown.

# `show`: print anything at all; `detail`: add the detail lines. The warnings
# raised during the fit are recorded in `warnings`.
progress_new <- function(show, detail = FALSE, frame = parent.frame()) {
  p <- new.env(parent = emptyenv())
  p$show <- show
  p$detail <- show && detail
  p$frame <- frame
  p$id <- NULL
  p$calls <- FALSE # whether the log-posterior wrapper should tick the bar
  p$every <- 16L # it ticks every so many calls, or every call for a slow one
  p$warnings <- character()
  p
}

# Start a stage. `msg` is the line shown while it runs, a cli format string
# that counts with `{cli::pb_current}` or `pb_item`. It is the stage's
# sentence, about as wide as the line that ends it, never a cli bar, which
# stretches to the width of the console, and progress_clock() ends it with the
# time, where the line that ends it puts the time taken. With `total`, the
# stage has that many items; without, the log-posterior wrapper ticks it with
# the number of calls. The delay is set while the bar is created (cli reads it
# then) and the option is restored afterwards. A user's own setting is kept,
# so a knitr or Quarto setup chunk can keep bars, which cannot redraw in place
# there, out of the document. `waited` is the seconds already gone before this
# stage, which count towards the delay.
progress_start <- function(p, msg, total = NA, waited = 0) {
  if (!p$show) {
    return(invisible(p))
  }
  old <- options(
    cli.progress_show_after = getOption(
      "cli.progress_show_after",
      max(0.5 - waited, 0)
    )
  )
  on.exit(options(old))
  p$id <- cli::cli_progress_bar(
    format = paste0(
      "{cli::pb_spin} ",
      msg,
      "{progress_clock(cli::pb_elapsed_raw, cli::pb_total, cli::pb_eta_raw)}"
    ),
    total = total,
    clear = TRUE,
    .envir = p$frame
  )
  p$calls <- is.na(total)
  invisible(p)
}

# A first call longer than this, in seconds, makes every call tick the line. A
# tick costs a microsecond or two, which matters only beside a very fast
# function, and 16 calls of less than this are too short to notice.
progress_slow_s <- 0.01

# The calls so far, in a line that counts them
pb_calls <- "{cli::pb_current} call{?s}"

# The item being worked on, "4/6", counted from 1 as in INLAvaan
pb_item <- "{min(cli::pb_current + 1, cli::pb_total)}/{cli::pb_total}"

# The grey bracket that ends a running stage's line, where progress_done()
# puts the time the stage took: the time left for a stage with a known total,
# once cli can estimate it, and the time so far for one without. It appears
# after two seconds, when the estimate has settled, in whole seconds so that
# it does not flicker. cli gives "" for its variables outside a bar.
progress_clock <- function(secs, total, left) {
  if (!is.numeric(secs) || secs < 2) {
    return("")
  }
  if (is.na(total)) {
    return(paste0(" ", grey("[", format_clock(secs), "]")))
  }
  if (inherits(left, "difftime")) {
    left <- as.numeric(left, units = "secs")
  }
  if (!is.numeric(left) || !is.finite(left)) {
    return("")
  }
  paste0(" ", grey("[~", format_clock(left), " left]"))
}

# Whole seconds, at least one: "3s", "1m 5s"
format_clock <- function(s) {
  s <- max(1, round(s))
  if (s < 60) paste0(s, "s") else format_time(s)
}

progress_tick <- function(p, set = NULL) {
  if (!is.null(p$id)) {
    if (is.null(set)) {
      cli::cli_progress_update(id = p$id, .envir = p$frame)
    } else {
      cli::cli_progress_update(id = p$id, set = set, .envir = p$frame)
    }
  }
}

# End a stage's line without one of its own.
progress_close <- function(p) {
  if (!is.null(p$id)) {
    cli::cli_progress_done(id = p$id, .envir = p$frame)
  }
  p$id <- NULL
  p$calls <- FALSE
  invisible(p)
}

# End a stage with a tick (or a yellow ! when `ok` is FALSE), `text`, and the
# time it took.
progress_done <- function(p, text, secs, ok = TRUE) {
  if (!p$show) {
    return(invisible(p))
  }
  progress_close(p)
  message(
    if (ok) cli::col_green(cli::symbol$tick) else cli::col_yellow("!"),
    " ",
    text,
    " ",
    cli::col_grey(paste0("[", format_time(secs), "]"))
  )
  invisible(p)
}

# Highlight a value in a progress line.
hl <- function(x) {
  col_vanilla(x)
}

# Verbose detail ---------------------------------------------------------------
#
# The report_*() functions are called only when p$detail is TRUE, and use
# only what the fit has already computed, with no further calls of logpost or
# grad.

# A line of its own, marked with an i in the package's colour.
progress_info <- function(p, text) {
  if (p$detail) {
    message(wrap_detail(paste(col_vanilla(cli::symbol$info), text), 0))
  }
}

# The same at the default level, for something the user should know. Returns
# whether it was shown.
progress_note <- function(p, text) {
  if (p$show) {
    message(wrap_detail(paste(col_vanilla(cli::symbol$info), text), 0))
  }
  invisible(p$show)
}

# Detail lines, already styled (grey, with any flags in yellow), indented
# under the stage line, one message each.
progress_detail <- function(p, lines) {
  if (p$detail) {
    for (line in lines) {
      message(wrap_detail(line, 2))
    }
  }
}

# A line indented by `indent` spaces and wrapped to out_width(), with
# continuation lines indented two spaces more. A line that fits is left as
# it is, so the aligned rows of a table keep their spacing.
wrap_detail <- function(line, indent) {
  w <- out_width()
  if (cli::ansi_nchar(line) + indent <= w) {
    return(paste0(strrep(" ", indent), line))
  }
  paste(
    cli::ansi_strwrap(line, width = w, indent = indent, exdent = indent + 2),
    collapse = "\n"
  )
}

grey <- function(...) {
  cli::col_grey(paste0(...))
}

flag <- function() {
  cli::col_yellow("!")
}

with_sd <- function(x) {
  paste0(fmt_num(x), sd_unit())
}

plural <- function(n, word) {
  paste0(n, " ", word, if (n == 1) "" else "s")
}

# "12 logpost calls", "3 logpost and 40 grad calls",
# "3 logpost, 40 grad and 2 hess calls"
format_calls <- function(n_lp, n_gr, n_he = 0L) {
  parts <- c(
    if (n_lp > 0 || n_gr + n_he == 0) paste(n_lp, "logpost"),
    if (n_gr > 0) paste(n_gr, "grad"),
    if (n_he > 0) paste(n_he, "hess")
  )
  paste0(
    format_names(parts),
    if (n_lp + n_gr + n_he == 1) " call" else " calls"
  )
}

# "a", "a and b", "a, b and c", "a, b, c, d, e and 25 more"
format_names <- function(x, max = 6L) {
  n <- length(x)
  if (n > max) {
    x <- c(x[seq_len(max - 1L)], paste(n - max + 1L, "more"))
    n <- max
  }
  if (n == 1L) x else paste(paste(x[-n], collapse = ", "), "and", x[n])
}

format_value <- function(x) {
  format(round(x, 2), nsmall = 2)
}

# Before the mode search: the parameters, where the derivatives come from, and
# logpost at init.
report_start <- function(
  p,
  init,
  lp_init,
  has_grad,
  has_hess,
  has_hess_mode = FALSE
) {
  m <- length(init)
  progress_info(
    p,
    paste0(plural(m, "parameter"), ": ", format_names(names(init)))
  )
  lines <- grey(
    if (has_grad) {
      "Gradient from grad"
    } else {
      "Derivatives by finite differences of logpost"
    }
  )
  if (has_hess) {
    lines <- c(lines, grey("Hessian from hess, checked at the mode"))
  }
  if (has_hess_mode) {
    lines <- c(lines, grey("Hessian at the mode from hess_mode, checked there"))
  }
  lines <- c(lines, grey("logpost at init ", format_value(lp_init[[1L]])))
  progress_detail(p, lines)
}

# The mode and the Laplace approximation.
report_mode <- function(p, lap, calls) {
  opt <- lap$opt
  ev <- opt$evaluations
  lines <- grey(
    "nlminb: ",
    opt$message,
    ", after ",
    plural(opt$iterations, "iteration"),
    " (",
    ev[["function"]],
    " function and ",
    ev[["gradient"]],
    " gradient evaluations)"
  )
  rs <- lap$restart
  if (!is.null(rs)) {
    first <- if (is.finite(rs$grad_mode)) {
      paste0("whitened gradient ", with_sd(rs$grad_mode))
    } else {
      "Hessian not positive definite"
    }
    lines <- c(
      lines,
      grey(
        "Restarted nlminb with each parameter scaled by its curvature (first ",
        "run: ",
        plural(rs$iterations, "iteration"),
        ", ",
        first,
        ", restart: ",
        plural(rs$restart_iterations, "iteration"),
        "); ",
        if (rs$kept) "kept the restart" else "kept the first run"
      )
    )
  }
  # The whitened gradient decides the restart, and the Newton step (INLAvaan's
  # mode shift, in posterior SDs) the warning
  msm <- max(lap$mode_shift)
  lines <- c(
    lines,
    paste0(
      grey(
        "logpost at the mode ",
        format_value(lap$lp_max),
        ", whitened gradient ",
        with_sd(lap$grad_mode),
        ", Newton step ",
        with_sd(msm)
      ),
      if (any(off_level(lap$mode_shift, warn_levels$mode_shift))) {
        flag()
      } else {
        ""
      },
      grey(" (warning > ", with_sd(warn_levels$mode_shift), ")")
    )
  )
  hs <- lap$hessian
  gap <- if (all(is.na(lap$grad_gap))) {
    NA_real_
  } else {
    max(lap$grad_gap, na.rm = TRUE)
  }
  asymmetry_line <- paste0(
    grey("Asymmetry of the Hessian ", fmt_num(lap$asymmetry)),
    if (lap$asymmetry > 1e-3) flag() else "",
    grey(" (warning > 0.001)")
  )
  gap_line <- paste0(
    grey("Gap between grad and logpost ", fmt_num(gap)),
    if (isTRUE(off_level(gap, warn_levels$grad_gap))) flag() else "",
    grey(" (warning > ", warn_levels$grad_gap, ")")
  )
  lines <- c(
    lines,
    if (hs$by %in% c("hess", "hess_mode")) {
      c(
        grey(
          if (hs$by == "hess") {
            "Hessian from hess: "
          } else {
            "Hessian from hess_mode: "
          },
          plural(hs$calls, "call"),
          ", and ",
          hs$step_calls,
          " logpost calls to check grad"
        ),
        asymmetry_line,
        paste0(
          grey(
            "Discrepancy from central differences of grad ",
            fmt_num(lap$hess_error)
          ),
          if (!isTRUE(lap$hess_error <= 1e-3)) flag() else "",
          grey(" (warning > 0.001), ", plural(hs$check_calls, "grad call"))
        ),
        gap_line
      )
    } else if (hs$by == "grad") {
      c(
        grey(
          "Hessian by central differences of grad: ",
          plural(hs$calls, "call"),
          ", and ",
          hs$step_calls,
          " logpost calls to set the steps"
        ),
        asymmetry_line,
        gap_line
      )
    } else {
      grey(
        "Hessian by second differences of logpost: ",
        plural(hs$calls, "call"),
        ", and ",
        hs$step_calls,
        " more to set the steps"
      )
    }
  )
  # The smallest eigenvalue of the correlation matrix is near 0 when some
  # combination of the parameters is poorly identified; the ratio of the
  # largest to it is the condition number of the Hessian in posterior-SD
  # units, which the fit-time warning checks
  if (length(lap$mode) > 1L) {
    rho <- stats::cov2cor(lap$Sigma)
    upper <- which(upper.tri(rho), arr.ind = TRUE)
    k <- which.max(abs(rho[upper]))
    pair <- rownames(rho)[upper[k, ]]
    ev_min <- min(eigen(rho, symmetric = TRUE, only.values = TRUE)$values)
    lines <- c(
      lines,
      grey(
        "Posterior correlation matrix: largest correlation ",
        fmt_num(rho[upper][k]),
        " (",
        pair[1],
        ", ",
        pair[2],
        "), smallest eigenvalue ",
        fmt_num(ev_min),
        if (ev_min < 0.01) {
          ": some combination of the parameters is poorly identified"
        } else {
          ""
        }
      ),
      paste0(
        grey("Hessian condition number ", fmt_num(lap$hess_cond_cor)),
        if (off_level(lap$hess_cond_cor, warn_levels$hess_cond)) {
          flag()
        } else {
          ""
        },
        grey(
          ", in SD units (warning > ",
          fmt_num(warn_levels$hess_cond),
          ")"
        )
      )
    )
  }
  lines <- c(
    lines,
    grey("Laplace log marginal likelihood ", format_value(lap$logml)),
    grey(format_calls(calls[[1]], calls[[2]], calls[[3]]))
  )
  progress_detail(p, lines)
}

# The VB correction: the fixed-point steps, the fallback, the term it adds to
# the log marginal likelihood and the largest shift.
report_vb <- function(p, vbc, sds, logml_laplace, calls) {
  h <- vbc$history
  steps <- vapply(h$steps, fmt_num, "")
  n <- length(steps)
  if (n == 0L) {
    steps <- "none"
  } else if (n > 8L) {
    steps <- c(steps[1:3], "...", steps[(n - 2L):n])
  }
  lines <- grey(
    "Fixed-point steps, largest over the parameters (",
    trimws(sd_unit()),
    "): ",
    paste(steps, collapse = ", ")
  )
  n_acc <- sum(h$accelerated)
  n_cut <- sum(h$cut)
  lines <- c(
    lines,
    if (isTRUE(vbc$one_pass)) {
      c(
        grey("One step, accepted (guard > ", with_sd(vb_onestep_guard), ")"),
        if (!is.na(vbc$gain_ratio)) {
          paste0(
            grey("Gain ratio ", fmt_num(vbc$gain_ratio)),
            if (isTRUE(below_level(vbc$gain_ratio, warn_levels$vb_gain))) {
              flag()
            } else {
              ""
            },
            grey(" (warning < ", warn_levels$vb_gain, ")")
          )
        }
      )
    } else if (isTRUE(h$ran_out)) {
      grey(
        "The first step was over the guard. Stopped after ",
        plural(vbc$iterations, "iteration"),
        ", at the budget of the mode and Hessian stage"
      )
    } else if (vbc$fallback) {
      # Where it landed: the step still to go there, marked when the
      # correction did not settle
      step_max <- max(vbc$step_sigma)
      c(
        grey(
          "Fell back to nlminb, as ",
          h$reason,
          ": ",
          h$nlminb,
          ", after ",
          plural(vbc$iterations, "iteration"),
          if (isTRUE(h$rerun)) ", rerun once from where it landed"
        ),
        paste0(
          grey("One more step would move a mean by ", with_sd(step_max)),
          if (off_level(step_max, warn_levels$vb_step)) flag() else "",
          grey(" (warning > ", with_sd(warn_levels$vb_step), ")")
        )
      )
    } else {
      grey(
        "Converged in ",
        plural(vbc$iterations, "iteration"),
        if (n_cut > 0) {
          paste0(", ", plural(n_cut, "step"), " cut to ", with_sd(vb_maxstep))
        },
        if (n_acc > 0) {
          paste0(", Anderson acceleration at ", n_acc, " of them")
        } else {
          ", without Anderson acceleration"
        },
        "; no fallback"
      )
    }
  )
  shift_sd <- vbc$shift / sds
  j <- which.max(abs(shift_sd))
  lines <- c(
    lines,
    paste0(
      grey("Largest shift ", with_sd(shift_sd[[j]])),
      if (off_level(abs(shift_sd[[j]]), warn_levels$vb_shift)) flag() else "",
      grey(
        ", for ",
        names(vbc$shift)[j],
        " (warning > ",
        with_sd(warn_levels$vb_shift),
        ")"
      )
    ),
    grey(
      "KL term ",
      fmt_num(vbc$kld_global),
      ", giving a log marginal likelihood of ",
      format_value(logml_laplace + vbc$kld_global)
    ),
    grey(format_calls(calls[[1]], calls[[2]], calls[[3]]))
  )
  progress_detail(p, lines)
}

# The skew-normal marginals: one aligned line per parameter, with the values
# that summary() flags marked, at its levels. A parameter with NA in `method`
# (the `gaussian_marginals` of vanilla_core()) has a Gaussian marginal and no
# scan, and shows a dash.
report_marginals <- function(
  p,
  sn,
  calls,
  method = NULL,
  marginal_scan = "linear"
) {
  rn <- rownames(sn)
  gauss <- if (is.null(method)) logical(length(rn)) else is.na(method)
  tails <- sn[, "tails"]
  cols <- list(
    Shape = formatC(sn[, "alpha"], digits = 2, format = "f"),
    NMAD = formatC(sn[, "nmad"], digits = 2, format = "g"),
    `Ridge bend` = formatC(sn[, "ridge_bend"], digits = 2, format = "g"),
    Tails = ifelse(is.na(tails), "NA", ifelse(tails == 1, "ok", "heavy")),
    Unscanned = formatC(sn[, "unscanned_mass"], digits = 2, format = "g"),
    gamma1 = formatC(sn[, "gamma1"], digits = 2, format = "g")
  )
  cols <- lapply(cols, function(v) replace(v, gauss, "-"))
  flagged <- function(x, level) !gauss & check_ran(x) & off_level(x, level)
  none <- logical(length(rn))
  flags <- list(
    Shape = none,
    NMAD = flagged(sn[, "nmad"], warn_levels$nmad),
    `Ridge bend` = flagged(sn[, "ridge_bend"], summary_levels$ridge_bend),
    Tails = !gauss & check_ran(tails) & !tails %in% 1,
    Unscanned = flagged(sn[, "unscanned_mass"], warn_levels$unscanned_mass),
    gamma1 = none
  )
  fallback <- !gauss & !is.null(method) & method != marginal_scan
  rw <- max(nchar(rn))
  widths <- vapply(
    names(cols),
    function(k) max(nchar(k), nchar(cols[[k]])),
    numeric(1)
  )
  # Right-aligned, each value followed by its flag or a space; the last
  # column is never flagged
  header <- grey(
    formatC("", width = rw),
    paste0("  ", mapply(formatC, names(cols), width = widths), collapse = "")
  )
  rows <- vapply(
    seq_along(rn),
    function(i) {
      cells <- vapply(
        names(cols),
        function(k) {
          paste0(
            grey(formatC(cols[[k]][i], width = widths[[k]])),
            if (isTRUE(flags[[k]][i])) flag() else " "
          )
        },
        ""
      )
      sub(
        " $",
        "",
        paste0(
          grey(formatC(rn[i], width = -rw)),
          " ",
          paste0(" ", cells, collapse = "")
        )
      )
    },
    ""
  )
  progress_detail(
    p,
    c(
      header,
      rows,
      paste0(
        flag(),
        grey(
          " marks what summary() flags: NMAD > ",
          warn_levels$nmad,
          ", ridge bend > ",
          summary_levels$ridge_bend,
          ", heavy tails, unscanned mass > ",
          warn_levels$unscanned_mass
        )
      ),
      if (marginal_scan != "linear") {
        grey("Scans by marginal_scan = \"", marginal_scan, "\"")
      },
      if (any(gauss)) {
        grey(
          "Gaussian marginals, not scanned, for ",
          paste(rn[gauss], collapse = ", ")
        )
      },
      if (any(fallback)) {
        grey(
          "TK fallback (ridge bend > ",
          summary_levels$ridge_bend,
          ") for ",
          paste(rn[fallback], collapse = ", ")
        )
      },
      grey(format_calls(calls[[1]], calls[[2]], calls[[3]]))
    )
  )
}

# The end of the fit: the calls and time in all, and the warnings.
report_end <- function(p, n_lp, n_gr, n_he, secs) {
  n_warn <- length(p$warnings)
  progress_info(
    p,
    paste0(
      "Done: ",
      format_calls(n_lp, n_gr, n_he),
      " in ",
      format_time(secs),
      if (n_warn > 0) {
        paste0(", with ", cli::col_yellow(plural(n_warn, "warning")))
      } else {
        ""
      }
    )
  )
}
