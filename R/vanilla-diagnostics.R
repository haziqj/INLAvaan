# The checks of a fit: their levels, the global and per-parameter diagnostics
# (diagnostics_internal()), and the bullets of the fit-time warning
# (diagnostic_problems()).
#
# The diagnostics() methods and the fit-time warning read
# diagnostics_internal(), as INLAvaan's diagnostics() and its
# warn_fit_diagnostics() do, and the flags of summary() and print() come from
# check_flags() on the same numbers, so none of them can
# disagree. The warning checks, in INLAvaan's order, the mode shift, NMAD,
# the unscanned mass, the VB shift and the Hessian condition number, at
# INLAvaan's levels. Three checks are inlavanilla's own: the gap between a user
# gradient and `logpost`, first, since a wrong gradient explains what follows;
# whether the VB correction settled, after the mode shift, since the two
# together are what print() calls converged; and the heavy tails, after the
# unscanned mass.
# INLAvaan's bullet for a non-zero return code of nlminb() is left out:
# without a gradient nlminb() reports false convergence at accurate modes.
# The VB fallback is recorded only, as in INLAvaan: it is a route of the
# solver, and the settling check judges where it lands. summary() flags
# every warned check at the same level, and also the ridge bend and the mean
# gap, which never warn. A check that did not run (VB with vb_correction =
# "none", the scans with Gaussian marginals) is NA and never flagged; one that
# ran and gave a value that is not finite is.
warn_levels <- list(
  mode_shift = 0.1,
  nmad = 0.1,
  unscanned_mass = 0.05,
  vb_shift = 1,
  hess_cond = 1e8,
  # inlavanilla's own: the VB correction settles as the mode search does
  vb_step = 0.1,
  # inlavanilla's own: a mode moved by 0.1 SDs, or an SD 10% off
  grad_gap = 0.1
)
# The ridge bend level is a marginal SD about 10% too small,
# 1 - sqrt(1 - 3 b / 4), in the normal mean with an unknown SD
# (lab/bend-threshold.md).
summary_levels <- list(ridge_bend = 0.25, mean_gap = 0.1)
# Past these levels the mode is not a near miss but is not where the posterior
# is: a Newton step over 1 SD, or more than 0.9 of a marginal's mass beyond
# its scan. On the funnels of lab/whitening-order.R (random effects whose
# scale runs to zero) every fit passed one of them (steps of 1.2 to 67 SDs, or
# unscanned mass 1.00), and none of 104 other fits came near (steps of at most
# 2e-4 SDs, unscanned mass at most 0.55).
no_mode_levels <- list(mode_shift = 1, unscanned_mass = 0.9)

# TRUE where `x` is above `level` or not finite.
off_level <- function(x, level) {
  !(is.finite(x) & x <= level)
}

# Whether a check ran: NA is a check that did not run, and NaN a value it
# produced, which off_level() flags
check_ran <- function(x) {
  !is.na(x) | is.nan(x)
}

# The global and per-parameter diagnostics, from the parts of a fit:
# fit$diagnostics$optim, $vb (NULL when vb_correction = "none") and $marginals,
# and the VB mean (`pars`, for grad_inf_rel and grad_rel; NA when not given). It
# runs before the fit object exists, for the warning. The definitions are
# INLAvaan's (R/method-diagnostics.R), with `grad` the gradient of minus the
# log-posterior; the unscanned mass, the VB shifts and the KL divergences are
# computed in vanilla_core(). The per-parameter frame has no row names, as
# INLAvaan's. `message` is nlminb()'s, for the warning.
diagnostics_internal <- function(optim, vb, marginals, pars = NULL) {
  d <- marginals
  m <- nrow(d)
  g <- unname(optim$grad)
  if (is.null(pars)) {
    pars <- rep(NA_real_, m)
  }
  pars <- unname(pars)
  vb_applied <- !is.null(vb)
  max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  # A fit saved before the check existed has no such column
  grad_gap <- if (is.null(d$grad_gap)) rep(NA_real_, m) else d$grad_gap
  # A fit made before 26.11.24 has only the condition number on the
  # correlation scale, under the name `hess_cond`
  hess_cond <- optim$hess_cond
  hess_cond_cor <- optim$hess_cond_cor
  if (is.null(hess_cond_cor)) {
    hess_cond_cor <- hess_cond
    hess_cond <- NA_real_
  }

  # A fit made before the check of the one step has no such entries
  vb_check <- c(lhat = NA_real_, j0 = NA_real_, j1 = NA_real_)
  if (vb_applied && !is.null(vb$check)) {
    vb_check <- vb$check
  }

  global <- c(
    npar = m,
    converged = as.numeric(optim$convergence == 0L),
    iterations = as.integer(optim$iterations),
    grad_inf = max(abs(g)),
    grad_inf_rel = max(abs(g) / (abs(pars) + 1e-06)),
    grad_l2 = sqrt(sum(g^2)),
    mode_shift_max = max(d$mode_shift_sigma),
    hess_cond = hess_cond,
    hess_cond_cor = hess_cond_cor,
    vb_applied = as.numeric(vb_applied),
    vb_shift_max = max_na(abs(d$vb_shift_sigma)),
    vb_kld_global = if (vb_applied) vb$kld_global else NA_real_,
    kld_max = max_na(d$kld),
    kld_mean = mean_na(d$kld),
    nmad_max = max_na(d$nmad),
    nmad_mean = mean_na(d$nmad),
    unscanned_mass_max = max_na(d$unscanned_mass),
    # inlavanilla's own
    grad_gap_max = max_na(grad_gap),
    vb_fallback = if (vb_applied) as.numeric(vb$fallback) else NA_real_,
    vb_step_max = max_na(d$vb_step_sigma),
    vb_lhat = vb_check[["lhat"]],
    vb_j0 = vb_check[["j0"]],
    vb_j1 = vb_check[["j1"]],
    vb_solve = if (vb_applied && !is.null(vb$solve)) {
      as.numeric(vb$solve)
    } else {
      NA_real_
    },
    mean_gap_max = max_na(abs(d$mean_gap)),
    ridge_bend_max = max_na(d$ridge_bend),
    n_heavy_tails = if (all(is.na(d$tails))) {
      NA_real_
    } else {
      sum(!d$tails, na.rm = TRUE)
    }
  )
  class(global) <- c("inla_vanilla_diagnostics", "numeric")

  param <- data.frame(
    names = rownames(d),
    grad = g,
    grad_abs = abs(g),
    grad_rel = abs(g) / (abs(pars) + 1e-06),
    mode_shift_sigma = d$mode_shift_sigma,
    kld = d$kld,
    vb_shift = d$vb_shift,
    vb_shift_sigma = d$vb_shift_sigma,
    nmad = d$nmad,
    alpha = d$alpha,
    unscanned_mass = d$unscanned_mass,
    # inlavanilla's own
    grad_gap = grad_gap,
    vb_step_sigma = d$vb_step_sigma,
    mean_gap = d$mean_gap,
    gamma1 = d$gamma1,
    tails = d$tails,
    ridge_bend = d$ridge_bend,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(param) <- c("inla_vanilla_diagnostics_param", "data.frame")

  # The scan of each parameter, which decides whether the ridge bend is
  # flagged (it is not for a parameter scanned at its conditional modes);
  # NULL for a fit without it. A fit made before 26.11.24 calls it `width`.
  list(
    global = global,
    param = param,
    message = optim$message,
    marginal_scan = if (is.null(d$marginal_scan)) d$width else d$marginal_scan
  )
}

# TRUE for the parameters scanned at their conditional modes ("laplace", which
# was "tk" before 26.11.24), whose ridge bend is not flagged: it measures what
# the straight scan misses, and those scans do not take it.
scanned_laplace <- function(dg) {
  if (is.null(dg$marginal_scan)) {
    rep(FALSE, nrow(dg$param))
  } else {
    dg$marginal_scan %in% c("laplace", "tk")
  }
}

# The order in which to list parameters: the largest values first, and any
# that are not finite before them.
worst_first <- function(x) {
  order(ifelse(is.finite(x), x, Inf), decreasing = TRUE)
}

# A cli template listing parameters as INLAvaan's warning does,
# "`name` (value), ..." for at most `n` of them, then " and N other(s)".
# The names, and the values formatted to two decimals, are put in `env` as
# `<key>_names` and `<key>_values`, and the template refers to them by
# index. So a name is never read as cli markup (names come from `init` and
# can hold braces), and each list keeps variables of its own: the templates
# are filled in only when the message is shown.
list_template <- function(env, key, names, values = NULL, n = 3L) {
  i <- seq_len(min(n, length(names)))
  names_var <- paste0(key, "_names")
  assign(names_var, names[i], envir = env)
  items <- paste0("{.code {", names_var, "[", i, "]}}")
  if (!is.null(values)) {
    values_var <- paste0(key, "_values")
    assign(
      values_var,
      formatC(values[i], digits = 2, format = "f"),
      envir = env
    )
    items <- paste0(items, " ({", values_var, "[", i, "]})")
  }
  more <- length(names) - length(i)
  paste0(
    paste(items, collapse = ", "),
    if (more > 0L) paste0(" and ", more, " other", if (more > 1L) "s")
  )
}

# The parameters flagged by a per-parameter check, worst first, as a list
# template (see list_template()).
flagged_list <- function(env, key, dg, column, level, n) {
  x <- dg$param[[column]]
  if (column %in% c("vb_shift_sigma", "mean_gap")) {
    x <- abs(x)
  }
  bad <- check_ran(x) & off_level(x, level)
  if (column == "ridge_bend") {
    bad <- bad & !scanned_laplace(dg)
  }
  bad <- which(bad)
  bad <- bad[worst_first(x[bad])]
  list_template(env, key, dg$param$names[bad], x[bad], n = n)
}

# A single value, as INLAvaan's warning formats it.
fmt_g3 <- function(x) {
  trimws(formatC(x, digits = 3, format = "g"))
}

# The sentence for a flagged check, as a cli template whose values are put
# in `env`: INLAvaan's words for the checks it shares, with the SD unit of
# sd_unit(), which summary() also prints with every parameter listed
# (`n = Inf`), and inlavanilla's own for the others. The mode and VB
# bullets name the worst parameter only, as INLAvaan's do; summary() has its
# own lines for those two.
problem_text <- function(check, dg, env, advice, n = 3L) {
  p <- dg$param
  glob <- dg$global
  switch(
    check,
    grad_gap = paste0(
      advice$grad_gap,
      " at the mode (gap > ",
      warn_levels$grad_gap,
      ") for ",
      flagged_list(env, "grad", dg, "grad_gap", warn_levels$grad_gap, n),
      "."
    ),
    mode = {
      env$mode_worst <- p$names[worst_first(p$mode_shift_sigma)[1L]]
      env$mode_grad <- fmt_g3(glob[["grad_inf"]])
      env$mode_shift <- fmt_g3(glob[["mode_shift_max"]])
      paste0(
        "The gradient at the posterior mode is not zero (max |grad| = ",
        "{mode_grad}): a Newton step would move {.code {mode_worst}} by ",
        "{mode_shift}",
        sd_unit(),
        "."
      )
    },
    vb_step = {
      env$vbs_worst <- p$names[worst_first(p$vb_step_sigma)[1L]]
      env$vbs_step <- fmt_g3(glob[["vb_step_max"]])
      paste0(
        "The VB correction did not settle: one more step would move ",
        "{.code {vbs_worst}} by {vbs_step}",
        sd_unit(),
        "."
      )
    },
    nmad = paste0(
      "The fitted marginal deviates from the scanned posterior (NMAD > ",
      warn_levels$nmad,
      ") for ",
      flagged_list(env, "nmad", dg, "nmad", warn_levels$nmad, n),
      "."
    ),
    unscanned_mass = paste0(
      "The fitted marginal puts more than ",
      warn_levels$unscanned_mass,
      " of its mass beyond the scanned window (4",
      sd_unit(),
      " either side of the mode) for ",
      flagged_list(
        env,
        "sem",
        dg,
        "unscanned_mass",
        warn_levels$unscanned_mass,
        n
      ),
      "; its credible limits rely on extrapolation."
    ),
    tails = paste0(
      "Heavy tails for ",
      list_template(
        env,
        "tails",
        p$names[check_ran(p$tails) & !p$tails %in% TRUE],
        n = n
      ),
      ": the density is still over 5% of its peak at the ends of the ",
      "scanned window (4",
      sd_unit(),
      " either side of the mode), and the skew-normal extrapolates beyond it."
    ),
    vb = {
      shift <- abs(p$vb_shift_sigma)
      env$vb_worst <- p$names[worst_first(shift)[1L]]
      env$vb_shift <- fmt_g3(glob[["vb_shift_max"]])
      paste0(
        "The VB correction shifted {.code {vb_worst}} by {vb_shift}",
        sd_unit(),
        "; the Gaussian approximation at the mode may be inaccurate."
      )
    },
    hess_cond = {
      env$hess_cond <- fmt_g3(glob[["hess_cond_cor"]])
      paste0(
        "The Hessian at the mode is near-singular (condition number = ",
        "{hess_cond}); some parameters may be weakly identified."
      )
    },
    ridge_bend = paste0(
      "Ridge bend > ",
      summary_levels$ridge_bend,
      " for ",
      flagged_list(
        env,
        "ridge_bend",
        dg,
        "ridge_bend",
        summary_levels$ridge_bend,
        n
      ),
      ": the marginal SD may be >10% too small."
    ),
    mean_gap = paste0(
      "Mean gap > ",
      with_sd(summary_levels$mean_gap),
      " for ",
      flagged_list(env, "gap", dg, "mean_gap", summary_levels$mean_gap, n),
      ": the posterior is far from Gaussian there, and either mean may be off."
    )
  )
}

# The warning of a flagged one step (vb_checked_onestep()): the check found
# that the fixed-point map does not contract along the step, so the posterior
# is far from Gaussian there. `worst` is the parameter that the one step moves
# furthest. It is a warning of its own, raised by vanilla_core() on every
# flagged fit, whether or not the solve that followed converged, with no
# numbers: those are in the diagnostics. Its class lets a caller catch it.
warn_vb_check <- function(worst) {
  warn(
    c(
      paste0(
        "VB one-step check flagged that the posterior is far from Gaussian ",
        "along the mean correction, most of all in {.code {worst}}."
      ),
      "i" = "Treat posterior means, mloglik, looic and waic with care.",
      "i" = paste0(
        "This reflects the model, the data, or the prior, not a numerical ",
        "failure. If these numbers matter, compare with MCMC."
      )
    ),
    class = "inlavanilla_vb_check_warning"
  )
}

# The wording a caller gives diagnostic_problems() and problem_text(): cli
# templates that name its own arguments, so that each package words the
# bullets and the advice for its users. Every entry is needed:
# * `grad_gap`: the subject of the bullet on a gradient that does not match
#   the log-posterior, such as "{.arg grad} does not match {.arg logpost}".
# * `no_grad`: the advice for a VB correction that did not settle without a
#   gradient, such as "supply {.arg grad} if you can" (a caller that always
#   has a gradient can give "").
# * `vb_none`: the code that asks for no VB correction, such as
#   {.code vb_correction = "none"}.
# * `mode_limit`: how to raise the limits of the mode search, a sentence.
# * `mode_retry`: what else to try when it stops short, a sentence.
# * `funnel`: what to do about random effects whose scale can run to zero, a
#   sentence.
advice_entries <- c(
  "grad_gap",
  "no_grad",
  "vb_none",
  "mode_limit",
  "mode_retry",
  "funnel"
)

check_advice <- function(advice) {
  if (!is.list(advice) || !all(advice_entries %in% names(advice))) {
    abort(
      "{.arg advice} must be a list with the entries {.val {advice_entries}}."
    )
  }
  invisible(advice)
}

# The bullets of the fit-time warning, in INLAvaan's order with tails after
# the unscanned mass, as cli templates with the environment that fills them
# in attribute "envir". A flagged mode adds nlminb()'s message and a hint.
# `advice` words them for the caller (see advice_entries). The caller raises
# the warning, with any checks of its own and a footer.
diagnostic_problems <- function(dg, gaussian, has_grad = TRUE, advice) {
  check_advice(advice)
  flags <- check_flags(dg, gaussian)
  env <- new.env(parent = baseenv())
  out <- character()
  warned <- c(
    "grad_gap",
    "mode",
    "vb_step",
    "nmad",
    "unscanned_mass",
    "tails",
    "vb",
    "hess_cond"
  )
  for (check in warned[flags[warned]]) {
    out <- c(out, x = problem_text(check, dg, env, advice))
    if (check == "grad_gap") {
      out <- c(
        out,
        i = paste0(
          "A wrong gradient gives a wrong log marginal likelihood, and can ",
          "move the mode and the covariance."
        )
      )
    }
    if (check == "vb_step") {
      # Without a gradient the likely cause is the rule's error on a
      # posterior far from Gaussian, which a gradient removes, not the
      # solver. Then what the unsettled VB mean moves: not skew-normal
      # marginals, which are scanned around the mode, but Gaussian ones,
      # which are centred on it.
      if (!has_grad && nzchar(advice$no_grad)) {
        out <- c(
          out,
          i = paste0(
            "Without a gradient the VB mean is approximate on a posterior ",
            "this far from Gaussian; ",
            advice$no_grad,
            "."
          )
        )
      }
      out <- c(
        out,
        i = if (gaussian) {
          paste0(
            "The Gaussian marginals are centred on the VB mean; ",
            advice$vb_none,
            " centres them at the mode."
          )
        } else {
          paste0(
            "The skew-normal marginals do not depend on it; ",
            advice$vb_none,
            " leaves it out."
          )
        }
      )
    }
    if (check == "mode") {
      env$mode_message <- dg$message
      out <- c(
        out,
        i = "{.fn nlminb} says: {.val {mode_message}}.",
        i = if (isTRUE(grepl("limit", dg$message))) {
          advice$mode_limit
        } else {
          advice$mode_retry
        }
      )
    }
  }
  if (no_mode(dg)) {
    out <- c(
      out,
      i = paste0(
        "The mode may be far from the posterior mass, or the log-posterior ",
        "may have no maximum. ",
        advice$funnel
      )
    )
  }
  attr(out, "envir") <- env
  out
}

# TRUE when the mode search ran off or landed far from the posterior mass,
# past no_mode_levels (lab/DESIGN.md, "No mode to find")
no_mode <- function(dg) {
  glob <- dg$global
  isTRUE(glob[["mode_shift_max"]] > no_mode_levels$mode_shift) ||
    isTRUE(glob[["unscanned_mass_max"]] > no_mode_levels$unscanned_mass)
}
