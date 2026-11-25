# The stages of a fit, which inla_vanilla() and INLAvaan both run:
# vanilla_core(), the wrappers of the user's functions, and the checks of them
# at the start.

# The stages of a fit, as inla_vanilla() and INLAvaan run them: the mode and
# the Laplace approximation, the VB correction, the marginals, and the
# diagnostics of each stage. `lp`, `gr` and `hess` are the log-posterior, its
# gradient and its Hessian as functions of the parameter vector alone (any
# data already bound), with `gr` and `hess` NULL when there are none, and
# `init` the named start. Where `lp` fails (NA, NaN, -Inf, or a value at or
# below -1e39), `gr` must return a value that is not finite: the VB
# correction and the ridge bend call `gr` alone at some points, and take a
# value that is not finite as the sign of a failed point. The other arguments
# are those of inla_vanilla(), and two that only INLAvaan passes, as it knows
# its models:
# * `hess_mode`, a function that gives the Hessian of `lp` at the mode, as
#   `hess` does, but is called there alone, once or, after a restart of the
#   mode search, twice: never by nlminb(), the VB correction or the scans. It
#   replaces the differences of `gr` at the mode, and is checked against them
#   and for symmetry, as `hess` is. It needs `gr`.
# * `gaussian_marginals`, the indices (or names) of parameters whose marginal
#   may be taken as Gaussian: the log-posterior is exactly quadratic along
#   each of their axes with the other parameters fixed, and those axes are
#   nearly separate from the non-Gaussian parameters (saturated intercepts
#   qualify, random effects kept in the parameters do not, because of the
#   funnel with their variance). Their VB shift is held at zero, and they get
#   no scan: their marginal is N(mode_j, Sigma_jj), with NA for the scan
#   diagnostics.
#
# It prints the stage lines (none with `silent = TRUE`) and raises the
# warnings of the stages, but installs no condition handler and raises no
# diagnostics warning: the caller records and muffles warnings as it wants, and
# words the diagnostics warning from diagnostic_problems(). It returns a plain
# list: the mode, the VB mean, the covariance, the marginals with their scan
# diagnostics (`sn`, nine columns), the log marginal likelihood and its
# Laplace part, the log-posterior at the mode, nlminb()'s result, the
# diagnostics of each stage, the scans, the timing and the calls, and the
# settings used.
vanilla_core <- function(
  lp,
  gr = NULL,
  init,
  hess = NULL,
  hess_mode = NULL,
  gaussian_marginals = NULL,
  vb_correction = c("onestep", "full", "none"),
  marginals = c("skewnorm", "gaussian"),
  marginal_scan = c("linear", "exact", "laplace", "plain"),
  tk_fallback = FALSE,
  cores = NULL,
  control = list(),
  verbose = FALSE,
  silent = FALSE
) {
  t_start <- elapsed()
  check_function(lp, "lp")
  if (!is.null(gr)) {
    check_function(gr, "gr")
  }
  if (!is.null(hess)) {
    check_function(hess, "hess")
    if (is.null(gr)) {
      abort("{.arg hess} needs {.arg gr}.")
    }
  }
  if (!is.null(hess_mode)) {
    check_function(hess_mode, "hess_mode")
    if (is.null(gr)) {
      abort("{.arg hess_mode} needs {.arg gr}.")
    }
  }
  if (!is.numeric(init) || length(init) == 0L || !is_named(init)) {
    abort("{.arg init} must be a named numeric vector.")
  }
  if (anyDuplicated(names(init))) {
    abort("{.arg init} must have unique names.")
  }
  gm <- check_gaussian_marginals(gaussian_marginals, names(init))
  vb_correction <- check_vb(vb_correction)
  marginals <- check_marginals(marginals)
  marginal_scan <- check_choice(
    marginal_scan,
    c("linear", "exact", "laplace", "plain"),
    "marginal_scan"
  )
  check_flag(tk_fallback, "tk_fallback")
  cores <- check_cores(cores)
  check_flag(verbose, "verbose")
  check_flag(silent, "silent")
  if (verbose && silent) {
    abort("{.arg verbose} and {.arg silent} cannot both be {.code TRUE}.")
  }
  ctrl <- check_control(control)
  prog <- progress_new(show = !silent, detail = verbose)
  # The user's functions, which the stages call through the wrappers of
  # wrap_fns(), named `lp`, `gr` and `he` below
  logpost_x <- lp
  grad_x <- gr
  hess_x <- hess
  hess_mode_x <- hess_mode

  m <- length(init)
  parnames <- names(init)
  has_grad <- !is.null(grad_x)
  has_hess <- !is.null(hess_x)

  # The engine calls the wrappers of wrap_fns(), and the progress bar ticks
  # from them. The spinner counts the stage's logpost, grad and hess calls
  # together, from `stage_calls()`, so stages driven by grad tick too.
  n_base <- 0L
  stage_calls <- function() {
    n_base <<- st$n_lp + st$n_gr + st$n_he
  }
  tick <- function() {
    n <- st$n_lp + st$n_gr + st$n_he
    if (prog$calls && n %% prog$every == 0L) {
      progress_tick(prog, set = n - n_base)
    }
  }
  fns <- wrap_fns(
    logpost_x,
    grad_x,
    hess_x,
    init,
    on_call = tick,
    hess_mode_x = hess_mode_x
  )
  lp <- fns$lp
  gr <- fns$gr
  he <- fns$he
  he_mode <- fns$he_mode
  st <- fns$state
  # The calls since the last stage, for verbose output
  calls_seen <- c(0L, 0L, 0L)
  calls_since <- function() {
    now <- c(st$n_lp, st$n_gr, st$n_he)
    out <- now - calls_seen
    calls_seen <<- now
    out
  }
  # An error inside logpost, grad or hess stops the fit; say where it happened
  # and at which parameter value.
  with_context <- function(stage, expr) {
    tryCatch(expr, error = function(e) {
      if (inherits(e, "inlavanilla_error")) {
        stop(e)
      }
      at <- paste(parnames, "=", signif(st$last_x, 4), collapse = ", ")
      abort(
        c(
          "The fit stopped with an error while {stage}.",
          "i" = "The last parameter value tried was {at}.",
          "i" = "If {.arg logpost} cannot be evaluated somewhere, return
                 {.code -Inf} there instead of failing."
        ),
        parent = e,
        call = quote(inla_vanilla())
      )
    })
  }

  # One call of each function, to fail at once with a clear message. Anything
  # more waits for the mode (grad_gap()), where it costs no calls without
  # `hess`, and the step search of fd_scales() with it.
  t_first <- elapsed()
  lp_init <- with_context(
    "evaluating logpost at init",
    check_logpost_at_init(logpost_x, init)
  )
  # A slow function gets its progress line on time, and an update is nothing
  # beside the call
  if (elapsed() - t_first >= progress_slow_s) {
    prog$every <- 1L
  }
  if (has_grad) {
    with_context(
      "evaluating grad at init",
      check_grad_at_init(grad_x, init)
    )
  }
  if (has_hess) {
    with_context(
      "evaluating hess at init",
      check_hess_at_init(hess_x, init)
    )
  }
  if (prog$detail) {
    report_start(prog, init, lp_init, has_grad, has_hess, !is.null(hess_mode))
  }

  # 1a. Joint Gaussian -------------------------------------------------------
  # Each segment of fit$timing runs from its start to the start of the next,
  # so the segments add up to the total. The progress lines time the work of
  # each stage alone, from `t0`.
  t0 <- elapsed()
  t_starts <- c(init = t_start, laplace = t0)
  stage_calls()
  progress_start(
    prog,
    paste0("Finding the posterior mode, ", pb_calls),
    waited = t0 - t_start
  )
  lap <- with_context(
    "finding the posterior mode",
    laplace_fit(lp, gr, init, ctrl, he, he_mode)
  )
  if (lap$asymmetry > 1e-3 && lap$hessian$by == "grad") {
    warn(c(
      "{.arg grad} does not look like the gradient of {.arg logpost}.",
      "x" = "Its Jacobian at the mode is not symmetric (relative asymmetry
             {signif(lap$asymmetry, 2)}), as the Hessian of a function must
             be.",
      "i" = "A wrong gradient gives a wrong mode and wrong marginals."
    ))
  }
  # A supplied Hessian, through `hess` or `hess_mode`, is checked for
  # symmetry and against differences of the gradient
  supplied <- c(
    hess = "{.arg hess} does not look like the Hessian of {.arg logpost}.",
    hess_mode = "The Hessian at the mode does not look like the Hessian of the
                 log-posterior."
  )[lap$hessian$by]
  if (lap$asymmetry > 1e-3 && !is.na(supplied)) {
    warn(c(
      supplied,
      "x" = "It is not symmetric at the mode (relative asymmetry
             {signif(lap$asymmetry, 2)}), as the Hessian of a function must
             be.",
      "i" = "The fit used its symmetric part. A wrong Hessian gives a wrong
             covariance and wrong marginals."
    ))
  }
  if (!is.na(supplied) && !isTRUE(lap$hess_error <= 1e-3)) {
    warn(c(
      supplied,
      "x" = "At the mode it differs from central differences of the gradient
             by {signif(lap$hess_error, 2)}, in units where the Hessian is
             the identity.",
      "i" = "A wrong Hessian gives a wrong covariance and wrong marginals."
    ))
  }
  # INLAvaan's Newton step at the mode, in posterior SDs, decides whether the
  # mode is marked here and reported in the fit-time warning, not nlminb()'s
  # return code: without a gradient nlminb() can report "false convergence"
  # at an accurate mode, and it can report success short of one.
  mode_shift_max <- max(lap$mode_shift)
  mode_ok <- is.finite(mode_shift_max) &&
    mode_shift_max <= warn_levels$mode_shift
  t_laplace <- elapsed() - t0
  progress_done(
    prog,
    if (mode_ok) {
      paste0(
        "Found the posterior mode in ",
        hl(lap$opt$iterations),
        " iteration",
        if (lap$opt$iterations == 1L) "" else "s"
      )
    } else {
      paste0(
        "Stopped near the posterior mode (Newton step ",
        hl(with_sd(mode_shift_max)),
        ")"
      )
    },
    t_laplace,
    ok = mode_ok
  )
  if (prog$detail) {
    report_mode(prog, lap, calls_since())
  }

  sds <- sqrt(diag(lap$Sigma))

  # 1b. VB mean correction ---------------------------------------------------
  # With the user's gradient when there is one; without, from log-posterior
  # values alone (see vb_correct())
  t0 <- elapsed()
  t_starts[["vb"]] <- t0
  vbc <- NULL
  use_vb <- vb_correction != "none"
  if (use_vb) {
    stage_calls()
    progress_start(
      prog,
      paste0("Applying the VB mean correction, ", pb_calls)
    )
    vbc <- with_context(
      "applying the VB mean correction",
      vb_correct(
        lp,
        gr,
        lap$mode,
        lap$Sigma,
        lap$R,
        lap$A,
        ctrl,
        method = vb_correction,
        budget = st$n_lp + st$n_gr + st$n_he - lap$gap_calls,
        fixed = gm
      )
    )
    # The fallback is a route of the solver, reported but not marked, as in
    # INLAvaan. A correction that did not settle is marked.
    vb_max <- max(abs(vbc$shift) / sds)
    gain_ok <- !isTRUE(below_level(vbc$gain_ratio, warn_levels$vb_gain))
    settled <- !any(
      check_ran(vbc$step_sigma) &
        off_level(vbc$step_sigma, warn_levels$vb_step)
    )
    progress_done(
      prog,
      paste0(
        "VB mean correction, max shift ",
        hl(paste0(format(signif(vb_max, 2)), sd_unit())),
        if (vbc$fallback) " (fallback optimiser)" else "",
        if (!settled) ", not settled" else "",
        if (!gain_ok) ", gain below its promise" else ""
      ),
      elapsed() - t0,
      ok = settled &&
        gain_ok &&
        is.finite(vb_max) &&
        vb_max <= warn_levels$vb_shift
    )
    if (prog$detail) {
      report_vb(prog, vbc, sds, lap$logml, calls_since())
    }
  } else {
    progress_info(prog, "No VB correction (vb_correction = \"none\")")
  }
  shift <- if (use_vb) vbc$shift else stats::setNames(numeric(m), parnames)
  mean_vb <- lap$mode + shift

  # 2. Skew-normal marginals -------------------------------------------------
  # With Gaussian marginals there are no scans: each marginal is the
  # Gaussian's own, N(mean_vb_j, Sigma_jj), a skew-normal with shape 0.
  t0 <- elapsed()
  t_starts[["marginals"]] <- t0
  if (marginals == "skewnorm") {
    start_marginals <- function(step) {
      progress_close(prog)
      if (step == "shared") {
        stage_calls()
        progress_start(
          prog,
          paste0("Computing the volume corrections, ", pb_calls)
        )
      } else {
        progress_start(
          prog,
          paste0(
            "Fitting ",
            pb_item,
            " skew-normal marginal",
            if (m == 1L) "" else "s"
          ),
          total = m
        )
      }
    }
    mar <- with_context(
      "fitting the skew-normal marginals",
      fit_marginals(
        lp,
        gr,
        lap,
        ctrl,
        tick = function() progress_tick(prog),
        start = start_marginals,
        he = he,
        marginal_scan = marginal_scan,
        tk_fallback = tk_fallback,
        par = list(
          logpost = logpost_x,
          grad = grad_x,
          hess = hess_x,
          init = init,
          cores = cores,
          # Seconds per call so far, to judge how long the scans will take.
          # The clock has a resolution of a millisecond
          call_s = max(elapsed() - t_start, 1e-3) /
            (st$n_lp + st$n_gr + st$n_he),
          add_calls = function(n) {
            st$n_lp <- st$n_lp + n[[1L]]
            st$n_gr <- st$n_gr + n[[2L]]
            st$n_he <- st$n_he + n[[3L]]
          },
          # A message cannot share the console with a running progress line
          note = function(text) {
            progress_close(prog)
            progress_note(prog, text)
          }
        ),
        gaussian = gm
      )
    )
    workers <- mar$workers
    sn <- cbind(
      mar$sn,
      unscanned_mass = replace(
        unscanned_mass(mar$sn, lap$mode, sds),
        gm,
        NA_real_
      )
    )
    # The scans, kept for plot(type = "sn_fit"): no further calls
    scan <- list(
      z = mar$z,
      logpost = mar$logpost,
      width = mar$width,
      logC = sn[, "logC"]
    )
    method <- mar$method
    progress_done(
      prog,
      paste0(
        "Fitted ",
        hl(m),
        " skew-normal marginal",
        if (m == 1L) "" else "s"
      ),
      elapsed() - t0
    )
    if (prog$detail) {
      report_marginals(prog, sn, calls_since(), method, marginal_scan)
      if (workers > 1L) {
        progress_info(prog, paste0("The scans ran on ", workers, " workers"))
      }
    }
  } else {
    workers <- 1L
    na <- rep(NA_real_, m)
    sn <- cbind(
      xi = mean_vb,
      omega = sds,
      alpha = 0,
      gamma1 = na,
      nmad = na,
      tails = na,
      ridge_bend = na,
      logC = na,
      unscanned_mass = na
    )
    rownames(sn) <- parnames
    scan <- NULL
    method <- stats::setNames(rep(NA_character_, m), parnames)
    progress_info(
      prog,
      "Gaussian marginals (marginals = \"gaussian\"): no scans"
    )
  }
  t_starts[["diagnostics"]] <- elapsed()

  logml <- lap$logml + if (use_vb) vbc$kld_global else 0
  mom <- sn_moments(sn[, "xi"], sn[, "omega"], sn[, "alpha"])

  # The diagnostics, in the names and order of INLAvaan's diagnostics() where
  # it has the quantity. Checks that did not run are NA: the VB columns with
  # vb_correction = "none", the scan columns with Gaussian marginals.
  skew <- marginals == "skewnorm"
  na <- rep(NA_real_, m)
  diagnostics <- list(
    optim = list(
      convergence = lap$opt$convergence,
      message = lap$opt$message,
      iterations = lap$opt$iterations,
      # INLAvaan's sign: the gradient of nlminb()'s objective, -logpost
      grad = -lap$grad,
      grad_mode = lap$grad_mode,
      hess_cond = lap$hess_cond,
      hess_cond_cor = lap$hess_cond_cor
    ),
    vb = if (use_vb) {
      list(
        iterations = vbc$iterations,
        fallback = vbc$fallback,
        one_pass = vbc$one_pass,
        gain_ratio = vbc$gain_ratio,
        kld_global = vbc$kld_global
      )
    },
    marginals = data.frame(
      mode_shift_sigma = lap$mode_shift,
      kld = if (use_vb) 0.5 * (shift / sds)^2 else na,
      vb_shift = if (use_vb) shift else na,
      vb_shift_sigma = if (use_vb) shift / sds else na,
      nmad = sn[, "nmad"],
      alpha = if (skew) replace(sn[, "alpha"], gm, NA_real_) else na,
      unscanned_mass = sn[, "unscanned_mass"],
      vb_step_sigma = if (use_vb) vbc$step_sigma else na,
      grad_gap = lap$grad_gap,
      mean_gap = if (use_vb && skew) {
        replace((mom$mean - mean_vb) / sds, gm, NA_real_)
      } else {
        na
      },
      gamma1 = sn[, "gamma1"],
      tails = as.logical(sn[, "tails"]),
      ridge_bend = sn[, "ridge_bend"],
      marginal_scan = unname(method),
      row.names = parnames,
      stringsAsFactors = FALSE
    )
  )

  t_end <- elapsed()
  list(
    mode = lap$mode,
    mean_vb = mean_vb,
    cov = lap$Sigma,
    sn = sn,
    logml = logml,
    logml_laplace = lap$logml,
    lp_max = lap$lp_max,
    opt = lap$opt,
    restart = lap$restart,
    diagnostics = diagnostics,
    scan = scan,
    timing = c(
      stats::setNames(diff(c(t_starts, t_end)), names(t_starts)),
      total = t_end - t_start
    ),
    counts = c(logpost = st$n_lp, grad = st$n_gr, hess = st$n_he),
    vb_correction = vb_correction,
    marginals = marginals,
    marginal_scan = if (skew) marginal_scan else NA_character_,
    tk_fallback = tk_fallback,
    fallback = if (skew) {
      parnames[!is.na(method) & method != marginal_scan]
    } else {
      character()
    },
    cores = cores,
    workers = workers,
    has_grad = has_grad
  )
}

# The wrappers the stages call in place of the user's functions, which have
# `...` already bound. They name the parameters, and count the calls and record
# the last parameter value in the environment `state`. `on_call` runs at each
# call, for the progress bar. `gr` and `he` are NULL when the user gave none,
# and `he_mode`, from `hess_mode_x`, a Hessian for the mode alone, too.
wrap_fns <- function(
  logpost_x,
  grad_x,
  hess_x,
  init,
  on_call = function() NULL,
  hess_mode_x = NULL
) {
  m <- length(init)
  parnames <- names(init)
  state <- new.env(parent = emptyenv())
  state$n_lp <- 0L
  state$n_gr <- 0L
  state$n_he <- 0L
  state$last_x <- init
  lp <- function(x) {
    names(x) <- parnames
    state$last_x <- x
    state$n_lp <- state$n_lp + 1L
    on_call()
    # [[1L]] also accepts a 1 x 1 matrix, such as a quadratic form
    v <- logpost_x(x)[[1L]]
    failed_lp(v)
  }
  gr <- if (is.null(grad_x)) {
    NULL
  } else {
    function(x) {
      names(x) <- parnames
      state$last_x <- x
      state$n_gr <- state$n_gr + 1L
      on_call()
      as.numeric(grad_x(x))
    }
  }
  # A plain m x m matrix, whatever the user's function returns (a sparse
  # matrix from the Matrix package, or a number when m = 1), and an error
  # naming `arg` for any other shape. A Hessian for the mode alone counts as a
  # call of `hess`.
  wrap_hess <- function(hess_x, arg) {
    if (is.null(hess_x)) {
      return(NULL)
    }
    function(x) {
      names(x) <- parnames
      state$last_x <- x
      state$n_he <- state$n_he + 1L
      on_call()
      h <- hess_x(x)
      H <- tryCatch(as.matrix(h), error = function(e) NULL)
      if (!is.numeric(H) || !identical(dim(H), c(m, m))) {
        abort_hess_shape(h, m, arg, paste0(arg, "()"))
      }
      matrix(as.numeric(H), m, m)
    }
  }
  list(
    lp = lp,
    gr = gr,
    he = wrap_hess(hess_x, "hess"),
    he_mode = wrap_hess(hess_mode_x, "hess_mode"),
    state = state
  )
}

# The error for a Hessian that is not a numeric m x m matrix: `arg` names the
# argument, and `call` the call that returned `h`.
abort_hess_shape <- function(h, m, arg, call) {
  shape <- if (is.null(dim(h))) {
    paste("of length", length(h))
  } else {
    paste("with dimensions", paste(dim(h), collapse = " x "))
  }
  abort(c(
    "{.arg {arg}} must return a numeric {m} x {m} matrix.",
    "x" = "{.code {call}} returned {.obj_type_friendly {h}} {shape}."
  ))
}

# The log-posterior as the stages read it: -Inf where it failed. NA and NaN
# count as a failure, and so does any value at or below -1e39, such as the
# -1e40 that lavaan's likelihoods, and INLAvaan's, return where a model cannot
# be evaluated. As a finite number it would pass for a very low density: the
# VB fallback would take a failed node for one it can average over, and a
# failed start would not stop the fit.
failed_lp <- function(v) {
  if (is.na(v) || v <= -1e39) -Inf else v
}

check_logpost_at_init <- function(logpost, init) {
  v <- logpost(init)
  if (!is.numeric(v) || length(v) != 1L) {
    abort(
      c(
        "{.arg logpost} must return a single number.",
        "x" = "{.code logpost(init)} returned {.obj_type_friendly {v}} of length {length(v)}."
      )
    )
  }
  if (!is.finite(failed_lp(v))) {
    abort(
      c(
        "{.arg logpost} is not finite at {.arg init}.",
        "x" = "{.code logpost(init)} returned {.val {v}}.",
        "i" = "Start inside the support of the posterior, and check that every
             parameter is on an unconstrained scale."
      )
    )
  }
  invisible(v)
}

# A Hessian of the right shape, finite at init. Whether it is the Hessian of
# `logpost` is checked at the mode, against the gradient (hess_discrepancy()).
check_hess_at_init <- function(hess, init) {
  m <- length(init)
  h <- hess(init)
  H <- tryCatch(as.matrix(h), error = function(e) NULL)
  if (!is.numeric(H) || !identical(dim(H), c(m, m))) {
    abort_hess_shape(h, m, "hess", "hess(init)")
  }
  if (!all(is.finite(H))) {
    abort("{.arg hess} is not finite at {.arg init}.")
  }
  invisible(H)
}

# A gradient of the right length, finite at init. Whether it is the gradient of
# `logpost` is checked at the mode (grad_gap()).
check_grad_at_init <- function(grad, init) {
  g <- grad(init)
  if (!is.numeric(g) || length(g) != length(init)) {
    abort(
      c(
        "{.arg grad} must return a numeric vector of length {length(init)}.",
        "x" = "{.code grad(init)} returned {.obj_type_friendly {g}} of length {length(g)}."
      )
    )
  }
  if (!all(is.finite(g))) {
    abort(
      "{.arg grad} is not finite at {.arg init}.",
    )
  }
  invisible(g)
}
