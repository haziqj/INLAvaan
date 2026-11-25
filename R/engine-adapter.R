# The adapter between inlavaan() and the engine, the files R/vanilla-*.R that
# INLAvaan copies from inlavanilla (see inst/inlavanilla.lock). The engine runs
# the stages of a fit on the theta scale: the mode and the Laplace
# approximation, the VB correction of the mean, and the skew-normal marginals.
# INLAvaan gives it the log-posterior, its gradient and, where INLAvaan knows
# more than the engine, the Hessian at the mode. It builds the fields of the
# internal list from what the engine returns. Only this file calls
# vanilla_core().

## ----- Hooks -----------------------------------------------------------------

# The log-posterior and its gradient for the engine. `lp` is joint_lp() as it
# is, because the engine reads -1e40 as a failed point itself. `gr` returns NA
# where joint_lp() fails: the VB correction and the ridge bend call the
# gradient alone at some points, and see a failed point only through a
# gradient that is not finite, while joint_lp_grad() is finite there. A failed
# point is what the engine's failed_lp() reads as one (NA, NaN, or -1e39 and
# below), so that the gradient fails exactly where the stages see a failed
# log-posterior. A value of +Inf is not a failure to the engine, so its
# gradient is computed. lavaan warns that Sigma.hat is not positive definite at
# a failed point, so that warning is muffled. A one-entry memo of the last
# value of joint_lp() saves the extra call when nlminb() asks for the gradient
# at the point whose objective it has just taken. With `numerical_grad = TRUE`
# there is no `gr`.
engine_fns <- function(joint_lp, joint_lp_grad, numerical_grad = FALSE) {
  memo <- new.env(parent = emptyenv())
  memo$theta <- NULL
  memo$value <- NA_real_
  lp <- function(theta) {
    value <- joint_lp(theta)
    memo$theta <- theta
    memo$value <- value
    value
  }
  if (isTRUE(numerical_grad)) {
    return(list(lp = lp, gr = NULL))
  }
  gr <- function(theta) {
    value <- if (identical(theta, memo$theta)) {
      memo$value
    } else {
      joint_lp(theta)
    }
    if (identical(failed_lp(value), -Inf)) {
      return(rep(NA_real_, length(theta)))
    }
    withCallingHandlers(
      joint_lp_grad(theta),
      warning = function(w) {
        if (grepl("Sigma.hat", conditionMessage(w), fixed = TRUE)) {
          tryInvokeRestart("muffleWarning")
        }
      }
    )
  }
  list(lp = lp, gr = gr)
}

# The Hessian of the log-posterior at the mode, for the engine's `hess_mode`,
# or NULL, and the engine then takes central differences of `gr` itself. With
# `method = "forward"` the columns are forward differences of the gradient
# (fast_jacobian()). On the saturated-means fast path (`fastpath` from
# saturated_mean_idx()) only the columns of the other parameters are
# differenced. The cross block of the intercept rows and those columns is then
# mirrored from the rows that the differenced columns already hold, because it
# is not zero at the mode: the prior pulls the fitted means away from the
# sample means. The intercept block is analytic, -(n Sigma^-1 + P) with Sigma
# the model-implied covariance at `theta` (`implied_cov`) and P the prior
# precision of the intercepts, zero with `add_priors = FALSE`. The matrix is
# returned as differenced, not symmetrised, so that the engine's check of its
# symmetry sees it.
engine_hess_mode <- function(
  joint_lp_grad,
  method,
  fastpath,
  n,
  implied_cov,
  add_priors
) {
  if (is.null(fastpath) && method == "central") {
    return(NULL)
  }
  function(theta) {
    theta <- as.numeric(theta)
    if (is.null(fastpath)) {
      return(fast_jacobian(joint_lp_grad, theta, method = method))
    }
    fp <- fastpath$idx
    cc <- setdiff(seq_along(theta), fp)
    H <- fast_jacobian(joint_lp_grad, theta, cols = cc, method = method)
    H[cc, fp] <- t(H[fp, cc, drop = FALSE])
    prec <- if (isTRUE(add_priors)) fastpath$prec else 0
    sp <- fastpath$sigma_pos
    H[fp, fp] <- -(n *
      chol2inv(chol(implied_cov(theta)))[sp, sp, drop = FALSE] +
      diag(prec, length(fp)))
    H
  }
}

# The log-posterior at the start values must be finite, or the fit cannot
# start. The engine checks this too, but in its own words, which name its
# arguments.
engine_check_start <- function(joint_lp, init) {
  value <- joint_lp(init)
  if (!is.finite(value) || value <= -1e39) {
    cli_abort(c(
      "The log-posterior cannot be evaluated at the starting values.",
      "x" = "The model-implied covariance matrix is not positive definite
             there, or a prior density is zero.",
      "i" = "Check that the model is identified, or give starting values in
             the model syntax, such as {.code start(1)*x1}."
    ))
  }
  invisible(value)
}

## ----- The call --------------------------------------------------------------

# vanilla_core() with the arguments given. The engine raises its errors with
# the call inla_vanilla(), the function of inlavanilla that users of INLAvaan
# never call, so the call is dropped and the error raised again.
engine_call <- function(...) {
  tryCatch(
    vanilla_core(...),
    inlavanilla_error = function(e) {
      e$call <- NULL
      stop(e)
    }
  )
}

# The segments optim and vb of the timing of inlavaan(), from the engine's
# `timing` (`et`) and the wall time since timing$start_time, which then starts
# the next segment. optim takes the engine's start checks and its Laplace
# stage, and whatever wall time the engine did not count. The engine's
# marginals and diagnostics go to INLAvaan's marginals segment. Its total is
# never copied, because the timing slot adds the segments up.
engine_timing <- function(timing, et) {
  now <- proc.time()[3]
  wall <- now - timing$start_time
  timing$optim <- et[["init"]] + et[["laplace"]] + max(wall - et[["total"]], 0)
  timing$vb <- et[["vb"]]
  timing$start_time <- now
  timing
}

## ----- Fields ----------------------------------------------------------------

# The fields of the internal list that come from the engine's result `core`,
# in the layout of INLAvaan 0.3.x: theta_star is the VB mean and
# theta_star_novbc the mode, both unnamed. `loglik_x_adj` is the constant of
# a two-level fit under fixed.x (twolevel_fixedx_loglik_adj()). The engine's
# log marginal likelihood already holds the VB term, so it is not added
# again. The gradient at the mode is the engine's, which has the sign of
# nlminb()'s objective, as opt$dx_analytic had. With no VB correction the VB
# fields are NA, as before. A Gaussian marginal of the fast path has alpha 0
# and NA for logC, nmad and gamma1, since it has no scan. The field `engine`
# keeps the engine's diagnostics, scans, counts and timing, and the release of
# inlavanilla that the engine files come from.
engine_fields <- function(core, loglik_x_adj = 0) {
  m <- length(core$mode)
  parnames <- names(core$mode)
  vb_on <- core$vb_correction != "none"
  shift <- unname(core$mean_vb - core$mode)
  dg <- core$diagnostics
  opt <- core$opt
  opt$dx <- unname(dg$optim$grad)
  opt$dx_analytic <- opt$dx
  approx_data <- NULL
  if (core$marginals == "skewnorm") {
    rows <- lapply(seq_len(m), function(j) core$sn[j, ])
    names(rows) <- parnames
    approx_data <- approx_data_rows(rows)
  }
  list(
    theta_star = unname(core$mean_vb),
    theta_star_novbc = unname(core$mode),
    Omega = core$cov,
    approx_data = approx_data,
    mloglik = unname(core$logml) + loglik_x_adj,
    opt = opt,
    vb = list(
      opt = if (vb_on) {
        list(
          par = shift,
          objective = NA_real_,
          iterations = dg$vb$iterations,
          fallback = dg$vb$fallback
        )
      } else {
        NA
      },
      n_qmc = NA_integer_,
      method = if (vb_on) core$vb_correction else NA_character_,
      correction = if (vb_on) shift else NA,
      mcse = if (vb_on) rep(NA_real_, m) else NA,
      kld = if (vb_on) shift^2 / (2 * diag(core$cov)) else NA,
      kld_global = if (vb_on) dg$vb$kld_global else NA
    ),
    visual_debug = engine_visual_debug(core),
    engine = c(
      core[c(
        "diagnostics",
        "scan",
        "counts",
        "timing",
        "logml_laplace",
        "lp_max",
        "restart",
        "vb_correction",
        "marginals",
        "marginal_scan",
        "fallback",
        "workers"
      )],
      as.list(engine_lock())
    )
  )
}

# The scans in the layout of INLAvaan 0.3.x, for plot(type = "sn_fit"): per
# parameter, the log-posterior along its scan (Original), the same with the
# volume correction (Corrected) and the fitted skew-normal (SN_Fit), on the
# whitened scale, with the fit in the attribute sn_params. NULL for a
# parameter with no scan, and NULL in all with Gaussian marginals. Each curve
# is normalised over its finite points, since a scan can leave a point NA
# where the volume correction fails (marginal_scan = "exact").
engine_visual_debug <- function(core) {
  sc <- core$scan
  if (is.null(sc)) {
    return(NULL)
  }
  sds <- sqrt(diag(core$cov))
  peak <- function(y) max(y[is.finite(y)])
  out <- lapply(seq_along(core$mode), function(j) {
    raw <- sc$logpost[, j]
    if (!any(is.finite(raw))) {
      return(NULL)
    }
    y <- raw + sc$width[, j]
    p <- c(
      xi = (core$sn[j, "xi"] - core$mode[[j]]) / sds[[j]],
      omega = core$sn[j, "omega"] / sds[[j]],
      alpha = core$sn[j, "alpha"],
      logC = core$sn[j, "logC"]
    )
    vd <- data.frame(
      x = sc$z,
      Original = exp(raw - peak(raw)),
      Corrected = exp(y - peak(y)),
      SN_Fit = dsnorm(
        x = sc$z,
        xi = p[["xi"]],
        omega = p[["omega"]],
        alpha = p[["alpha"]],
        logC = p[["logC"]]
      )
    )
    attr(vd, "sn_params") <- p
    vd
  })
  names(out) <- names(core$mode)
  out
}

# The release and the commit of inlavanilla that the engine files come from,
# as inst/inlavanilla.lock records them
engine_lock <- function() {
  path <- system.file("inlavanilla.lock", package = "INLAvaan")
  lines <- if (nzchar(path)) readLines(path, warn = FALSE) else character()
  field <- function(key) {
    hit <- grep(paste0("^", key, ":"), lines, value = TRUE)
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    trimws(sub("^[^:]*:", "", hit[[1L]]))
  }
  c(release = field("Release"), commit = field("Commit"))
}
