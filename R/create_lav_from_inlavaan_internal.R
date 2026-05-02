create_lav_from_inlavaan_internal <- function(fit0, fit_inlv) {
  resolve_optim_converged <- function(opt, method) {
    if (!is.null(opt$converged) && length(opt$converged) == 1L &&
        !is.na(opt$converged)) {
      return(isTRUE(opt$converged))
    }
    if (method == "ucminf") {
      return(isTRUE(opt$convergence == 1L))
    }
    if (!is.null(opt$convergence) && length(opt$convergence) == 1L &&
        !is.na(opt$convergence)) {
      return(isTRUE(opt$convergence == 0L))
    }
    FALSE
  }
  resolve_optim_iterations <- function(opt, method) {
    if (!is.null(opt$iterations) && length(opt$iterations) == 1L &&
        is.finite(opt$iterations)) {
      return(as.integer(opt$iterations))
    }
    if (method == "optim") {
      return(as.integer(opt$counts["function"]))
    }
    if (method == "ucminf") {
      return(as.integer(opt$info["neval"]))
    }
    as.integer(opt$iterations %||% NA_integer_)
  }
  resolve_optim_objective <- function(opt) {
    if (!is.null(opt$objective) && length(opt$objective) == 1L &&
        is.finite(opt$objective)) {
      return(opt$objective)
    }
    if (!is.null(opt$value) && length(opt$value) == 1L &&
        is.finite(opt$value)) {
      return(opt$value)
    }
    NA_real_
  }

  if (is.null(fit0)) {
    fit0 <- new("lavaan")
    fit0@call <- quote(INLAvaan::inlavaan())
    fit0@timing <- list()
    fit0@Options <- fit_inlv$lavoptions
    fit0@ParTable <- fit_inlv$lavpartable
    fit0@pta <- list()
    fit0@Data <- fit_inlv$lavdata
    fit0@SampleStats <- fit_inlv$lavsamplestats
    fit0@Model <- fit_inlv$lavmodel
    fit0@Cache <- fit_inlv$lavcache %||% list()
    fit0@Fit <- new("Fit")
    fit0@boot <- list()
    fit0@optim <- list()
    fit0@loglik <- list()
    fit0@implied <- list()
    fit0@vcov <- list()
    fit0@test <- list()
    fit0@h1 <- list()
    fit0@baseline <- list()
    fit0@internal <- list()
    fit0@external <- list()
  }

  ## ----- Update Model and implied slots --------------------------------------
  x <- fit_inlv$coefficients
  fit0@Model <- lavaan::lav_model_set_parameters(fit0@Model, x)
  # fit0@Model@estimator <- "BAYES"
  fit0@implied <- lavaan::lav_model_implied(fit0@Model)

  ## ----- Update ParTable slot ------------------------------------------------

  # # Find Theta matrix (residuals) and Sigmay (implied covariance matrix)
  # thetadiag <- diag(fit0@Model@GLIST$theta)
  # Sigmay <- fit0@implied$cov[[1]]  # FIXME: Group 1 only
  SD <- fit_inlv$summary[, "SD"] # free only

  pt <- fit_inlv$partable
  fixed_idx <- which(pt$free == 0L & !(pt$op %in% c(":=", "~*~")))
  if (length(fixed_idx) > 0L) {
    fixed_vals <- pt$ustart[fixed_idx]
    use_start <- is.na(fixed_vals)
    fixed_vals[use_start] <- pt$start[fixed_idx][use_start]
    pt$est[fixed_idx] <- fixed_vals
    pt$se[fixed_idx] <- 0
  }
  pt$est[pt$free > 0] <- x[pt$free[pt$free > 0]]
  pt$se[pt$free > 0] <- SD[pt$free[pt$free > 0]]

  pt$par <- pt$parstart
  pt$par[pt$free > 0] <- fit_inlv$theta_star[pt$free[pt$free > 0]]

  # Update defined parameters
  if (any(pt$op == ":=")) {
    pt_def_rows <- which(pt$op == ":=")
    def_names <- pt$names[pt_def_rows]

    pt$est[pt_def_rows] <- fit_inlv$summary[def_names, "Mean"]
    pt$se[pt_def_rows] <- fit_inlv$summary[def_names, "SD"]
  }

  # Put the delta scales back (theta parameterisation)
  if (any(pt$op == "~*~")) {
    delta_rows <- which(pt$op == "~*~")
    delta_names <- pt$names[delta_rows]

    pt$est[delta_rows] <- fit_inlv$summary[delta_names, "Mean"]
    pt$se[delta_rows] <- fit_inlv$summary[delta_names, "SD"]
  }

  # Put the diag theta values in the pt (ONLY IF ORDINAL)
  # ov_names <- fit0@Data@ov.names[[1]]  # FIXME: Group 1 only
  # pt$est[pt$lhs %in% ov_names &
  #          pt$rhs %in% ov_names &
  #          pt$lhs == pt$rhs &
  #          pt$op == "~~"] <- thetadiag

  # Manually change the slack column
  # slack_values <- as.vector(fit0@Model@con.jac %*% x - fit0@Model@ceq.rhs)
  # pt$est[pt$op == "=="] <- slack_values

  # Flatten functions list
  pt$g <- sapply(pt$g, as_fun_string)
  pt$g_prime <- sapply(pt$g_prime, as_fun_string)
  pt$ginv <- sapply(pt$ginv, as_fun_string)
  pt$ginv_prime <- sapply(pt$ginv_prime, as_fun_string)
  pt$ginv_prime2 <- sapply(pt$ginv_prime2, as_fun_string)

  fit0@ParTable <- pt

  ## ----- Update Options slot -------------------------------------------------
  optim_method <- fit_inlv$optim_method
  if (optim_method == "optim") {
    optim_method <- "BFGS"
  }
  fit0@Options$optim.method <- fit_inlv$optim_method
  fit0@Options$do.fit <- TRUE
  fit0@Options$estimator <- "Bayes"
  fit0@Options$se <- "standard"

  ## ----- Update Fit slot -----------------------------------------------------
  fit0@Fit@npar <- as.integer(length(x))
  fit0@Fit@x <- x
  # fit0@Fit@TH[[1]] <- tau  # FIXME: Group 1
  fit0@Fit@est <- pt$est
  fit0@Fit@se <- pt$se
  fit0@Fit@start <- pt$start

  if (fit_inlv$optim_method == "nlminb") {
    fit0@Fit@iterations <- resolve_optim_iterations(fit_inlv$opt, "nlminb")
    fit0@Fit@converged <- resolve_optim_converged(fit_inlv$opt, "nlminb")
    fit0@Fit@fx <- resolve_optim_objective(fit_inlv$opt)
  } else if (fit_inlv$optim_method == "optim") {
    fit0@Fit@iterations <- resolve_optim_iterations(fit_inlv$opt, "optim")
    fit0@Fit@converged <- resolve_optim_converged(fit_inlv$opt, "optim")
    fit0@Fit@fx <- resolve_optim_objective(fit_inlv$opt)
  } else if (fit_inlv$optim_method == "ucminf") {
    fit0@Fit@iterations <- resolve_optim_iterations(fit_inlv$opt, "ucminf")
    fit0@Fit@converged <- resolve_optim_converged(fit_inlv$opt, "ucminf")
    fit0@Fit@fx <- resolve_optim_objective(fit_inlv$opt)
  }
  fit0@Fit@Sigma.hat <- fit0@implied

  fit0@Fit@test <- list(
    mloglik = list(
      test = "mloglik",
      stat = fit_inlv$mloglik,
      stat.group = fit_inlv$mloglik, # FIXME
      df = NA,
      refdistr = NA,
      pvalue = NA
    )
  )
  if (!is.null(fit_inlv$ppp)) {
    fit0@Fit@test <- c(
      fit0@Fit@test,
      list(
        ppp = list(
          test = "ppp",
          stat = fit_inlv$ppp,
          stat.group = fit_inlv$ppp, # FIXME
          df = NA,
          refdistr = NA,
          pvalue = NA
        )
      )
    )
  }

  ## ----- Update optim slot ---------------------------------------------------
  fit0@optim$x <- fit_inlv$opt$par
  fit0@optim$dx <- fit_inlv$opt$dx
  fit0@optim$npar <- length(x)

  fit0@optim$fx <- fit0@Fit@fx
  fit0@optim$fx.group <- fit0@Fit@fx.group # FIXME: What's this?
  fit0@optim$iterations <- fit0@Fit@iterations
  fit0@optim$converged <- fit0@Fit@converged

  ## ----- Update loglik slot --------------------------------------------------
  # if (fit_inlv$method == "ucminf") {
  #   fit0@loglik$loglik <- fit_inlv$numFit$value
  # } else if (fit_inlv$method == "SA") {
  #   fit0@loglik$loglik <- fit_inlv$stoFit@nll
  # }
  # fit0@loglik$estimator <- "ML"  # FIXME: to turn off warning for now
  # if (!is.null(vars)) {
  #   fit0@loglik$AIC <- get_AIC(NLL = fit0@loglik$loglik, INVH = vars$invH,
  #                              J = vars$J)
  #   fit0@loglik$BIC <- get_BIC(NLL = fit0@loglik$loglik, INVH = vars$invH,
  #                              J = vars$J, N = n)
  # }
  fit0@loglik <- list()

  ## ----- Update vcov slot ----------------------------------------------------
  fit0@vcov <- list(
    vcov = fit_inlv$vcov_x,          # Sample-based vcov (lavaan parameterisation)
    vcov_theta = fit_inlv$Sigma_theta # Laplace vcov (theta parameterisation)
  )

  ## ----- Update test slot ----------------------------------------------------
  fit0@test <- fit0@Fit@test

  ## ----- Change baseline slot ------------------------------------------------
  fit0@baseline <- list()

  ## ----- Change version slot -------------------------------------------------
  fit0@version <- as.character(packageVersion("INLAvaan"))

  ## ----- Change timing slot --------------------------------------------------
  t0 <- fit0@timing
  t1 <- fit_inlv$timing
  nms <- union(names(t0), names(t1))
  fit0@timing <- lapply(nms, function(n) {
    (if (n %in% names(t0)) t0[[n]] else 0) +
      (if (n %in% names(t1)) t1[[n]] else 0)
  })
  names(fit0@timing) <- nms
  fit0@timing$total <- NULL
  fit0@timing$total <- sum(unlist(fit0@timing))

  ## ----- Return --------------------------------------------------------------
  fit0@external <- list(
    inlavaan_internal = fit_inlv #[!grepl("lav", names(fit_inlv))]
  )
  fit0
}
