create_lav_from_inlavaan_internal <- function(fit0, fit_inlv) {

  ## ----- Change Model and implied slots --------------------------------------
  x <- fit_inlv$theta_star_trans
  fit0@Model <- lavaan::lav_model_set_parameters(fit0@Model, x)
  fit0@implied <- lavaan::lav_model_implied(fit0@Model)

  ## ----- Change ParTable slot ------------------------------------------------

  # # Find Theta matrix (residuals) and Sigmay (implied covariance matrix)
  # thetadiag <- diag(fit0@Model@GLIST$theta)
  # Sigmay <- fit0@implied$cov[[1]]  # FIXME: Group 1 only

  # Change parameter table and pta slots
  pt <- fit_inlv$partable
  pt$est[pt$free > 0] <- x[pt$free[pt$free > 0]]

  SE <- fit_inlv$summary[, "SD"]
  pt$se <- 0
  if (length(SE) > 0) pt$se[pt$free > 0] <- SE[pt$free[pt$free > 0]]

  # Put the diag theta values in the pt (ONLY IF ORDINAL)
  # ov_names <- fit0@Data@ov.names[[1]]  # FIXME: Group 1 only
  # pt$est[pt$lhs %in% ov_names &
  #          pt$rhs %in% ov_names &
  #          pt$lhs == pt$rhs &
  #          pt$op == "~~"] <- thetadiag

  # Manually change the slack column
  # slack_values <- as.vector(fit0@Model@con.jac %*% x - fit0@Model@ceq.rhs)
  # pt$est[pt$op == "=="] <- slack_values
  fit0@ParTable <- pt

  ## ----- Change Options slot -------------------------------------------------
  optim_method <- fit_inlv$optim_method
  if (optim_method == "optim") optim_method <- "BFGS"
  fit0@Options$optim.method <- fit_inlv$optim_method
  fit0@Options$do.fit <- TRUE

  ## ----- Change Fit slot -----------------------------------------------------
  fit0@Fit@x <- x
  # fit0@Fit@TH[[1]] <- tau  # FIXME: Group 1
  fit0@Fit@est <- pt$est
  fit0@Fit@se <- pt$se
  fit0@Fit@start <- pt$start

  if (fit_inlv$optim_method == "nlminb") {
    fit0@Fit@iterations <- as.integer(fit_inlv$opt$iterations)
    fit0@Fit@converged  <- fit_inlv$opt$convergence == 0L
    fit0@Fit@fx         <- fit_inlv$opt$objective
  } else if (fit_inlv$optim_method == "optim") {
    fit0@Fit@iterations <- as.integer(fit_inlv$opt$counts["function"])
    fit0@Fit@converged  <- fit_inlv$opt$convergence == 0L
    fit0@Fit@fx         <- fit_inlv$opt$value
  } else if (fit_inlv$optim_method == "ucminf") {
    fit0@Fit@iterations <- as.integer(fit_inlv$opt$info["neval"])
    fit0@Fit@converged  <- fit_inlv$opt$convergence == 1L
    fit0@Fit@fx         <- fit_inlv$opt$value
  }
  fit0@Fit@Sigma.hat <- fit0@implied

  ## ----- Change optim slot ---------------------------------------------------
  fit0@optim$x    <- fit_inlv$opt$par
  fit0@optim$dx   <- fit_inlv$opt$dx
  fit0@optim$npar <- length(x)

  fit0@optim$fx         <- fit0@Fit@fx
  fit0@optim$fx.group   <- fit0@Fit@fx.group  # FIXME: What's this?
  fit0@optim$iterations <- fit0@Fit@iterations
  fit0@optim$converged  <- fit0@Fit@converged

  ## ----- Change loglik slot --------------------------------------------------
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

  ## ----- Change vcov slot ----------------------------------------------------
  fit0@vcov <- list()  # FIXME: Do we need this?

  ## ----- Change test slot ----------------------------------------------------
  fit0@test <- list(
    mloglik = list(
      test = "mloglik",
      stat = fit_inlv$mloglik,
      stat.group = fit_inlv$mloglik,  # FIXME
      df = NA,
      refdistr = NA,
      pvalue = NA
    ),
    ppp = list(
      test = "ppp",
      stat = fit_inlv$ppp,
      stat.group = fit_inlv$ppp,  # FIXME
      df = NA,
      refdistr = NA,
      pvalue = NA
    )
  )

  # Change baseline slot -------------------------------------------------------
  fit0@baseline <- list()

  # Change timing slot ---------------------------------------------------------
  t0 <- fit0@timing
  t1 <- fit_inlv$timing
  nms <- union(names(t0), names(t1))
  fit0@timing <- lapply(nms, function(n) {
    (if (n %in% names(t0)) t0[[n]] else 0) +
      (if (n %in% names(t1)) t1[[n]] else 0)
  })
  names(fit0@timing) <- nms

  ## ----- Return --------------------------------------------------------------
  fit0@external <- list(
    inlavaan_internal = fit_inlv[!grepl("lav", names(fit_inlv))]
  )
  fit0
}

