create_lav_from_inlavaan_internal <- function(fit0, fit_inlv) {
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
  pt$est[pt$free > 0] <- x[pt$free[pt$free > 0]]
  pt$se[pt$free > 0] <- SD[pt$free[pt$free > 0]]

  pt$par <- pt$parstart
  pt$par[pt$free > 0] <- fit_inlv$theta_star[pt$free[pt$free > 0]]

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

  ## ----- Update Fit slot -----------------------------------------------------
  fit0@Fit@x <- x
  # fit0@Fit@TH[[1]] <- tau  # FIXME: Group 1
  fit0@Fit@est <- pt$est
  fit0@Fit@se <- pt$se
  fit0@Fit@start <- pt$start

  if (fit_inlv$optim_method == "nlminb") {
    fit0@Fit@iterations <- as.integer(fit_inlv$opt$iterations)
    fit0@Fit@converged <- fit_inlv$opt$convergence == 0L
    fit0@Fit@fx <- fit_inlv$opt$objective
  } else if (fit_inlv$optim_method == "optim") {
    fit0@Fit@iterations <- as.integer(fit_inlv$opt$counts["function"])
    fit0@Fit@converged <- fit_inlv$opt$convergence == 0L
    fit0@Fit@fx <- fit_inlv$opt$value
  } else if (fit_inlv$optim_method == "ucminf") {
    fit0@Fit@iterations <- as.integer(fit_inlv$opt$info["neval"])
    fit0@Fit@converged <- fit_inlv$opt$convergence == 1L
    fit0@Fit@fx <- fit_inlv$opt$value
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
  fit0@vcov <- list() # FIXME: Do we need this?

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
  fit0@timing$total <- NULL
  fit0@timing$total <- sum(unlist(fit0@timing))
  names(fit0@timing) <- nms

  ## ----- Return --------------------------------------------------------------
  fit0@external <- list(
    inlavaan_internal = fit_inlv #[!grepl("lav", names(fit_inlv))]
  )
  fit0
}
