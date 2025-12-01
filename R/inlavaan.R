#' @export
inlavaan <- function(
    model,
    data,
    lavfun = "sem",
    estimator = "ML",
    method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
    start = NULL,
    control = list(
      eval.max = 4000,
      iter.max = 2000,
      rel.tol  = 1e-12,
      x.tol    = 1e-12,
      step.min = 1e-8,
      step.max = 1.0
    ),
    verbose = TRUE,
    add_priors = TRUE,
    nsamp = 3000,
    dp = blavaan::dpriors(),
    optim = c("nlminb", "ucminf", "optim"),
    ...
) {

  # Check arguments ------------------------------------------------------------
  method <- match.arg(method)
  optim <- match.arg(optim)
  # estimator <- match.arg(estimator, choices = c("ML", "PML"))
  lavargs <- list(...)
  lavargs$model <- model
  lavargs$data <- data
  lavargs$estimator <- estimator
  lavargs$ceq.simple <- TRUE  # FIXME: Force ceq.simple rather than eq.constraints
  lavargs$verbose <- FALSE  # FIXME: Need some quiet mode maybe
  lavargs$do.fit <- FALSE
  lavargs$parser <- "old"  # To get priors parsed

  # Build joint log posterior --------------------------------------------------
  if (estimator == "ML") {
    if (isTRUE(verbose)) cli::cli_alert_info("Using MVN log-likelihood.")
    loglik <- function(x) {
      mvnorm_loglik_samplestats(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache)
    }
    grad_loglik <- function(x) {
      mvnorm_loglik_grad(x, lavmodel, lavsamplestats, lavdata, lavoptions)
    }
  } else if (estimator == "PML") {
    if (isTRUE(verbose)) cli::cli_alert_info("Using pairwise log-likelihood.")
    loglik <- function(x) {
      pl_fn(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache)
    }
  } else {
    # FIXME: Add Laplace?
    cli::cli_abort("That's not supported yet.")
  }

  # Initialise lavaan object ---------------------------------------------------
  fit0 <- do.call(get(lavfun, envir = asNamespace("lavaan")), lavargs)
  lavmodel       <- fit0@Model
  lavsamplestats <- fit0@SampleStats
  lavdata        <- fit0@Data
  lavoptions     <- fit0@Options
  lavpartable    <- fit0@ParTable
  lavcache       <- fit0@Cache
  n              <- fit0@SampleStats@ntotal
  ceq.simple     <- lavmodel@ceq.simple.only
  ceq.K          <- lavmodel@ceq.simple.K  # used to pack params/grads

  # Partable and check for equality constraints
  pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions)
  PTFREEIDX <- which(pt$free > 0L)
  if (isTRUE(ceq.simple)) {
    # Note: Always work in the reduced space
    PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
  }
  m <- length(PTFREEIDX)
  parnames <- pt$names[PTFREEIDX]

  # Prep work for approximation ------------------------------------------------
  joint_lp <- function(pars) {
    if (isTRUE(ceq.simple)) pars <- as.numeric(ceq.K %*% pars)  # Unpack
    x <- pars_to_x(pars, pt)
    ll <- loglik(x)
    pld <- 0
    if (isTRUE(add_priors)) pld <- prior_logdens(pars, pt)
    as.numeric(ll + pld)
  }

  joint_lp_grad <- function(pars) {
    # First, the likelihood gradient
    if (isTRUE(ceq.simple)) pars <- as.numeric(ceq.K %*% pars)  # Unpack
    x <- pars_to_x(pars, pt)
    gll <- grad_loglik(x)

    # Jacobian adjustment: d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ
    jcb <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
    jcb <- diag(jcb, length(jcb))

    # This is the extra jacobian adjustment for covariances, since dx/dθ affects
    # more than one place if covariance exists
    sd1sd2 <- attr(x, "sd1sd2")
    jcb <- jcb * sd1sd2
    jcb_mat <- attr(x, "jcb_mat")

    if (!is.null(jcb_mat)) {
      for (k in seq_len(nrow(jcb_mat))) {
        i <- jcb_mat[k, 1]
        j <- jcb_mat[k, 2]
        jcb[i, j] <- jcb_mat[k, 3]
      }
    }
    gll_th <- as.numeric(jcb %*% gll)
    if (isTRUE(ceq.simple)) gll_th <- as.numeric(gll_th %*%ceq.K)  # Repack

    # Second, the prior gradient
    glp_th <- 0
    if (isTRUE(add_priors)) glp_th <- prior_grad(pars, pt)

    as.numeric(gll_th + glp_th)
  }

  if (isTRUE(verbose)) cli::cli_progress_step("Finding posterior mode.")
  parstart <- pt$parstart[PTFREEIDX]
  if (optim == "nlminb") {
    opt <- nlminb(
      start = parstart,
      objective = function(x) -1 * joint_lp(x),
      gradient = function(x) -1 * joint_lp_grad(x),
      control = control
    )
    theta_star <- opt$par
    if (isTRUE(verbose)) cli::cli_progress_step("Computing the Hessian.")
    # H_neg <- numDeriv::hessian(
    #   func = \(x) -1 * joint_lp(x),
    #   x = theta_star,
    #   method.args = list(eps = 1e-4, d = 0.0005) # high stability
    # )
    grad_fd <- numDeriv::grad(\(x) -1 * joint_lp(x), theta_star)
    grad_an <- -1 * joint_lp_grad(theta_star)
    print(cbind(fd = grad_fd, analytic = grad_an, diff = grad_an - grad_fd))
    H_neg <- numDeriv::jacobian(function(x) -1 * joint_lp_grad(x), theta_star)
    Sigma_theta <- solve(0.5 * (H_neg + t(H_neg)))
  } else if (optim == "ucminf") {
    opt <- ucminf::ucminf(
      par = parstart,
      fn = function(x) -1 * joint_lp(x),
      gr = function(x) -1 * joint_lp_grad(x),
      control = list(),
      hessian = 2
    )
    theta_star <- opt$par
    Sigma_theta <- opt$invhessian
  } else {
    opt <- stats::optim(
      par = parstart,
      fn = function(x) -1 * joint_lp(x),
      gr = function(x) -1 * joint_lp_grad(x),
      method = "BFGS",
      hessian = TRUE,
      control = list()
    )
    theta_star <- opt$par
    Sigma_theta <- solve(opt$hessian)
  }
  lp_max <- joint_lp(theta_star)

  # Approximations -------------------------------------------------------------
  if (lavmodel@ceq.simple.only) m <- length(theta_star)
  pars_list <- setNames(as.list(1:m), paste0("pars[", 1:m, "]"))

  if (method == "sampling") {
    # Do sampling and return results
    if (isTRUE(verbose)) cli::cli_alert_info("Using sampling-based approximation.")
    if (isTRUE(verbose)) cli::cli_progress_step("Sampling from posterior.")
    approx_data <- NULL
    postmargres <- post_marg_sampling(theta_star, Sigma_theta, pt, ceq.K, nsamp)
  } else {
    if (method == "asymgaus") {
      if (isTRUE(verbose)) {
        cli::cli_alert_info("Using asymmetric Gaussian approximation.")
        cli::cli_progress_step("Calibrating asymmetric Gaussians.")
      }

      # For whitening transformation: z = L^{-1}(theta - theta*)
      L <- t(chol(Sigma_theta))
      L_inv <- solve(L)

      approx_data <-
        do.call(what = "rbind", lapply(
          pars_list,
          function(j) {
            # Gauge the drop in joint_lp in whitened Z space
            k <- 2
            dplus  <- max(0.01, lp_max - joint_lp(theta_star + L[, j] * k))
            dminus <- max(0.01, lp_max - joint_lp(theta_star - L[, j] * k))
            c(sigma_plus = sqrt(k ^ 2 / (2 * dplus)),
              sigma_minus = sqrt(k ^ 2 / (2 * dplus)))
          }
        ))

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_asymgaus(j = j, g = g, g_prime = g_prime, ginv = ginv,
                           ginv_prime = ginv_prime, theta_star = theta_star,
                           Sigma_theta = Sigma_theta, sigma_asym = approx_data)
      }
    } else if (method == "skewnorm") {
      if (isTRUE(verbose)) {
        cli::cli_progress_step("Fitting skew normal to marginals.")
        cli::cli_alert_info("Using skew normal approximation.")
      }

      # For whitening transformation: z = L^{-1}(theta - theta*)
      L <- t(chol(Sigma_theta))
      L_inv <- solve(L)

      approx_data <- matrix(NA, nrow = m, ncol = 4)
      colnames(approx_data) <- c("xi", "omega", "alpha", "logC")

      approx_data <-
        do.call(what = "rbind", lapply(
          pars_list,
          function(j) {
            mv <- seq(-4, 4, length = 21)
            tt <- theta_star[j] + mv * sqrt(Sigma_theta[j, j])
            yy <- numeric(length(mv))
            for (k in seq_along(mv)) {
              # # Evaluate joint_lp at theta_j with others fixed at the
              # # conditional mode
              # theta_new <- rep(NA, length(theta_star))
              # theta_new[j] <- tt[k]
              # theta_new[-j] <- theta_star[-j] +
              #   Sigma_theta[-j, j] / Sigma_theta[j, j] * (tt[k] - theta_star[j])
              # yy[k] <- joint_lp(theta_new)

              # Fit in decoupled Z-space
              tt[k] <- mv[k]
              yy[k] <- joint_lp(theta_star + L[, j] * mv[k])
            }
            fit_sn <- fit_skew_normal(tt, yy - max(yy))

            fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
            fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])
            unlist(fit_sn)
          }
        ))

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_skewnorm(j = j, g = g, g_prime = g_prime, ginv = ginv,
                           ginv_prime = ginv_prime, theta_star = theta_star,
                           Sigma_theta = Sigma_theta, sn_params = approx_data)
      }
    } else if (method == "marggaus") {
      if (isTRUE(verbose))
        cli::cli_alert_info("Using marginal Gaussian approximation.")

      approx_data <- NULL

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_marggaus(j = j, g = g, g_prime = g_prime, ginv = ginv,
                           ginv_prime = ginv_prime, theta_star = theta_star,
                           Sigma_theta = Sigma_theta)
      }
    }

    # Compute posterior marginals ----------------------------------------------
    if (isTRUE(verbose)) cli::cli_progress_step("Preparing output.")
    postmargres <- Map(
      f          = post_marg,
      j          = seq_len(m),
      g          = pt$g[PTFREEIDX],
      g_prime    = pt$g_prime[PTFREEIDX],
      ginv       = pt$ginv[PTFREEIDX],
      ginv_prime = pt$ginv_prime[PTFREEIDX]
    )
  }

  summ <- do.call("rbind", Map(
    f = function(x, y) {
      out <- t(data.frame(x$summary))
      row.names(out) <- y
      out
    },
    x = postmargres,
    y = parnames
  ))

  pdf_data <- lapply(postmargres, \(x) x$pdf_data)
  names(pdf_data) <- parnames

  # Sampling for covariances
  if (sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
    if (method == "sampling") {
      # Do nothing
    } else {
      if (isTRUE(verbose)) cli::cli_progress_step("Sampling posterior covariances.")

      if (method == "skewnorm") {
        samp_cov <- sample_covariances_fit_sn(theta_star, Sigma_theta, pt, ceq.K, nsamp)
      } else {
        samp_cov <- sample_covariances(theta_star, Sigma_theta, pt, ceq.K, nsamp)
      }

      for (cov_name in names(samp_cov)) {
        summ[cov_name, ] <- samp_cov[[cov_name]]$summary
        pdf_data[[cov_name]] <- samp_cov[[cov_name]]$pdf_data
      }
    }
  }

  # Compute ppp
  if (FALSE){ #method == "skewnorm" | method == "asymgaus") {
    env <- NULL
    if (isTRUE(verbose)) {
      env <- environment()
      cli::cli_progress_step("Computing posterior predictive p-value.",
                             spinner = TRUE, .envir = env)
    }
    ppp <- get_ppp(theta_star, Sigma_theta, method, approx_data, pt, lavmodel,
                   lavsamplestats, nsamp = nsamp, cli_env = env)
  } else {
    ppp <- NA
  }

  # Unpack?
  # idx <- lavpartable$free[lavpartable$free > 0]

  coefs <- summ[, "Mean"]
  names(coefs) <- parnames

  summ <- as.data.frame(summ)
  summ$Prior <- pt$prior[PTFREEIDX]

  if (lavmodel@ceq.simple.only) {
    theta_star_trans <- pars_to_x(as.numeric(lavmodel@ceq.simple.K %*% theta_star), pt)
  } else {
    theta_star_trans <- pars_to_x(theta_star, pt)
  }

  # Marginal log-likelihood (for BF comparison)
  marg_loglik <- lp_max + (m / 2) * log(2 * pi) + 0.5 * log(det(Sigma_theta))
  AIC <- NA
  BIC <- NA

  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, coefs)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigmay <- lavimplied$cov

  out <- list(
    coefficients = coefs,
    marg_loglik = marg_loglik,
    AIC = AIC,
    BIC = BIC,
    summary = summ,
    ppp = ppp,
    method = method,
    theta_star = as.numeric(theta_star),
    Sigma_theta = Sigma_theta,
    theta_star_trans = theta_star_trans,
    approx_data = approx_data,
    pdf_data = pdf_data,
    partable = pt,
    lavmodel = lavmodel,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    Sigmay = Sigmay,
    opt = opt
  )
  class(out) <- "inlavaan_internal"
  out
}

#' @export
print.inlavaan_internal <- function(x, digits = 3, ...) {
  print(round(x$coefficients, digits))
  invisible(x)
}

#' @export
summary.inlavaan_internal <- function(object, ...) {
  structure(
    list(summary = object$summary),
    class = "summary.inlavaan_internal"
  )
}

#' @export
print.summary.inlavaan_internal <- function(x, digits = 3, ...) {
  summ <- x$summary
  which_numeric <- sapply(summ, is.numeric)
  summ[, which_numeric] <- round(summ[, which_numeric], digits)
  print(summ)
  invisible(x)
}

#' @export
plot.inlavaan_internal <- function(x, ...) {
  all_plots <- list()
  postmode <- x$summary[, "Mode"]

  for (j in seq_along(x$pdf_data)) {
    param <- names(x$pdf_data)[j]
    plot_df <- x$pdf_data[[param]]

    p_dens <-
      ggplot(plot_df, aes(x, y)) +
      geom_line() +
      geom_vline(xintercept = postmode[j], linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(x = NULL, y = NULL, subtitle = param)

    all_plots[[param]] <- p_dens
  }

  cowplot::plot_grid(plotlist = all_plots)
}


