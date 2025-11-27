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
    nsamp = 10000,
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
  m              <- sum(lavpartable$free > 0)

  pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions)
  PARIDX <- pt$free > 0
  parnames <- pt$names[pt$free > 0]

  # Prep work for approximation ------------------------------------------------
  joint_lp <- function(pars) {
    x <- pars_to_x(pars, pt)
    ll <- loglik(x)

    pld <- 0
    if (isTRUE(add_priors)) pld <- prior_logdens(pars, pt)

    ll + pld
  }
  joint_lp_grad <- function(pars) {
    x <- pars_to_x(pars, pt)
    gll <- grad_loglik(x)

    # Jacobian adjustment, since we need:
    # d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ
    jcb <- diag(mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars))
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


    gpld <- 0
    if (isTRUE(add_priors)) gpld <- prior_grad(pars, pt)
    # jcb * gll + gpld
    as.numeric(jcb %*% gll) + gpld
  }

  if (isTRUE(verbose)) cli::cli_progress_step("Finding posterior mode.")
  parstart <- pt$parstart[pt$free > 0]
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

  # Stabilise inversion
  # H_neg <- 0.5 * (H_neg + t(H_neg))
  # eps <- 1e-6 * mean(diag(H_neg) ^ 2) ^ 0.5   # scale by Hessian magnitude
  # H_neg_reg <- H_neg + diag(eps, nrow(H_neg))
  # ok <- FALSE
  # tries <- 0
  # while (!ok && tries < 5) {
  #   tries <- tries + 1
  #   ok <- tryCatch({
  #     L <- chol(solve(H_neg_reg))  # directly get Sigma^{1/2}
  #     TRUE
  #   }, error = function(e) FALSE)
  #   if (!ok) H_neg_reg <- H_neg_reg + diag(eps * 10 ^ tries, nrow(H_neg))
  # }
  # Sigma_theta <- solve(H_neg_reg)

  # For whitening transformation: z = L^{-1}(theta - theta*)
  L <- t(chol(Sigma_theta))
  L_inv <- solve(L)

  # Approximations -------------------------------------------------------------
  pars_list <- setNames(as.list(1:m), paste0("pars[", 1:m, "]"))

  if (method == "sampling") {
    # Do sampling and return results
    if (isTRUE(verbose)) cli::cli_alert_info("Using sampling-based approximation.")
    if (isTRUE(verbose)) cli::cli_progress_step("Sampling from posterior.")
    approx_data <- NULL
    tmp <- post_marg_sampling(theta_star, Sigma_theta, pt, nsamp)
  } else {
    if (method == "asymgaus") {
      if (isTRUE(verbose)) cli::cli_alert_info("Using asymmetric Gaussian approximation.")
      if (isTRUE(verbose)) cli::cli_progress_step("Calibrating asymmetric Gaussians.")

      approx_data <-
        do.call(what = "rbind", lapply(
          pars_list,
          function(j) {
            k <- 2
            dplus  <- max(0.01, lp_max - joint_lp(theta_star + L[, j] * k))
            dminus <- max(0.01, lp_max - joint_lp(theta_star - L[, j] * k))
            c(sigma_plus = sqrt(k ^ 2 / (2 * dplus)),
              sigma_minus = sqrt(k ^ 2 / (2 * dplus)))
          }
        ))

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_asymgaus(j = j, g = g, g_prime = g_prime, ginv = ginv, ginv_prime = ginv_prime,
                           theta_star = theta_star, Sigma_theta = Sigma_theta,
                           sigma_asym = approx_data)
      }
    } else if (method == "skewnorm") {
      if (isTRUE(verbose)) cli::cli_progress_step("Fitting skew normal to marginals.")
      if (isTRUE(verbose)) cli::cli_alert_info("Using skew normal approximation.")

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
              yy[k] <- joint_lp(theta_star + L[, j] * mv[k])
            }
            yy <- yy - max(yy)  # normalise to have maximum at zero
            fit_sn <- fit_skew_normal(tt, yy)
            unlist(fit_sn)
          }
        ))

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_skewnorm(j = j, g = g, g_prime = g_prime, ginv = ginv, ginv_prime = ginv_prime,
                           theta_star = theta_star, Sigma_theta = Sigma_theta,
                           sn_params = approx_data)
      }
    } else if (method == "marggaus") {
      if (isTRUE(verbose)) cli::cli_alert_info("Using marginal Gaussian approximation.")
      approx_data <- NULL

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_marggaus(j = j, g = g, g_prime = g_prime, ginv = ginv, ginv_prime = ginv_prime,
                           theta_star = theta_star, Sigma_theta = Sigma_theta)
      }
    }

    # Compute posterior marginals ----------------------------------------------
    if (isTRUE(verbose)) cli::cli_progress_step("Preparing output.")
    tmp <- Map(
      f          = post_marg,
      j          = seq_len(m),
      # g          = rep(list(identity), m), #pt$g[pt$free > 0],
      # g_prime    = rep(list(\(x) 1), m), #pt$g_prime[pt$free > 0],
      # ginv       = rep(list(identity), m), #pt$ginv[pt$free > 0],
      # ginv_prime = rep(list(\(x) 1), m)
      g          = pt$g[pt$free > 0],
      g_prime    = pt$g_prime[pt$free > 0],
      ginv       = pt$ginv[pt$free > 0],
      ginv_prime = pt$ginv_prime[pt$free > 0]
    )
  }

  summ <- do.call("rbind", Map(
    f = function(x, y) {
      out <- t(data.frame(x$summary))
      row.names(out) <- y
      out
    },
    x = tmp,
    y = parnames
  ))

  pdf_data <- lapply(tmp, \(x) x$pdf_data)
  names(pdf_data) <- parnames

  # Sampling for covariances
  if (sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
    if (method == "sampling") {
      # Do nothing
    } else {
      if (isTRUE(verbose)) cli::cli_progress_step("Sampling posterior covariances.")

      if (method == "skewnorm") {
        samp_cov <- sample_covariances_fit_sn(theta_star, Sigma_theta, pt, nsamp)
      } else {
        samp_cov <- sample_covariances(theta_star, Sigma_theta, pt, nsamp)
      }

      for (cov_name in names(samp_cov)) {
        summ[cov_name, ] <- samp_cov[[cov_name]]$summary
        pdf_data[[cov_name]] <- samp_cov[[cov_name]]$pdf_data
      }
    }
  }

  # Compute ppp
  if (FALSE & method == "skewnorm") {
    if (isTRUE(verbose)) cli::cli_progress_step("Computing posterior predictive p-value.")
    ppp <- get_ppp(theta_star, Sigma_theta, approx_data, pt, lavmodel,
                   lavsamplestats, nsamp = nsamp)
  } else {
    ppp <- NA
  }

  coefs <- summ[, "Mean"]
  names(coefs) <- parnames

  summ <- as.data.frame(summ)
  summ$prior <- pt$prior[pt$free > 0]

  out <- list(
    coefficients = coefs,
    summary = summ,
    ppp = ppp,
    theta_star = as.numeric(theta_star),
    Sigma_theta = Sigma_theta,
    theta_star_trans = pars_to_x(theta_star, pt),
    approx_data = approx_data,
    pdf_data = pdf_data,
    partable = pt,
    lavmodel = lavmodel,
    lavsamplestats = lavsamplestats,
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


