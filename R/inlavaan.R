#' Fit an Approximate Bayesian Latent Variable Model
#'
#' This function fits a Bayesian latent variable model by approximating the
#' posterior distributions of the model parameters using various methods,
#' including skew normal, asymmetric Gaussian, marginal Gaussian, or
#' sampling-based approaches. It leverages the lavaan package for model
#' specification and estimation.
#'
#' @inheritParams lavaan::lavaan
#' @inheritParams lavaan::simulateData
#' @inheritParams blavaan::blavaan
#'
#' @param estimator The estimator to be used. Currently only `"ML"` (maximum
#'   likelihood) is supported.
#' @param vb_correction Logical indicating whether to apply a variational Bayes
#'   correction for the posterior mean vector of estimates. Defaults to `TRUE`.
#' @param marginal_method The method for approximating the marginal posterior
#'   distributions. Options include `"skewnorm"` (skew normal), `"asymgaus"`
#'   (two-piece asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
#'   approximation), and `"sampling"` (sampling from the joint Laplace
#'   approximation).
#' @param marginal_correction Which type of correction to use when fitting the
#'   skew normal or two-piece Gaussian marginals. `"hessian"` computes the full
#'   Hessian-based correction (slow), `"shortcut"` (default) computes only
#'   diagonals, and `"none"` (or `FALSE`) applies no correction.
#' @param nsamp The number of samples to draw for all sampling-based approaches
#'   (including posterior sampling for model fit indices).
#' @param test Character indicating whether to compute posterior fit indices.
#'   Defaults to "standard". Change to "none" to skip these computations.
#' @param sn_fit_logthresh The log-threshold for fitting the skew normal. Points
#'   with log-posterior drop below this threshold (relative to the maximum) will
#'   be excluded from the fit. Defaults to `-6`.
#' @param sn_fit_temp Temperature parameter for fitting the skew normal. If
#'   `NA`, the temperature will be included in the optimisation during the skew
#'   normal fit.
#' @param control A list of control parameters for the optimiser.
#' @param verbose Logical indicating whether to print progress messages.
#' @param debug Logical indicating whether to return debug information.
#' @param add_priors Logical indicating whether to include prior densities in
#'   the posterior computation.
#' @param optim_method The optimisation method to use for finding the posterior
#'   mode. Options include `"nlminb"` (default), `"ucminf"`, and `"optim"`
#'   (BFGS).
#' @param numerical_grad Logical indicating whether to use numerical gradients
#'   for the optimisation.
#' @param ... Additional arguments to be passed to the [lavaan] model fitting
#'   function.
#'
#' @seealso Typically, users will interact with the specific latent variable
#'   model functions instead, including [acfa()], [asem()], and [agrowth()].
#'
#' @example inst/examples/ex-inlavaan.R
#'
#' @return An S4 object of class `INLAvaan` which is a subclass of the
#'   [lavaan-class] class.
#' @export
inlavaan <- function(
  model,
  data,
  model.type = "sem",
  dp = blavaan::dpriors(),
  estimator = "ML",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "hessian", "none"),
  nsamp = 1000,
  test = "standard",
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  ...
) {
  start.time0 <- proc.time()[3]
  timing <- list(start.time = start.time0)

  ## ----- Check arguments -----------------------------------------------------
  marginal_method <- match.arg(marginal_method)
  if (isFALSE(marginal_correction)) {
    marginal_correction <- "none"
  } else {
    marginal_correction <- match.arg(marginal_correction)
  }
  optim_method <- match.arg(optim_method)
  if (isTRUE(debug)) {
    verbose <- TRUE
  }
  # estimator <- match.arg(estimator, choices = c("ML", "PML"))
  lavargs <- list(...)
  lavargs$model <- model
  lavargs$data <- data
  lavargs$estimator <- estimator
  lavargs$ceq.simple <- TRUE # FIXME: Force ceq.simple rather than eq.constraints
  lavargs$verbose <- FALSE # FIXME: Need some quiet mode maybe
  lavargs$do.fit <- FALSE
  lavargs$parser <- "old" # To get priors parsed
  lavargs$test <- test

  ## ----- Build joint log posterior -------------------------------------------
  if (estimator == "ML") {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Using MVN log-likelihood.")
    }
    loglik <- function(x) {
      mvnorm_loglik_samplestats(
        x,
        lavmodel,
        lavsamplestats,
        lavdata,
        lavoptions,
        lavcache
      )
    }
    grad_loglik <- function(x) {
      mvnorm_loglik_grad(x, lavmodel, lavsamplestats, lavdata, lavoptions)
    }
  } else if (estimator == "PML") {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Using pairwise log-likelihood.")
    }
    loglik <- function(x) {
      pl_fn(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache)
    }
    grad_loglik <- function(x) {
      pl_grad(x, lavmodel, lavsamplestats, lavdata, lavcache)
    }
  } else {
    # FIXME: Add Laplace?
    cli::cli_abort("That's not supported yet.")
  }

  ## ----- Initialise lavaan object --------------------------------------------
  fit0 <- do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
  lavmodel <- fit0@Model
  lavsamplestats <- fit0@SampleStats
  lavdata <- fit0@Data
  lavoptions <- fit0@Options
  lavpartable <- fit0@ParTable
  lavcache <- fit0@Cache
  n <- fit0@SampleStats@ntotal
  ceq.simple <- lavmodel@ceq.simple.only
  ceq.K <- lavmodel@ceq.simple.K # used to pack params/grads

  # Partable and check for equality constraints
  pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions)
  PTFREEIDX <- which(pt$free > 0L)
  if (isTRUE(ceq.simple)) {
    # Note: Always work in the reduced space
    PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
  }
  m <- length(PTFREEIDX)
  parnames <- pt$names[PTFREEIDX]

  ## ----- Prep work for approximation -----------------------------------------
  joint_lp <- function(pars) {
    if (isTRUE(ceq.simple)) {
      pars <- as.numeric(ceq.K %*% pars)
    } # Unpack
    x <- pars_to_x(pars, pt)
    ll <- loglik(x)
    pld <- 0
    if (isTRUE(add_priors)) {
      pld <- prior_logdens(pars, pt)
    }
    as.numeric(ll + pld)
  }

  joint_lp_grad <- function(pars) {
    # First, the likelihood gradient
    if (isTRUE(ceq.simple)) {
      pars <- as.numeric(ceq.K %*% pars)
    } # Unpack
    x <- pars_to_x(pars, pt)
    gll <- grad_loglik(x)

    # Jacobian adjustment: d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ
    jcb <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
    jcb <- diag(jcb, length(jcb))

    # This is the extra jacobian adjustment for covariances, since dx/dθ affects
    # more than one place if covariance exists
    sd1sd2 <- attr(x, "sd1sd2")
    jcb <- jcb * sd1sd2 # this adjusts the correlation parameters (diagonals)
    jcb_mat <- attr(x, "jcb_mat")

    if (!is.null(jcb_mat)) {
      for (k in seq_len(nrow(jcb_mat))) {
        i <- jcb_mat[k, 1]
        j <- jcb_mat[k, 2]
        jcb[i, j] <- jcb_mat[k, 3]
      }
    }
    gll_th <- as.numeric(jcb %*% gll) # this adjusts the cov parameters, if any
    if (isTRUE(ceq.simple)) {
      gll_th <- as.numeric(gll_th %*% ceq.K)
    } # Repack

    # Second, the prior gradient
    glp_th <- 0
    if (isTRUE(add_priors)) {
      glp_th <- prior_grad(pars, pt)
    }

    as.numeric(gll_th + glp_th)
  }

  timing <- add_timing(timing, "init")

  ## ----- Start optimisation --------------------------------------------------
  if (isTRUE(verbose)) {
    cli::cli_progress_step("Finding posterior mode.")
  }

  ob <- function(x) -1 * joint_lp(x)
  gr <- if (isTRUE(numerical_grad)) NULL else function(x) -1 * joint_lp_grad(x)
  parstart <- pt$parstart[PTFREEIDX]

  if (optim_method == "nlminb") {
    opt <- nlminb(
      start = parstart,
      objective = ob,
      gradient = gr,
      control = control
    )
    theta_star <- opt$par
    if (isTRUE(verbose)) {
      cli::cli_progress_step("Computing the Hessian.")
    }
    if (isTRUE(numerical_grad)) {
      H_neg <- numDeriv::hessian(
        func = ob,
        x = theta_star,
        method.args = list(eps = 1e-4, d = 0.0005) # high stability
      )
    } else {
      H_neg <- numDeriv::jacobian(function(x) -1 * joint_lp_grad(x), theta_star)
    }
  } else if (optim_method == "ucminf") {
    opt <- ucminf::ucminf(
      par = parstart,
      fn = ob,
      gr = gr,
      control = list(),
      hessian = 1
    )
    theta_star <- opt$par
    H_neg <- opt$hessian
  } else {
    opt <- stats::optim(
      par = parstart,
      fn = ob,
      gr = gr,
      method = "BFGS",
      hessian = TRUE,
      control = list()
    )
    theta_star <- opt$par
    H_neg <- opt$hessian
  }
  if (estimator == "PML") {
    # FIXME: Testing
    # First, the likelihood gradient
    .pars <- theta_star
    if (isTRUE(ceq.simple)) {
      .pars <- as.numeric(ceq.K %*% theta_star)
    } # Unpack
    .x <- pars_to_x(.pars, pt)

    lavopts <- lavoptions
    lavopts$se <- "robust.huber.white"

    Sigma_theta <- lavaan:::lav_model_vcov(
      lavmodel = lavaan::lav_model_set_parameters(lavmodel, .x),
      lavsamplestats = lavsamplestats,
      lavoptions = lavopts,
      lavdata = lavdata,
      lavpartable = lavpartable,
      lavcache = lavcache
    )
  } else {
    Sigma_theta <- solve(0.5 * (H_neg + t(H_neg)))
  }
  L <- t(chol(Sigma_theta)) # For whitening: z = L^{-1}(theta - theta*)

  lp_max <- joint_lp(theta_star) # before correction

  # Derivatives at optima
  opt$dx <- numDeriv::grad(function(x) -1 * joint_lp(x), theta_star) # fd grad
  if (isTRUE(debug)) {
    grad_an <- -1 * joint_lp_grad(theta_star)
    print(cbind(fd = opt$dx, analytic = grad_an, diff = grad_an - opt$dx))
  }

  timing <- add_timing(timing, "optim")

  ## ----- VB correction -------------------------------------------------------
  vb_opt <- vb_shift <- vb_kld <- vb_kld_global <- NA
  if (isTRUE(vb_correction)) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step("Performing VB correction.")
    }

    # QMC noise (scrambled Sobol)
    us <- qrng::sobol(n = 50, d = m, randomize = "Owen", seed = 123)
    set.seed(NULL)
    zs <- qnorm(us) %*% t(L)

    vb_ob <- function(eta, mu0, Z) {
      # eta = t(L) %*% delta, the shift in whitened space
      mu_new <- mu0 + as.numeric(L %*% eta)

      ns <- nrow(Z)
      lp_total <- 0
      for (i in seq_len(ns)) {
        thetai <- mu_new + Z[i, ]
        lp_total <- lp_total + joint_lp(thetai)
      }

      -1 * (lp_total / ns)
    }

    vb_gr <- function(eta, mu0, Z) {
      mu_new <- mu0 + as.numeric(L %*% eta)

      ns <- nrow(Z)
      lpgrad_total <- numeric(length(mu0))
      for (i in seq_len(ns)) {
        thetai <- mu_new + Z[i, ]
        lpgrad_total <- lpgrad_total + joint_lp_grad(thetai)
      }

      lpgrad_avg <- -1 * lpgrad_total / ns
      as.numeric(t(L) %*% lpgrad_avg)
    }

    vb_opt <- nlminb(
      start = rep(0, m),
      objective = vb_ob,
      gradient = vb_gr,
      mu0 = theta_star,
      Z = zs,
      control = list(rel.tol = 1e-4)
    )

    vb_shift <- as.numeric(L %*% vb_opt$par)
    vb_kld <- (vb_shift)^2 / (2 * diag(Sigma_theta))
    vb_kld_global <- lp_max + vb_opt$objective
  }

  vb <- list(
    opt = vb_opt,
    correction = vb_shift,
    kld = vb_kld,
    kld_global = vb_kld_global
  )

  timing <- add_timing(timing, "vb")

  ## ----- Info at optima ------------------------------------------------------
  if (isTRUE(vb_correction)) {
    theta_star <- theta_star + vb_shift
  }
  if (ceq.simple) {
    theta_star_trans <- pars_to_x(as.numeric(ceq.K %*% theta_star), pt)
  } else {
    theta_star_trans <- pars_to_x(theta_star, pt)
  }

  # Marginal log-likelihood (for BF comparison)
  mloglik <- lp_max + (m / 2) * log(2 * pi) + 0.5 * log(det(Sigma_theta))
  if (isTRUE(vb_correction)) {
    mloglik <- mloglik - vb_kld_global
  }
  timing <- add_timing(timing, "loglik")

  ## ----- Marginal approximations ---------------------------------------------
  # pars_list <- setNames(as.list(1:m), paste0("pars[", 1:m, "]"))
  pars_list <- setNames(as.list(1:m), parnames)
  visual_debug <- NULL

  # When asymgaus or skewnorm marginals, we need the correction factor gamma1
  if (marginal_method %in% c("asymgaus", "skewnorm")) {
    # Step size for finite difference / central difference
    delta_outer <- 0.01 # for rate of change of Hessian (3rd deriv)
    delta_inner <- 0.001 # for rate of change of gradients (2nd deriv)

    if (marginal_correction %in% c("shortcut", "hessian")) {
      # Precompute baseline Hessian (diagonal of Hessian_z at mode)
      Hz0 <- numeric(m)
      for (j in 1:m) {
        g_fwd <- -1 * joint_lp_grad(theta_star + L[, j] * delta_inner)
        g_bwd <- -1 * joint_lp_grad(theta_star - L[, j] * delta_inner)
        Hz0[j] <- sum(L[, j] * (g_fwd - g_bwd)) / (2 * delta_inner)
      }
    }

    get_gamma1 <- function(.j) {
      if (marginal_correction == "none") {
        gamma1j <- 0
      } else {
        th_plus <- theta_star + L[, .j] * delta_outer
        if (marginal_correction == "hessian") {
          Htheta1_full <- numDeriv::jacobian(
            function(x) -1 * joint_lp_grad(x),
            th_plus
          )
          Hz1 <- diag(t(L) %*% Htheta1_full %*% L)
        } else if (marginal_correction == "shortcut") {
          Hz1 <- numeric(m)
          for (jj in 1:m) {
            g_fwd <- -1 * joint_lp_grad(th_plus + L[, jj] * delta_inner)
            g_bwd <- -1 * joint_lp_grad(th_plus - L[, jj] * delta_inner)
            Hz1[jj] <- sum(L[, jj] * (g_fwd - g_bwd)) / (2 * delta_inner)
          }
        }
        dH_dz <- (Hz1 - Hz0) / delta_outer
        gamma1j <- -0.5 * sum(dH_dz[-.j])
      }
      gamma1j
    }
  }

  if (marginal_method == "sampling") {
    # Do sampling and return results
    if (isTRUE(verbose)) {
      cli::cli_progress_done()
      cli::cli_alert_info("Using sampling-based approximation.")
      cli::cli_progress_step("Sampling from posterior.")
    }
    approx_data <- NULL
    postmargres <- post_marg_sampling(theta_star, Sigma_theta, pt, ceq.K, nsamp)
  } else {
    if (marginal_method == "asymgaus") {
      if (isTRUE(verbose)) {
        cli::cli_progress_done()
        cli::cli_alert_info("Using asymmetric Gaussian approximation.")
        cli::cli_progress_step("Calibrating asymmetric Gaussians.")
      }

      obtain_approx_data <- function(j) {
        # Gauge the drop in joint_lp in whitened Z space
        k <- 2
        gamma1j <- get_gamma1(j)
        dplus <- max(
          0.01,
          lp_max - joint_lp(theta_star + L[, j] * k) - gamma1j * k
        )
        dminus <- max(
          0.01,
          lp_max - joint_lp(theta_star - L[, j] * k) + gamma1j * k
        )
        c(
          sigma_plus = sqrt(k^2 / (2 * dplus)),
          sigma_minus = sqrt(k^2 / (2 * dplus))
        )
      }

      approx_data <- list()
      for (j in seq_len(m)) {
        approx_data[[j]] <- obtain_approx_data(j)
      }
      approx_data <- do.call(what = "rbind", approx_data)

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_asymgaus(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star,
          Sigma_theta = Sigma_theta,
          sigma_asym = approx_data
        )
      }
    } else if (marginal_method == "skewnorm") {
      if (isTRUE(verbose)) {
        cli::cli_progress_done()
        cli::cli_alert_info("Using skew normal approximation.")
        j <- 0
        cli::cli_progress_step(
          "Fitting skew normal to {j}/{m} marginal{?s}.",
          spinner = TRUE
        )
      }

      obtain_approx_data <- function(j) {
        z <- seq(-4, 4, length = 31)
        yync <- yy <- numeric(length(z))
        gamma1j <- get_gamma1(j)

        for (k in seq_along(z)) {
          yync[k] <- joint_lp(theta_star + L[, j] * z[k])
          yy[k] <- yync[k] + gamma1j * z[k]
        }

        fit_sn <- fit_skew_normal(
          x = z,
          y = yy - max(yy),
          threshold_log_drop = sn_fit_logthresh,
          temp = sn_fit_temp
        )

        if (isTRUE(debug)) {
          visual_debug[[j]] <<- data.frame(
            x = z,
            Original = exp(yync - max(yync)),
            Corrected = exp(yy - max(yy)),
            SN_Fit = dsnorm(
              x = z,
              xi = fit_sn$xi,
              omega = fit_sn$omega,
              alpha = fit_sn$alpha,
              logC = fit_sn$logC
            )
          )
        }

        # Adjust back to theta space
        fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
        fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])

        c(unlist(fit_sn), gamma1 = gamma1j)
      }

      approx_data <- visual_debug <- vector("list", length = m)
      for (j in seq_len(m)) {
        approx_data[[j]] <- obtain_approx_data(j)
        if (isTRUE(verbose)) cli::cli_progress_update()
      }
      approx_data <- do.call(what = "rbind", approx_data)
      rownames(approx_data) <- parnames
      names(visual_debug) <- parnames

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_skewnorm(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star,
          Sigma_theta = Sigma_theta,
          sn_params = approx_data
        )
      }
    } else if (marginal_method == "marggaus") {
      if (isTRUE(verbose)) {
        cli::cli_alert_info("Using marginal Gaussian approximation.")
      }

      approx_data <- NULL

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_marggaus(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star,
          Sigma_theta = Sigma_theta
        )
      }
    }

    # Compute posterior marginals ----------------------------------------------
    postmargres <- Map(
      f = post_marg,
      j = seq_len(m),
      g = pt$g[PTFREEIDX],
      g_prime = pt$g_prime[PTFREEIDX],
      ginv = pt$ginv[PTFREEIDX],
      ginv_prime = pt$ginv_prime[PTFREEIDX]
    )
  }

  summ <- do.call(
    "rbind",
    Map(
      f = function(x, y) {
        out <- t(data.frame(x$summary))
        row.names(out) <- y
        out
      },
      x = postmargres,
      y = parnames
    )
  )
  summ <- cbind(summ, kld = vb$kld)

  pdf_data <- lapply(postmargres, function(x) x$pdf_data)
  names(pdf_data) <- parnames

  coefs <- summ[, "Mean"]
  names(coefs) <- parnames

  summ <- as.data.frame(summ)
  summ$Prior <- pt$prior[PTFREEIDX]

  timing <- add_timing(timing, "marginals")

  ## ----- Sampling for covariances and defined params -------------------------
  if (sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
    if (marginal_method == "sampling") {
      # Do nothing
    } else {
      if (isTRUE(verbose)) {
        cli::cli_progress_step("Sampling covariances and defined parameters.")
      }

      if (marginal_method == "skewnorm") {
        samp_cov <- sample_covariances_fit_sn(
          theta_star,
          Sigma_theta,
          pt,
          ceq.K,
          nsamp
        )
      } else {
        samp_cov <- sample_covariances(
          theta_star,
          Sigma_theta,
          pt,
          ceq.K,
          nsamp
        )
      }

      for (cov_name in names(samp_cov)) {
        tmp_new_summ <- samp_cov[[cov_name]]$summary
        summ[cov_name, names(tmp_new_summ)] <- tmp_new_summ
        pdf_data[[cov_name]] <- samp_cov[[cov_name]]$pdf_data
      }
    }
  }
  timing <- add_timing(timing, "covariances")

  # Defined parameters
  if (any(pt$op == ":=")) {
    defpars <- get_defpars(
      theta_star,
      Sigma_theta,
      marginal_method,
      approx_data,
      pt,
      lavmodel,
      lavsamplestats,
      nsamp = 250
    )
    for (def_name in names(defpars)) {
      tmp_new_summ <- defpars[[def_name]]$summary
      summ[def_name, names(tmp_new_summ)] <- tmp_new_summ
      pdf_data[[def_name]] <- defpars[[def_name]]$pdf_data
    }
  }
  timing <- add_timing(timing, "definedpars")

  ## ----- Compute ppp and dic -------------------------------------------------
  if (test != "none") {
    env <- NULL
    if (isTRUE(verbose)) {
      env <- environment()
      cli::cli_progress_step(
        "Computing ppp and DIC.",
        spinner = TRUE,
        .envir = env
      )
    }
    ppp <- get_ppp(
      theta_star,
      Sigma_theta,
      marginal_method,
      approx_data,
      pt,
      lavmodel,
      lavsamplestats,
      nsamp = nsamp,
      cli_env = env
    )
    dic_list <- get_dic(
      theta_star,
      Sigma_theta,
      marginal_method,
      approx_data,
      pt,
      lavmodel,
      lavsamplestats,
      loglik,
      nsamp = nsamp,
      cli_env = env
    )
  } else {
    ppp <- dic_list <- NULL
  }
  timing <- add_timing(timing, "test")

  ## ----- Output --------------------------------------------------------------
  out <- list(
    coefficients = coefs,
    mloglik = mloglik,
    DIC = dic_list,
    summary = summ,
    ppp = ppp,
    optim_method = optim_method,
    marginal_method = marginal_method,
    theta_star = as.numeric(theta_star),
    Sigma_theta = Sigma_theta,
    theta_star_trans = theta_star_trans,
    approx_data = approx_data,
    pdf_data = pdf_data,
    partable = pt,
    lavmodel = lavmodel,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    opt = opt,
    timing = timing[-1], # remove start.time
    visual_debug = visual_debug,
    vb = vb
  )
  class(out) <- "inlavaan_internal"

  if (isTRUE(debug)) {
    return(out)
  } else {
    out <- create_lav_from_inlavaan_internal(fit0, out)
    return(new("INLAvaan", out))
  }
}

#' Fit an Approximate Bayesian Confirmatory Factor Analysis Model
#'
#' Fit an Approximate Bayesian Confirmatory Factor Analysis Model
#'
#' The [acfa()] function is a wrapper for the more general [inlavaan()]
#' function, using the following default arguments:
#'   - `int.ov.free = TRUE`
#'   - `int.lv.free = FALSE`
#'   - `auto.fix.first = TRUE` (unless `std.lv = TRUE`)
#'   - `auto.fix.single = TRUE`
#'   - `auto.var = TRUE`
#'   - `auto.cov.lv.x = TRUE`
#'   - `auto.efa = TRUE`
#'   - `auto.th = TRUE`
#'   - `auto.delta = TRUE`
#'   - `auto.cov.y = TRUE`
#'
#' For further information regarding these arguments, please refer to the
#' [lavaan::lavOptions()] documentation.
#'
#' @inherit inlavaan params return seealso
#' @example inst/examples/ex-cfa.R
#' @export
acfa <- function(
  model,
  data,
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 3000,
  test = "standard",
  marginal_correction = c("shortcut", "hessian", "none"),
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  ...
) {
  sc <- sys.call()
  sc[["model.type"]] <- quote("cfa")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}

#' Fit an Approximate Bayesian Structural Equation Model
#'
#' Fit an Approximate Bayesian Structural Equation Model
#'
#' The [asem()] function is a wrapper for the more general [inlavaan()]
#' function, using the following default arguments:
#'   - `int.ov.free = TRUE`
#'   - `int.lv.free = FALSE`
#'   - `auto.fix.first = TRUE` (unless `std.lv = TRUE`)
#'   - `auto.fix.single = TRUE`
#'   - `auto.var = TRUE`
#'   - `auto.cov.lv.x = TRUE`
#'   - `auto.efa = TRUE`
#'   - `auto.th = TRUE`
#'   - `auto.delta = TRUE`
#'   - `auto.cov.y = TRUE`
#'
#' For further information regarding these arguments, please refer to the
#' [lavaan::lavOptions()] documentation.
#'
#' @inherit inlavaan params return seealso
#' @example inst/examples/ex-sem.R
#' @export
asem <- function(
  model,
  data,
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 3000,
  test = "standard",
  marginal_correction = c("shortcut", "hessian", "none"),
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  ...
) {
  sc <- sys.call()
  sc[["model.type"]] <- quote("sem")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}

#' Fit an Approximate Bayesian Growth Curve Model
#'
#' Fit an Approximate Bayesian Growth Curve Model
#'
#' The [asem()] function is a wrapper for the more general [inlavaan()]
#' function, using the following default arguments:
#'   - `meanstructure = TRUE`
#'   - `int.ov.free = FALSE`
#'   - `int.lv.free = TRUE`
#'   - `auto.fix.first = TRUE` (unless `std.lv = TRUE`)
#'   - `auto.fix.single = TRUE`
#'   - `auto.var = TRUE`
#'   - `auto.cov.lv.x = TRUE`
#'   - `auto.efa = TRUE`
#'   - `auto.th = TRUE`
#'   - `auto.delta = TRUE`
#'   - `auto.cov.y = TRUE`
#'
#' @inherit inlavaan params return seealso
#' @example inst/examples/ex-growth.R
#' @export
agrowth <- function(
  model,
  data,
  dp = blavaan::dpriors(),
  estimator = "ML",
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 3000,
  test = "standard",
  marginal_correction = c("shortcut", "hessian", "none"),
  sn_fit_logthresh = -6,
  sn_fit_temp = NA,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  ...
) {
  sc <- sys.call()
  sc[["model.type"]] <- quote("growth")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}
