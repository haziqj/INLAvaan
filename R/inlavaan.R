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
#' @param marginal_method The method for approximating the marginal posterior
#'   distributions. Options include `"skewnorm"` (skew normal), `"asymgaus"`
#'   (two-piece asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
#'   approximation), and `"sampling"` (sampling from the joint Laplace
#'   approximation).
#' @param nsamp The number of samples to draw for all sampling-based approaches
#'   (including posterior sampling for model fit indices).
#' @param test Logical indicating whether to compute posterior fit indices.
#' @param sn_fit_correction Logical indicating whether to apply half log
#'   determinant correction. More accurate but slower.
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
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  nsamp = 1000,
  test = TRUE,
  sn_fit_correction = TRUE,
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
  Sigma_theta <- solve(0.5 * (H_neg + t(H_neg)))

  # Derivatives at optima
  opt$dx <- numDeriv::grad(function(x) -1 * joint_lp(x), theta_star) # fd grad
  if (isTRUE(debug)) {
    grad_an <- -1 * joint_lp_grad(theta_star)
    print(cbind(fd = opt$dx, analytic = grad_an, diff = grad_an - opt$dx))
  }

  # Other info at optima
  lp_max <- joint_lp(theta_star)
  if (ceq.simple) {
    theta_star_trans <- pars_to_x(as.numeric(ceq.K %*% theta_star), pt)
  } else {
    theta_star_trans <- pars_to_x(theta_star, pt)
  }

  timing <- add_timing(timing, "optim")

  # Marginal log-likelihood (for BF comparison)
  mloglik <- lp_max + (m / 2) * log(2 * pi) + 0.5 * log(det(Sigma_theta))
  timing <- add_timing(timing, "loglik")

  ## ----- Marginal approximations ---------------------------------------------
  if (lavmodel@ceq.simple.only) {
    m <- length(theta_star)
  }
  pars_list <- setNames(as.list(1:m), paste0("pars[", 1:m, "]"))

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

      # For whitening transformation: z = L^{-1}(theta - theta*)
      L <- t(chol(Sigma_theta))
      L_inv <- solve(L)

      approx_data <-
        do.call(
          what = "rbind",
          lapply(
            pars_list,
            function(j) {
              # Gauge the drop in joint_lp in whitened Z space
              k <- 2
              dplus <- max(0.01, lp_max - joint_lp(theta_star + L[, j] * k))
              dminus <- max(0.01, lp_max - joint_lp(theta_star - L[, j] * k))
              c(
                sigma_plus = sqrt(k^2 / (2 * dplus)),
                sigma_minus = sqrt(k^2 / (2 * dplus))
              )
            }
          )
        )

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

      # For whitening transformation: z = L^{-1}(theta - theta*)
      L <- t(chol(Sigma_theta))
      L_inv <- solve(L)

      approx_data <- matrix(NA, nrow = m, ncol = 4)
      colnames(approx_data) <- c("xi", "omega", "alpha", "logC")

      obtain_approx_data <- function(j) {
        z <- seq(-4, 4, length = 31)
        yync <- yy <- numeric(length(z))

        gamma1 <- 0
        if (isTRUE(sn_fit_correction)) {
          # Simplifed Laplace Approximation correction using Forward
          # Difference
          H_mode_z <- t(L) %*% H_neg %*% L
          delta <- 0.01 # small step
          th_plus <- theta_star + L[, j] * delta
          H_plus_th <-
            numDeriv::jacobian(function(x) -1 * joint_lp_grad(x), th_plus)
          H_plus_z <- t(L) %*% H_plus_th %*% L
          d_curvature_dz <- diag(H_plus_z - H_mode_z) / delta
          gamma1 <- -0.5 * sum(d_curvature_dz[-j])
        }

        for (k in seq_along(z)) {
          yync[k] <- joint_lp(theta_star + L[, j] * z[k])
          yy[k] <- yync[k] + gamma1 * z[k]
        }

        fit_sn <- fit_skew_normal(
          x = z,
          y = yy - max(yy),
          threshold_log_drop = sn_fit_logthresh,
          temp = sn_fit_temp
        )

        # Adjust back to theta space
        fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
        fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])

        c(unlist(fit_sn), gamma1 = gamma1)
      }
      approx_data <- list()
      for (j in seq_len(m)) {
        approx_data[[j]] <- obtain_approx_data(j)
        if (isTRUE(verbose)) cli::cli_progress_update()
      }
      approx_data <-
        do.call(what = "rbind", approx_data)

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

  pdf_data <- lapply(postmargres, function(x) x$pdf_data)
  names(pdf_data) <- parnames

  coefs <- summ[, "Mean"]
  names(coefs) <- parnames

  summ <- as.data.frame(summ)
  summ$Prior <- pt$prior[PTFREEIDX]

  timing <- add_timing(timing, "marginals")

  ## ----- Sampling for covariances --------------------------------------------
  if (sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
    if (marginal_method == "sampling") {
      # Do nothing
    } else {
      if (isTRUE(verbose)) {
        cli::cli_progress_step("Sampling posterior covariances.")
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

  ## ----- Compute ppp and dic -------------------------------------------------
  if (isTRUE(test)) {
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
    timing = timing[-1] # remove start.time
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
  test = TRUE,
  sn_fit_cor = TRUE,
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
  test = TRUE,
  sn_fit_cor = TRUE,
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
  test = TRUE,
  sn_fit_cor = TRUE,
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
