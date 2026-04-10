#' Fit an Approximate Bayesian Latent Variable Model
#'
#' This function fits a Bayesian latent variable model by approximating the
#' posterior distributions of the model parameters using various methods,
#' including skew-normal, asymmetric Gaussian, marginal Gaussian, or
#' sampling-based approaches. It leverages the lavaan package for model
#' specification and estimation.
#'
#' @inheritParams lavaan::lavaan
#' @inheritParams lavaan::simulateData
#' @inheritParams blavaan::blavaan
#'
#' @param test Character indicating whether to compute posterior fit indices.
#'   Defaults to "standard". Change to "none" to skip these computations.
#' @param vb_correction Logical indicating whether to apply a variational Bayes
#'   correction for the posterior mean vector of estimates. Defaults to `TRUE`.
#' @param marginal_method The method for approximating the marginal posterior
#'   distributions. Options include `"skewnorm"` (skew-normal), `"asymgaus"`
#'   (two-piece asymmetric Gaussian), `"marggaus"` (marginalising the Laplace
#'   approximation), and `"sampling"` (sampling from the joint Laplace
#'   approximation).
#' @param marginal_correction Which type of correction to use when fitting the
#'   skew-normal or two-piece Gaussian marginals. `"hessian"` computes the full
#'   `"shortcut"` (default) computes only diagonals via central differences
#'   (full z-trace plus Schur complement correction), `"shortcut_fd"` is the
#'   same formula using forward differences (roughly half the cost, less
#'   accurate), `"hessian"` computes the full Hessian-based correction (slow),
#'   and `"none"` (or `FALSE`) applies no correction.
#' @param nsamp The number of samples to draw for all sampling-based approaches
#'   (including posterior sampling for model fit indices).
#' @param samp_copula Logical. When `TRUE` (default), posterior samples are
#'   drawn using the copula method with the fitted marginals (e.g.
#'   skew-normal or asymmetric Gaussian), with NORTA correlation adjustment.
#'   When `FALSE`, samples are drawn from the Gaussian (Laplace)
#'   approximation. Only re
#' @param sn_fit_ngrid Number of grid points to lay out per dimension when
#'   fitting the skew-normal marginals. A finer grid gives a better fit at the
#'   cost of more joint-log-posterior evaluations. Defaults to `21`.
#' @param sn_fit_logthresh The log-threshold for fitting the skew-normal. Points
#'   with log-posterior drop below this threshold (relative to the maximum) will
#'   be excluded from the fit. Defaults to `-6`.
#' @param sn_fit_temp Temperature parameter for fitting the skew-normal.
#'   Defaults to `1` (weights are the density values themselves). If
#'   `NA`, the temperature is included as an additional optimisation parameter.
#' @param sn_fit_sample Logical. When `TRUE` (default), a parametric
#'   skew-normal is fitted to the posterior samples for covariance and defined
#'   parameters. When `FALSE`, these are summarised using kernel density
#'   estimation instead.
#' @param control A list of control parameters for the optimiser.
#' @param verbose Logical indicating whether to print progress messages.
#' @param debug Logical indicating whether to return debug information.
#' @param add_priors Logical indicating whether to include prior densities in
#'   the posterior computation.
#' @param optim_method The optimisation method to use for finding the posterior
#'   mode. Options include `"nlminb"` (default), `"ucminf"`, and `"optim"`
#'   (BFGS).
#' @param numerical_grad Logical indicating whether to use numerical gradients
#'   for the optimisation. Defaults to `FALSE` to use analytical gradients.
#' @param cores Integer or `NULL`. Number of cores for parallel marginal
#'   fitting. When `NULL` (default), serial execution is used unless the number
#'   of free parameters exceeds 120, in which case parallelisation is enabled
#'   automatically using all available physical cores. Set to `1L` to force
#'   serial execution. If `cores > 1`, marginal fits are distributed across
#'   cores using [parallel::mclapply()] (fork-based; no parallelism on
#'   Windows).
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
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  sn_fit_ngrid = 21,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  use_itp = FALSE,
  cores = NULL,
  ...
) {
  start.time0 <- proc.time()[3]
  timing <- list(start.time = start.time0)

  ## ----- Check arguments -----------------------------------------------------
  if (!is.null(cores)) { # nocov start
    cores <- as.integer(cores)
    if (is.na(cores) || cores < 1L) cores <- 1L
  } # nocov end
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
  lavargs <- list(...)
  lavargs$model <- model
  lavargs$data <- data
  lavargs$ceq.simple <- TRUE # FIXME: Force ceq.simple rather than eq.constraints
  lavargs$verbose <- FALSE # FIXME: Need some quiet mode maybe
  lavargs$do.fit <- FALSE
  lavargs$parser <- "old" # To get priors parsed
  lavargs$test <- test

  if ("estimator" %in% names(lavargs)) {
    if (!(lavargs$estimator %in% c("ML", "PML"))) { # nocov
      cli_abort("Only 'ML' and 'PML' estimators are supported currently.")
    }
  }

  ## ----- Initialise lavaan object --------------------------------------------
  fit0 <- do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
  if (length(fit0@Data@ordered) > 0) {
    # Redo automatically with PML if ordinal data
    lavargs$estimator <- "PML"
    lavargs$parameterization <- "theta"
    lavargs$test <- "none"
    fit0 <- do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
  }
  lavmodel <- fit0@Model
  lavsamplestats <- fit0@SampleStats
  lavdata <- fit0@Data
  lavoptions <- fit0@Options
  lavpartable <- fit0@ParTable
  lavcache <- fit0@Cache
  n <- fit0@SampleStats@ntotal
  ceq.simple <- lavmodel@ceq.simple.only
  ceq.K <- lavmodel@ceq.simple.K # used to pack params/grads

  # ITP now supports analytical gradients via block Jacobians
  if (isTRUE(use_itp) && isTRUE(verbose)) {
    cli_inform("ITP parametrisation active.")
  }
  # ITP analytical gradient is incorrect for gamma1 and VB correction
  if (isTRUE(use_itp)) {
    marginal_correction <- "none"
    vb_correction <- FALSE
  }

  # Partable and check for equality constraints
  pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions,
                             use_itp = use_itp)
  PTFREEIDX <- which(pt$free > 0L)
  if (isTRUE(ceq.simple)) {
    # Note: Always work in the reduced space
    PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
  }
  m <- length(PTFREEIDX)
  parnames <- pt$names[PTFREEIDX]

  # Cache partable for prior logdens and grad
  prior_cache <- prepare_priors_for_optim(pt)

  ## ----- Prep work for approximation -----------------------------------------
  joint_lp <- function(pars) {
    if (isTRUE(ceq.simple)) {
      pars_unpacked <- as.numeric(ceq.K %*% pars)
      x <- pars_to_x(pars_unpacked, pt, compute_jac = FALSE)
    } else {
      x <- pars_to_x(pars, pt, compute_jac = FALSE)
    }
    ll <- inlav_model_loglik(
      x,
      lavmodel,
      lavsamplestats,
      lavdata,
      lavoptions,
      lavcache
    )
    pld <- 0
    if (isTRUE(add_priors)) {
      # Always take in packed version
      # pld <- prior_logdens(pars, pt)
      pld <- prior_logdens_vectorized(pars, prior_cache, debug = FALSE)
    }
    as.numeric(ll + pld)
  }

  joint_lp_grad <- function(pars) {
    # First, the likelihood gradient
    if (isTRUE(ceq.simple)) {
      pars_unpacked <- as.numeric(ceq.K %*% pars)
      x <- pars_to_x(pars_unpacked, pt)
      jcb_vec <- mapply(
        function(f, x) f(x),
        pt$ginv_prime[pt$free > 0],
        pars_unpacked
      )
    } else {
      x <- pars_to_x(pars, pt)
      jcb_vec <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
    }
    gll <- inlav_model_grad(x, lavmodel, lavsamplestats, lavdata, lavcache)

    # ITP Handling:
    # For ITP parameters, the gradient is a block operation.
    # We zero out their diagonal entries in jcb_vec and handle them separately.
    itp_blocks <- attr(x, "itp_blocks")
    itp_jacs <- attr(x, "itp_jacs")
    itp_free_ids <- integer(0)
    if (!is.null(itp_blocks)) {
      itp_free_ids <- unique(unlist(lapply(itp_blocks, function(b) pt$free[b$pt_cor_idx])))
      jcb_vec[itp_free_ids] <- 0
    }

    # Jacobian adjustment: d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ
    jcb <- diag(jcb_vec, length(jcb_vec))

    # This is the extra jacobian adjustment for covariances, since dx/dθ affects
    # more than one place if covariance exists
    sd1sd2 <- attr(x, "sd1sd2")
    jcb <- jcb * sd1sd2 # this adjusts the correlation parameters (diagonals)
    jcb_mat <- attr(x, "jcb_mat")

    if (!is.null(jcb_mat)) {
      for (k in seq_len(nrow(jcb_mat))) {
        i <- jcb_mat[k, 1]
        j <- jcb_mat[k, 2]
        # Only add if the source parameter i is NOT an ITP parameter
        if (!(i %in% itp_free_ids)) {
          jcb[i, j] <- jcb_mat[k, 3]
        }
      }
    }
    gll_th <- as.numeric(jcb %*% gll) # this adjusts the cov parameters, if any

    # Add ITP block contributions: gll_th[theta] += J_ITP^T %*% (sd1sd2 * gll[rho])
    if (!is.null(itp_blocks)) {
      for (blk_idx in seq_along(itp_blocks)) {
        blk <- itp_blocks[[blk_idx]]
        free_ids <- pt$free[blk$pt_cor_idx]
        # Gradient w.r.t. lavaan-scale x, scaled by sd1sd2 for cov reconstruction
        rho_grad <- gll[free_ids] * sd1sd2[free_ids]
        # The analytical Jacobian for this block
        J_blk <- itp_jacs[[as.character(blk_idx)]]
        # Gradient w.r.t. the block's theta parameters
        theta_grad <- as.numeric(t(J_blk) %*% rho_grad)
        # Add to the global gradient at the correct theta indices
        gll_th[free_ids] <- gll_th[free_ids] + theta_grad
      }
    }

    if (isTRUE(ceq.simple)) {
      gll_th <- as.numeric(gll_th %*% ceq.K)
    } # Repack

    # Next, the prior gradient
    glp_th <- 0
    if (isTRUE(add_priors)) {
      # Always take in packed version
      # glp_th <- prior_grad(pars, pt)
      glp_th <- prior_grad_vectorized(pars, prior_cache)
    }

    as.numeric(gll_th + glp_th)
  }

  timing <- add_timing(timing, "init")

  ## ----- Start optimisation --------------------------------------------------
  if (isTRUE(verbose)) {
    cli_progress_step("Finding posterior mode.")
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
      cli_progress_step("Computing the Hessian.")
    }
    if (isTRUE(numerical_grad)) {
      H_neg <- fast_hessian(ob, theta_star)
    } else {
      # H_neg <- numDeriv::jacobian(function(x) -1 * joint_lp_grad(x), theta_star)
      H_neg <- fast_jacobian(function(x) -1 * joint_lp_grad(x), theta_star)
    }
  } else if (optim_method == "ucminf") { # nocov start
    if (!requireNamespace("ucminf", quietly = TRUE)) {
      cli_abort(
        "The `ucminf` package is required for this optimization method. Please install it using `install.packages('ucminf')`."
      )
    }

    opt <- ucminf::ucminf(
      par = parstart,
      fn = ob,
      gr = gr,
      control = list(),
      hessian = 1
    )
    theta_star <- opt$par
    H_neg <- opt$hessian
  } else { # nocov end
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
  # Cholesky-factorise the precision (neg. Hessian), then derive covariance
  # via triangular backsolve. We first sort parameters into a canonical order
  # (by name) so results don't depend on the latent-variable ordering in the
  # model specification string.
  H_sym <- 0.5 * (H_neg + t(H_neg))
  canon_perm <- order(parnames)
  inv_perm <- order(canon_perm)
  H_canon <- H_sym[canon_perm, canon_perm]
  R_prec <- chol(H_canon) # upper Cholesky of canonical precision
  L_canon <- backsolve(R_prec, diag(m)) # L_c L_c^T = Sigma_canon (upper tri)
  L <- L_canon[inv_perm, ] # rows back to original param order
  Sigma_theta <- tcrossprod(L) # reconstruct covariance
  dimnames(Sigma_theta) <- list(parnames, parnames)
  lp_max <- joint_lp(theta_star) # before correction

  Vscan <- sweep(Sigma_theta, 2, sqrt(diag(Sigma_theta)), "/")

  # Derivatives at optima
  opt$dx <- fast_grad(function(x) -1 * joint_lp(x), theta_star) # fd grad
  opt$dx_analytic <- -1 * joint_lp_grad(theta_star)             # analytic grad
  if (isTRUE(debug)) {
    tab <- data.frame(
      analytic = round(opt$dx_analytic, 6),
      fd       = round(opt$dx, 6),
      diff     = round(opt$dx_analytic - opt$dx, 6),
      row.names = parnames
    )
    cli::cli_rule(left = "{.strong Gradient check at posterior mode}")
    print(tab)
    cli::cli_rule()
  }

  timing <- add_timing(timing, "optim")

  ## ----- VB correction -------------------------------------------------------
  vb_opt <- vb_shift <- vb_kld <- vb_kld_global <- n_qmc <- NA
  if (isTRUE(vb_correction)) {
    if (isTRUE(verbose)) {
      cli_progress_step(
        "Performing VB correction.",
        msg_done = "VB correction; mean |\U03B4| = {formatC(mean(abs(vb_shift) / sqrt(diag(Sigma_theta))),
                    format = 'f', digits = 3)}\U03C3."
      )
    }

    # QMC noise (scrambled Sobol); scale n with dimension
    n_qmc <- min(100L, max(30L, m + 20L))
    us <- sobol_owen(n = n_qmc, d = m)
    zs <- rbind(0, qnorm(us) %*% t(L)) # Add 0 to "lock at" mode

    vb_ob <- function(delta, mu0, Z) {
      mu_new <- mu0 + as.numeric(L %*% delta)
      ns <- nrow(Z)
      lp_total <- 0
      for (b in seq_len(ns)) {
        thetab <- mu_new + Z[b, , drop = TRUE]
        lp_total <- lp_total + joint_lp(thetab)
      }
      -1 * (lp_total / ns)
    }

    vb_gr <- function(eta, mu0, Z) {
      mu_new <- mu0 + as.numeric(L %*% eta)
      ns <- nrow(Z)
      lpgrad_total <- numeric(length(mu0))
      for (b in seq_len(ns)) {
        thetab <- mu_new + Z[b, , drop = TRUE]
        lpgrad_total <- lpgrad_total + joint_lp_grad(thetab)
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
    n_qmc = n_qmc,
    correction = vb_shift,
    kld = vb_kld,
    kld_global = vb_kld_global
  )
  timing <- add_timing(timing, "vb")

  ## ----- Info at optima ------------------------------------------------------
  theta_star_vbc <- theta_star
  if (isTRUE(vb_correction)) {
    theta_star_vbc <- theta_star + vb_shift
  }
  if (ceq.simple) {
    theta_star_trans <- pars_to_x(as.numeric(ceq.K %*% theta_star_vbc), pt,
                                  compute_jac = FALSE)
  } else {
    theta_star_trans <- pars_to_x(theta_star_vbc, pt, compute_jac = FALSE)
  }

  # Marginal log-likelihood (for BF comparison)
  # log det(Sigma) = -2 sum(log(diag(R_prec))) from the precision Cholesky
  mloglik <- lp_max + (m / 2) * log(2 * pi) - sum(log(diag(R_prec)))
  if (isTRUE(vb_correction)) {
    mloglik <- mloglik - vb_kld_global
  }
  timing <- add_timing(timing, "loglik")

  ## ----- Marginal approximations ---------------------------------------------
  if (isTRUE(verbose)) {
    cli_progress_done()
  }

  # pars_list <- setNames(as.list(1:m), paste0("pars[", 1:m, "]"))
  pars_list <- setNames(as.list(1:m), parnames)
  visual_debug <- NULL

  # When asymgaus or skewnorm marginals, we need the correction factor gamma1
  if (marginal_method %in% c("asymgaus", "skewnorm")) {
    # Step size for finite difference / central difference
    delta_outer <- 0.01 # for rate of change of Hessian (3rd deriv)
    delta_inner <- 0.001 # for rate of change of gradients (2nd deriv)

    get_gamma1 <- function(.j) {
      compute_gamma1j(
        j = .j,
        method = marginal_correction,
        theta_star = theta_star,
        Vscan = Vscan,
        L = L,
        joint_lp_grad = joint_lp_grad,
        delta_outer = delta_outer,
        delta_inner = delta_inner,
        m = m
      )
    }
  }

  if (marginal_method == "sampling") {
    approx_data <- NULL
  } else {
    # --- Resolve effective core count for marginal fitting ------------------
    if (is.null(cores)) {
      # Auto: serial for small m, parallel for large m
      if (m > 120L) {
        eff_cores <- parallel::detectCores(logical = FALSE)
        if (is.na(eff_cores) || eff_cores < 2L) eff_cores <- 1L
      } else {
        eff_cores <- 1L
      }
    } else {
      eff_cores <- cores
    }
    if (eff_cores > 1L && .Platform$OS.type == "windows") { # nocov start
      cli_alert_warning(
        "Parallel marginal fitting uses forking and is not available on
        Windows. Falling back to serial."
      )
      eff_cores <- 1L
    } # nocov end
    eff_cores <- min(eff_cores, m)

    if (marginal_method == "asymgaus") {
      obtain_approx_data <- function(j) {
        # Gauge the drop in joint_lp in whitened Z space
        k <- 2
        gamma1j <- get_gamma1(j)
        dplus <- max(
          0.01,
          lp_max - joint_lp(theta_star + Vscan[, j] * k) + gamma1j * k
        )
        dminus <- max(
          0.01,
          lp_max - joint_lp(theta_star - Vscan[, j] * k) + gamma1j * k
        )
        c(
          sigma_plus = sqrt(k^2 / (2 * dplus)),
          sigma_minus = sqrt(k^2 / (2 * dminus))
        )
      }

      approx_data <- run_parallel_or_serial(
        m = m,
        FUN = obtain_approx_data,
        cores = eff_cores,
        verbose = verbose,
        msg_serial = "Calibrating {j}/{m} asymmetric Gaussian{?s}.",
        msg_parallel = "Calibrating {done}/{m} asymmetric Gaussians ({cores}\U00D7)."
      )
      approx_data <- do.call(what = "rbind", approx_data)

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_asymgaus(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star_vbc,
          Sigma_theta = Sigma_theta,
          sigma_asym = approx_data
        )
      }
    } else if (marginal_method == "skewnorm") {
      obtain_approx_data <- function(j) {
        z <- seq(-4, 4, length = sn_fit_ngrid)
        yync <- yy <- numeric(length(z))
        gamma1j <- get_gamma1(j)

        for (k in seq_along(z)) {
          yync[k] <- joint_lp(theta_star + Vscan[, j] * z[k])
          yy[k] <- yync[k] + gamma1j * z[k]
        }

        fit_sn <- fit_skew_normal(
          x = z,
          y = yy - max(yy),
          threshold_log_drop = sn_fit_logthresh,
          temp = sn_fit_temp
        )

        vd <- data.frame(
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

        # Adjust back to theta space
        fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
        fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])

        list(fit = c(unlist(fit_sn), gamma1 = gamma1j), visual_debug = vd)
      }

      all_results <- run_parallel_or_serial(
        m = m,
        FUN = obtain_approx_data,
        cores = eff_cores,
        verbose = verbose,
        msg_serial = "Fitting {j}/{m} skew-normal marginal{?s}.",
        msg_parallel = "Fitting {done}/{m} skew-normal marginals ({cores}\U00D7)."
      )

      approx_data <- do.call(what = "rbind", lapply(all_results, `[[`, "fit"))
      rownames(approx_data) <- parnames
      visual_debug <- lapply(all_results, `[[`, "visual_debug")
      names(visual_debug) <- parnames

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_skewnorm(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star_vbc,
          Sigma_theta = Sigma_theta,
          sn_params = approx_data
        )
      }
    } else if (marginal_method == "marggaus") {
      approx_data <- NULL

      post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
        post_marg_marggaus(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star_vbc,
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
  timing <- add_timing(timing, "marginals")

  ## ----- NORTA adjustment for SN copula sampling ----------------------------
  R_star <- NULL
  if (marginal_method == "skewnorm" && isTRUE(samp_copula)) {
    if (isTRUE(verbose)) {
      cli_progress_step("Adjusting copula correlations (NORTA).")
    }
    R_star <- norta_adjust_R(cov2cor(Sigma_theta), approx_data)
  }
  timing <- add_timing(timing, "norta")

  ## ----- Draw posterior samples (once) ---------------------------------------
  has_extra_samp_work <-
    (sum(pt$free > 0 & grepl("cov", pt$mat)) > 0 &&
      marginal_method != "sampling") ||
    any(pt$op == ":=") ||
    any(pt$op == "~*~") ||
    test != "none"
  samp_env <- NULL
  if (isTRUE(verbose)) {
    samp_msg <- if (has_extra_samp_work) {
      "Posterior sampling and summarising."
    } else {
      "Drawing posterior samples."
    }
    samp_env <- environment()
    cli_progress_step(samp_msg, spinner = TRUE, .envir = samp_env)
  }
  samp <- sample_params(
    theta_star = theta_star_vbc,
    Sigma_theta = Sigma_theta,
    method = if (isTRUE(samp_copula)) marginal_method else "sampling",
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp,
    R_star = R_star
  )
  theta_samp <- samp$theta_samp
  x_samp <- samp$x_samp
  vcov_x <- cov(x_samp)
  dimnames(vcov_x) <- list(parnames, parnames)
  timing <- add_timing(timing, "sampling")

  if (marginal_method == "sampling") {
    postmargres <- post_marg_sampling(x_samp)
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
  summ <- cbind(summ, kld = vb$kld, vb_shift_sigma = vb$correction / sqrt(diag(Sigma_theta)))

  pdf_data <- lapply(postmargres, function(x) x$pdf_data)
  names(pdf_data) <- parnames

  summ <- as.data.frame(summ)
  summ$Prior <- pt$prior[PTFREEIDX]

  ## ----- Sampling for covariances and defined params -------------------------
  if (sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
    if (marginal_method == "sampling") {
      # Already covered by post_marg_sampling above
    } else {
      if (marginal_method == "skewnorm" && isTRUE(sn_fit_sample)) {
        samp_cov <- sample_covariances_fit_sn(x_samp, pt)
        sn_rows <- do.call(rbind, lapply(samp_cov, `[[`, "sn_params"))
        approx_data <- rbind(approx_data, sn_rows)
      } else {
        samp_cov <- sample_covariances(x_samp, pt)
      }

      for (cov_name in names(samp_cov)) {
        tmp_new_summ <- samp_cov[[cov_name]]$summary
        summ[cov_name, names(tmp_new_summ)] <- tmp_new_summ
        pdf_data[[cov_name]] <- samp_cov[[cov_name]]$pdf_data
      }
    }
  }
  timing <- add_timing(timing, "covariances")

  # ITP correlation parameters: recompute marginals from posterior samples.
  # post_marg uses ginv (identity for ITP), which reports raw ITP theta values
  # instead of actual correlations. For "psi_cov" ITP params, the covariance
  # section above already corrects this; here we handle "psi_cor" ITP params.
  if (length(attr(pt, "itp_blocks")) > 0) {
    itp_cor_pt_idx <- unlist(lapply(attr(pt, "itp_blocks"), `[[`, "pt_cor_idx"))
    is_cor <- grepl("_cor$", pt$mat[itp_cor_pt_idx])
    itp_cor_pt_idx <- itp_cor_pt_idx[is_cor]

    if (length(itp_cor_pt_idx) > 0) {
      itp_free_idx <- pt$free[itp_cor_pt_idx]
      itp_names <- pt$names[itp_cor_pt_idx]

      if (nsamp >= 2) {
        itp_samp <- x_samp[, itp_free_idx, drop = FALSE]
        colnames(itp_samp) <- itp_names
        itp_marg <- apply(itp_samp, 2, summarise_samples)

        for (nm in names(itp_marg)) {
          summ[nm, names(itp_marg[[nm]]$summary)] <- itp_marg[[nm]]$summary
          pdf_data[[nm]] <- itp_marg[[nm]]$pdf_data
        }
      } else {
        # With < 2 samples, use pars_to_x(theta_star) for the point estimate
        x_mode <- pars_to_x(theta_star_vbc, pt, compute_jac = FALSE)
        for (k in seq_along(itp_cor_pt_idx)) {
          nm <- itp_names[k]
          summ[nm, "Mean"] <- x_mode[itp_free_idx[k]]
          summ[nm, "Mode"] <- x_mode[itp_free_idx[k]]
          summ[nm, "50%"]  <- x_mode[itp_free_idx[k]]
        }
      }
    }
  }

  # Defined parameters
  if (any(pt$op == ":=")) {
    if (marginal_method == "skewnorm" && isTRUE(sn_fit_sample)) { # nocov start
      defpars <- get_defpars_fit_sn(x_samp, pt)
      sn_rows <- do.call(rbind, lapply(defpars, `[[`, "sn_params"))
      approx_data <- rbind(approx_data, sn_rows)
    } else { # nocov end
      defpars <- get_defpars(x_samp, pt)
    }

    for (def_name in names(defpars)) {
      tmp_new_summ <- defpars[[def_name]]$summary
      summ[def_name, names(tmp_new_summ)] <- tmp_new_summ
      pdf_data[[def_name]] <- defpars[[def_name]]$pdf_data
    }
  }
  timing <- add_timing(timing, "definedpars")

  # For binary and ordinal data, sample the deltas
  if (any(pt$op == "~*~")) {
    deltapars <- get_thetaparamerization_deltas(x_samp, lavmodel)
    names(deltapars) <- pt$names[which(pt$op == "~*~")]

    for (delta_name in names(deltapars)) {
      tmp_new_summ <- deltapars[[delta_name]]$summary
      summ[delta_name, names(tmp_new_summ)] <- tmp_new_summ
      pdf_data[[delta_name]] <- deltapars[[delta_name]]$pdf_data
    }
  }
  timing <- add_timing(timing, "deltapars")

  # Finalize coefficients from (possibly updated) summ
  coefs <- summ[, "Mean"]
  names(coefs) <- parnames

  ## ----- Compute ppp and dic -------------------------------------------------
  if (test != "none") {
    ppp <- get_ppp(
      x_samp = x_samp,
      lavmodel = lavmodel,
      lavsamplestats = lavsamplestats,
      lavdata = lavdata,
      lavpartable = lavpartable,
      cli_env = samp_env
    )
    dic_list <- get_dic(
      x_samp = x_samp,
      theta_star = theta_star_vbc,
      pt = pt,
      lavmodel = lavmodel,
      loglik = function(x) {
        inlav_model_loglik(
          x,
          lavmodel,
          lavsamplestats,
          lavdata,
          lavoptions,
          lavcache
        )
      },
      cli_env = samp_env
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
    theta_star_novbc = as.numeric(theta_star),
    theta_star = as.numeric(theta_star_vbc),
    Sigma_theta = Sigma_theta,
    R_star = R_star,
    vcov_x = vcov_x,
    theta_star_trans = theta_star_trans,
    approx_data = approx_data,
    nsamp = nsamp,
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
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  sn_fit_ngrid = 21,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  use_itp = FALSE,
  cores = NULL,
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
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  sn_fit_ngrid = 21,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  use_itp = FALSE,
  cores = NULL,
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
  dp = priors_for(),
  test = "standard",
  vb_correction = TRUE,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  sn_fit_ngrid = 21,
  sn_fit_logthresh = -6,
  sn_fit_temp = 1,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = TRUE,
  debug = FALSE,
  add_priors = TRUE,
  optim_method = c("nlminb", "ucminf", "optim"),
  numerical_grad = FALSE,
  use_itp = FALSE,
  cores = NULL,
  ...
) {
  sc <- sys.call()
  sc[["model.type"]] <- quote("growth")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}
