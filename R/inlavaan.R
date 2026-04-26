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
  use_gcp = FALSE,
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
  inline_prior_spec <- extract_inline_priors(lavargs$model)
  lavargs$model <- inline_prior_spec$model
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
  fit0 <- NULL
  init_method <- "lavaan_full"
  light_init <- if (inlavaan_force_r_path()) {
    NULL
  } else {
    native_lisrel_light_init(lavargs)
  }

  if (!is.null(light_init)) {
    lavmodel <- light_init$lavmodel
    lavsamplestats <- light_init$lavsamplestats
    lavdata <- light_init$lavdata
    lavoptions <- light_init$lavoptions
    lavpartable <- light_init$lavpartable
    lavcache <- light_init$lavcache
    native_backend <- light_init$native_backend
    if (!inlavaan_can_use_native_backend(lavdata, lavsamplestats)) {
      native_backend <- NULL
    }
    init_method <- "native_light"
  } else {
    fit0 <- with_safe_detectCores(
      do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
    )
    if (length(fit0@Data@ordered) > 0) {
      # Redo automatically with PML if ordinal data
      lavargs$estimator <- "PML"
      lavargs$parameterization <- "theta"
      lavargs$test <- "none"
      fit0 <- with_safe_detectCores(
        do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
      )
    }
    lavmodel <- fit0@Model
    lavsamplestats <- fit0@SampleStats
    lavdata <- fit0@Data
    lavoptions <- fit0@Options
    lavpartable <- fit0@ParTable
    lavcache <- fit0@Cache
    native_backend <- NULL
    if (native_lisrel_backend_supported(fit0) &&
        inlavaan_can_use_native_backend(lavdata, lavsamplestats)) {
      native_backend <- native_lisrel_backend_extract(fit0)
    }
  }
  n_levels <- lavdata@nlevels %||% 1L
  n <- lavsamplestats@ntotal
  ceq.simple <- lavmodel@ceq.simple.only
  ceq.K <- lavmodel@ceq.simple.K # used to pack params/grads

  # inlavaan_require_native_backend(native_backend, "Model fitting")

  if (isTRUE(use_gcp) && isTRUE(verbose)) {
    cli_alert_info("GCP parametrisation active.")
  }

  attr(lavpartable, "inlavaan_inline_priors") <- inline_prior_spec$priors

  # Partable and check for equality constraints
  pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions,
                             use_gcp = use_gcp)
  has_gcp <- length(attr(pt, "gcp_blocks")) > 0L
  PTFREEIDX <- which(pt$free > 0L)
  if (isTRUE(ceq.simple)) {
    # Note: Always work in the reduced space
    PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
  }
  m <- length(PTFREEIDX)
  parnames <- pt$names[PTFREEIDX]
  native_theta_transforms <- if (!is.null(native_backend)) {
    # Always use unique-param (m_r) transforms; the native backend handles
    # equality constraints implicitly via pt$free indices.
    native_lisrel_theta_transforms(pt, PTFREEIDX)
  } else {
    NULL
  }
  native_ptfree_expand_idx <- if (!is.null(native_backend) && isTRUE(ceq.simple)) {
    ptfree_all <- pt$free[pt$free > 0L]
    ptfree_unique <- ptfree_all[!duplicated(ptfree_all)]
    match(ptfree_all, ptfree_unique)
  } else {
    NULL
  }
  native_ptfree_expand_wt <- if (!is.null(native_backend) && isTRUE(ceq.simple)) {
    ptfree_all <- pt$free[pt$free > 0L]
    1 / as.numeric(table(ptfree_all)[as.character(ptfree_all)])
  } else {
    NULL
  }

  # Cache partable for prior logdens and grad
  prior_cache <- prepare_priors_for_optim(pt)
  if (!is.null(native_backend)) {
    .parstart_native <- pt$parstart[PTFREEIDX]
    .x_pre <- pars_to_x(.parstart_native, pt)
    .jcb_pre <- attr(.x_pre, "jcb_mat")
    cov_var_idx1_pre <- integer(m)
    cov_var_idx2_pre <- integer(m)
    if (!is.null(.jcb_pre) && nrow(.jcb_pre) > 0L) {
      for (.ci in unique(.jcb_pre[, 2L])) {
        .rows <- which(.jcb_pre[, 2L] == .ci)
        if (length(.rows) >= 2L) {
          cov_var_idx1_pre[.ci] <- .jcb_pre[.rows[1L], 1L]
          cov_var_idx2_pre[.ci] <- .jcb_pre[.rows[2L], 1L]
        }
      }
    }
    native_backend$cov_var_idx1 <- cov_var_idx1_pre
    native_backend$cov_var_idx2 <- cov_var_idx2_pre
    native_backend$gcp_blocks <- gcp_native_blocks_from_pt(pt)
    native_backend$prior_cpp <- list(
      free_id0 = as.integer(prior_cache$free_id - 1L),
      trans_type = as.integer(prior_cache$trans_type),
      prior_type = as.integer(prior_cache$prior_type),
      p1 = as.numeric(prior_cache$p1),
      p2 = as.numeric(prior_cache$p2),
      is_sd_prior = as.logical(prior_cache$is_sd_prior),
      is_prec_prior = as.logical(prior_cache$is_prec_prior)
    )
  }
  native_backend_eval <- native_backend

  ## ----- Prep work for approximation -----------------------------------------
  # Cache for native backend: avoid recomputing loglik+grad at the same x.
  .nb_env <- if (!is.null(native_backend_eval)) {
    e <- new.env(parent = emptyenv())
    e$x    <- NULL
    e$ll   <- NULL
    e$grad <- NULL
    e$diagnostics <- NULL
    e
  } else {
    NULL
  }

  .nb_compute_cached <- function(x_plain) {
    if (!identical(x_plain, .nb_env$x)) {
      res <- cpp_lisrel_loglik_and_grad(native_backend_eval, x_plain)
      .nb_env$x  <- x_plain
      .nb_env$ll <- res[["loglik"]]
      g <- res[["grad"]]
      if (anyNA(g) || any(!is.finite(g))) {
        g <- cpp_fast_grad(
          function(z) cpp_lisrel_loglik(native_backend_eval, z),
          x_plain
        )
      }
      .nb_env$grad <- g
      .nb_env$diagnostics <- res[["diagnostics"]] %||% NULL
    }
    invisible(NULL)
  }

  .r_loglik_grad <- function(x_plain) {
    gll_r <- inlav_model_grad(x_plain, lavmodel, lavsamplestats, lavdata, lavcache)
    if (anyNA(gll_r) || any(!is.finite(gll_r))) {
      gll_r <- fast_grad(
        function(z) model_loglik_dispatch(
          z,
          lavmodel,
          lavsamplestats,
          lavdata,
          lavoptions,
          lavcache
        ),
        x_plain
      )
    }
    gll_r
  }

  joint_lp <- function(pars) {
    if (anyNA(pars) || any(!is.finite(pars))) {
      return(-1e40)
    }
    if (isTRUE(ceq.simple)) {
      pars_unpacked <- as.numeric(ceq.K %*% pars)
      x <- pars_to_x(pars_unpacked, pt, compute_jac = FALSE)
    } else {
      x <- pars_to_x(pars, pt, compute_jac = FALSE)
    }
    ll <- if (!is.null(native_backend)) {
      .nb_compute_cached(as.numeric(x))
      .nb_env$ll
    } else {
      model_loglik_dispatch(
        x,
        lavmodel,
        lavsamplestats,
        lavdata,
        lavoptions,
        lavcache
      )
    }
    pld <- 0
    if (isTRUE(add_priors)) {
      # Always take in packed version
      # pld <- prior_logdens(pars, pt)
      pld <- prior_logdens_vectorized(pars, prior_cache, debug = FALSE)
    }
    as.numeric(ll + pld)
  }

  joint_lp_grad <- function(pars) {
    if (anyNA(pars) || any(!is.finite(pars))) {
      return(rep(0, length(pars)))
    }
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
    gll <- if (!is.null(native_backend)) {
      .nb_compute_cached(as.numeric(x))
      gll_native <- .nb_env$grad
      if (isTRUE(ceq.simple)) {
        gll_native[native_ptfree_expand_idx] * native_ptfree_expand_wt
      } else {
        gll_native
      }
    } else {
      .r_loglik_grad(x)
    }

    # GCP Handling:
    # For GCP parameters, the gradient is a block operation.
    # We zero out their diagonal entries in jcb_vec and handle them separately.
    gcp_blocks <- attr(x, "gcp_blocks")
    gcp_jacs <- attr(x, "gcp_jacs")
    gcp_free_ids <- integer(0)
    if (!is.null(gcp_blocks)) {
      gcp_free_ids <- unique(unlist(lapply(gcp_blocks, function(b) pt$free[b$pt_cor_idx])))
      jcb_vec[gcp_free_ids] <- 0
    }

    # Jacobian adjustment: d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ
    # Sparse chain rule: diagonal jcb_vec*sd1sd2 + off-diagonal covariance
    # corrections from jcb_mat.  Avoids O(m^2) dense matrix allocation.
    sd1sd2 <- attr(x, "sd1sd2")
    jcb_mat <- attr(x, "jcb_mat")
    gll_th <- jcb_vec * sd1sd2 * gll
    if (!is.null(jcb_mat) && NROW(jcb_mat) > 0) {
      for (k in seq_len(nrow(jcb_mat))) {
        i <- jcb_mat[k, 1]
        j <- jcb_mat[k, 2]
        if (!(i %in% gcp_free_ids)) {
          gll_th[i] <- gll_th[i] + jcb_mat[k, 3] * gll[j]
        }
      }
    }

    # Add GCP block contributions: gll_th[theta] += J_GCP^T %*% (sd1sd2 * gll[rho])
    if (!is.null(gcp_blocks)) {
      for (blk_idx in seq_along(gcp_blocks)) {
        blk <- gcp_blocks[[blk_idx]]
        free_ids <- pt$free[blk$pt_cor_idx]
        # Gradient w.r.t. lavaan-scale x, scaled by sd1sd2 for cov reconstruction
        rho_grad <- gll[free_ids] * sd1sd2[free_ids]
        # The analytical Jacobian for this block
        J_blk <- gcp_jacs[[as.character(blk_idx)]]
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

  .joint_lp_grad_safe <- function(pars) {
    grad <- joint_lp_grad(pars)
    if (anyNA(grad) || any(!is.finite(grad))) {
      grad <- fast_grad(joint_lp, pars)
    }
    grad
  }

  .hessian_is_finite <- function(H) {
    !is.null(H) && is.matrix(H) && !anyNA(H) && all(is.finite(H))
  }

  joint_lp_grad_vectorised <- function(pars_mat) {
    # Batch gradient of the joint log-posterior across N parameter vectors.
    # Amortizes the chain-rule Jacobian by computing it once at the first
    # column (reference point).  Valid when all columns are small perturbations
    # of each other, as in the finite-difference loops of compute_gamma1j.
    #
    # pars_mat : m x N (packed if ceq.simple, full otherwise)
    # Returns  : m x N gradient matrix
    N <- ncol(pars_mat)

    # ---- 1. Chain-rule Jacobian at the reference (first) column -------------
    ref <- pars_mat[, 1L]
    if (isTRUE(ceq.simple)) {
      ref_unpacked <- as.numeric(ceq.K %*% ref)
      x_ref        <- pars_to_x(ref_unpacked, pt)
      jcb_vec_ref  <- mapply(function(f, x) f(x),
                             pt$ginv_prime[pt$free > 0], ref_unpacked)
    } else {
      x_ref        <- pars_to_x(ref, pt)
      jcb_vec_ref  <- mapply(function(f, x) f(x),
                             pt$ginv_prime[pt$free > 0], ref)
    }
    m_full       <- length(jcb_vec_ref)
    sd1sd2_ref   <- attr(x_ref, "sd1sd2")
    jcb_mat_ref  <- attr(x_ref, "jcb_mat")
    gcp_blocks   <- attr(x_ref, "gcp_blocks")
    gcp_jacs_ref <- attr(x_ref, "gcp_jacs")

    gcp_free_ids <- integer(0)
    if (!is.null(gcp_blocks) && length(gcp_blocks) > 0L) {
      gcp_free_ids <- unique(unlist(
        lapply(gcp_blocks, function(b) pt$free[b$pt_cor_idx])
      ))
      jcb_vec_ref[gcp_free_ids] <- 0
    }

    # Build full-space chain-rule matrix J (m_full x m_full):
    #   J = diag(jcb_vec * sd1sd2) + off-diagonal covariance corrections
    J <- diag(jcb_vec_ref * sd1sd2_ref, m_full)
    if (!is.null(jcb_mat_ref) && nrow(jcb_mat_ref) > 0L) {
      for (k in seq_len(nrow(jcb_mat_ref))) {
        ii <- jcb_mat_ref[k, 1L]
        jj <- jcb_mat_ref[k, 2L]
        if (!(ii %in% gcp_free_ids)) J[ii, jj] <- jcb_mat_ref[k, 3L]
      }
    }

    # ---- 2. Likelihood gradient at every column (skip Jacobian recompute) ---
    gll_mat <- matrix(0, nrow = m_full, ncol = N)
    for (i in seq_len(N)) {
      pars_i <- pars_mat[, i]
      if (anyNA(pars_i) || any(!is.finite(pars_i))) {
        next
      }
      if (isTRUE(ceq.simple)) {
        x_i <- pars_to_x(as.numeric(ceq.K %*% pars_i), pt, compute_jac = FALSE)
      } else {
        x_i <- pars_to_x(pars_i, pt, compute_jac = FALSE)
      }
      gll_mat[, i] <- if (!is.null(native_backend)) {
        .nb_compute_cached(as.numeric(x_i))
        gll_native <- .nb_env$grad
        if (isTRUE(ceq.simple)) {
          gll_native[native_ptfree_expand_idx] * native_ptfree_expand_wt
        } else {
          gll_native
        }
      } else {
        .r_loglik_grad(x_i)
      }
    }

    # ---- 3. Apply chain rule ------------------------------------------------
    gll_th_mat <- J %*% gll_mat   # m_full x N

    # ---- 4. GCP block contributions (reuse reference Jacobians) -------------
    if (!is.null(gcp_blocks) && length(gcp_blocks) > 0L) {
      for (blk_idx in seq_along(gcp_blocks)) {
        blk   <- gcp_blocks[[blk_idx]]
        fids  <- pt$free[blk$pt_cor_idx]
        J_blk <- gcp_jacs_ref[[as.character(blk_idx)]]
        # t(J_blk) %*% diag(sd1sd2[fids]) %*% gll_mat[fids, ]
        rho_gll <- sd1sd2_ref[fids] * gll_mat[fids, , drop = FALSE]
        gll_th_mat[fids, ] <- gll_th_mat[fids, ] + t(J_blk) %*% rho_gll
      }
    }

    # ---- 5. Repack to packed space when ceq.simple --------------------------
    # Scalar version: as.numeric(gll_th %*% ceq.K) = t(ceq.K) %*% gll_th
    if (isTRUE(ceq.simple)) {
      gll_th_mat <- t(ceq.K) %*% gll_th_mat  # m_packed x N
    }

    # ---- 6. Prior gradients -------------------------------------------------
    if (isTRUE(add_priors)) {
      for (i in seq_len(N)) {
        gll_th_mat[, i] <- gll_th_mat[, i] +
          prior_grad_vectorized(pars_mat[, i], prior_cache)
      }
    }

    gll_th_mat
  }

  timing <- add_timing(timing, "init")

  ## ----- Start optimisation --------------------------------------------------
  if (isTRUE(verbose)) {
    cli_progress_step("Finding posterior mode.")
  }

  ob <- function(x) -1 * joint_lp(x)
  gr <- if (isTRUE(numerical_grad)) NULL else function(x) -1 * .joint_lp_grad_safe(x)
  parstart <- pt$parstart[PTFREEIDX]

  max_abs_or_inf <- function(x) {
    xf <- x[is.finite(x)]
    if (length(xf) == 0L) {
      return(Inf)
    }
    max(abs(xf))
  }

  # For the native backend without priors or equality constraints, the main
  # optimisation runs entirely in C++ (no R calls in the hot loop):
  # L-BFGS + analytic gradient + Hessian.
  # ceq.simple packs parameters via ceq.K; the C++ optimizer expects the full
  # unpacked x_free, so we fall back to the R path when equality constraints
  # are present.
  .use_cpp_opt <- !is.null(native_backend) &&
    optim_method %in% c("nlminb", "ucminf")

  if (.use_cpp_opt) {
    if (isTRUE(verbose)) cli_progress_step("Computing the Hessian.")

    cpp_res <- cpp_lisrel_optimize(
      native_backend, parstart, native_theta_transforms,
      max_iter  = as.integer(control$iter.max %||% 1000L),
      grad_tol  = as.numeric(control$grad.tol %||% 1e-5),
      m_bfgs    = 10L,
      max_ls    = 40L,
      h         = as.numeric(control$hessian.step %||% 1e-5)
    )
    theta_star <- cpp_res$par
    H_neg      <- cpp_res$hessian
    opt <- list(
      par        = theta_star,
      objective  = -cpp_res$loglik,
      converged  = cpp_res$converged,
      iterations = cpp_res$n_iter,
      dx         = -cpp_res$grad,       # analytic grad of -loglik at optimum
      dx_analytic = -cpp_res$grad
    )
  } else if (optim_method == "nlminb") {
    opt <- nlminb(
      start = parstart,
      objective = ob,
      gradient = gr,
      control = control
    )
    opt_dx <- if (is.null(gr)) fast_grad(ob, opt$par) else gr(opt$par)
    need_retry <- !isTRUE(opt$convergence == 0L) || max_abs_or_inf(opt_dx) > 1e-3
    if (need_retry) {
      if (isTRUE(verbose)) {
        cli_alert_warning(
          "Primary optimiser stopped early; retrying with BFGS polishing."
        )
      }
      opt_retry <- stats::optim(
        par = opt$par,
        fn = ob,
        gr = gr,
        method = "BFGS",
        control = list(
          maxit = max(200L, as.integer(control$iter.max %||% 1000L)),
          reltol = 1e-10
        )
      )
      opt_retry_dx <- if (is.null(gr)) fast_grad(ob, opt_retry$par) else gr(opt_retry$par)
      retry_better <- isTRUE(opt_retry$convergence == 0L) &&
        (
          !isTRUE(opt$convergence == 0L) ||
          max_abs_or_inf(opt_retry_dx) < max_abs_or_inf(opt_dx) ||
          opt_retry$value <= opt$objective + 1e-8
        )
      if (retry_better) {
        opt <- list(
          par = opt_retry$par,
          objective = opt_retry$value,
          convergence = 0L,
          iterations = opt_retry$counts[["function"]] %||% NA_integer_,
          evaluations = opt_retry$counts,
          message = "BFGS polishing after nlminb false convergence",
          value = opt_retry$value
        )
      }
    }
    theta_star <- opt$par
    if (isTRUE(verbose)) {
      cli_progress_step("Computing the Hessian.")
    }
    if (!is.null(native_backend)) {
      # Compute covariance-param Jacobian indices so the Hessian uses the
      # same parameterisation as joint_lp_grad (sd1*sd2 correction for PSI cov).
      .hs_x <- pars_to_x(theta_star, pt)
      .hs_jcb <- attr(.hs_x, "jcb_mat")
      .cov_var_idx1_hs <- integer(m)
      .cov_var_idx2_hs <- integer(m)
      if (!is.null(.hs_jcb) && nrow(.hs_jcb) > 0L) {
        for (.ci in unique(.hs_jcb[, 2L])) {
          .rows <- which(.hs_jcb[, 2L] == .ci)
          if (length(.rows) >= 2L) {
            .cov_var_idx1_hs[.ci] <- .hs_jcb[.rows[1L], 1L]
            .cov_var_idx2_hs[.ci] <- .hs_jcb[.rows[2L], 1L]
          }
        }
      }
      H_neg <- cpp_lisrel_hessian_theta(
        native_backend,
        theta_star,
        native_theta_transforms,
        .cov_var_idx1_hs,
        .cov_var_idx2_hs
      )
      if (isTRUE(add_priors)) {
        H_neg <- H_neg - fast_jacobian(
          function(x) prior_grad_vectorized(x, prior_cache),
          theta_star
        )
      }
    } else if (isTRUE(numerical_grad)) {
      H_neg <- fast_hessian(ob, theta_star)
    } else {
      H_neg <- fast_jacobian(function(x) -1 * .joint_lp_grad_safe(x), theta_star)
      if (anyNA(H_neg) || any(!is.finite(H_neg))) {
        H_neg <- fast_hessian(ob, theta_star)
      }
    }
    if (!.hessian_is_finite(H_neg)) {
      H_neg <- stats::optimHess(theta_star, ob, gr)
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
  if (!.hessian_is_finite(H_sym)) {
    finite_diag <- diag(H_sym)[is.finite(diag(H_sym))]
    diag_fill <- if (length(finite_diag) > 0L) {
      max(stats::median(abs(finite_diag)), 1)
    } else {
      1
    }
    H_sym[!is.finite(H_sym)] <- 0
    bad_diag <- !is.finite(diag(H_sym)) | diag(H_sym) <= 0
    diag(H_sym)[bad_diag] <- diag_fill
    H_sym <- 0.5 * (H_sym + t(H_sym))
  }
  canon_perm <- order(parnames)
  inv_perm <- order(canon_perm)
  H_canon <- H_sym[canon_perm, canon_perm]
  R_prec <- tryCatch(
    chol(H_canon),
    error = function(e) {
      if (isTRUE(verbose)) {
        cli_alert_warning(
          "Estimated precision was not positive definite; applying eigenvalue clamping."
        )
      }
      chol(make_pd(H_canon))
    }
  ) # upper Cholesky of canonical precision
  L_canon <- backsolve(R_prec, diag(m)) # L_c L_c^T = Sigma_canon (upper tri)
  L <- L_canon[inv_perm, ] # rows back to original param order
  Sigma_theta <- tcrossprod(L) # reconstruct covariance
  dimnames(Sigma_theta) <- list(parnames, parnames)
  lp_max <- joint_lp(theta_star) # before correction

  Vscan <- sweep(Sigma_theta, 2, sqrt(diag(Sigma_theta)), "/")

  # Derivatives at optimum (diagnostic)
  if (isTRUE(.use_cpp_opt)) {
    # C++ optimizer already computed the analytic gradient; reuse it.
    opt$dx         <- opt$dx_analytic   # already set by cpp_lisrel_optimize
    opt$dx_fd      <- opt$dx_analytic
  } else if (!is.null(native_backend)) {
    # Native backend available: analytic gradient is exact, skip FD.
    opt$dx_analytic        <- -1 * .joint_lp_grad_safe(theta_star)
    opt$dx_native_analytic <- opt$dx_analytic
    opt$dx_fd              <- opt$dx_analytic
    opt$dx                 <- opt$dx_analytic
  } else {
    opt$dx         <- fast_grad(function(x) -1 * joint_lp(x), theta_star)
    opt$dx_analytic <- -1 * .joint_lp_grad_safe(theta_star)
  }
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

  ## ----- Pre-compute covariance jacobian indices (needed by VB + marginals) --
  if (!is.null(native_backend)) {
    .ts_pre <- pars_to_x(theta_star, pt)
    .jcb_pre <- attr(.ts_pre, "jcb_mat")
    if (!is.null(.jcb_pre) && nrow(.jcb_pre) > 0) {
      .cov_th <- unique(.jcb_pre[, 2L])
      cov_var_idx1 <- integer(m)
      cov_var_idx2 <- integer(m)
      for (.ci in .cov_th) {
        .rows <- which(.jcb_pre[, 2L] == .ci)
        if (length(.rows) >= 2L) {
          cov_var_idx1[.ci] <- .jcb_pre[.rows[1L], 1L]
          cov_var_idx2[.ci] <- .jcb_pre[.rows[2L], 1L]
        }
      }
    } else {
      cov_var_idx1 <- integer(m)
      cov_var_idx2 <- integer(m)
    }
  }

  ## ----- Resolve effective core count (needed by VB + marginals) ------------
  if (is.null(cores)) {
    if (m > 20L) {
      eff_cores <- parallel::detectCores(logical = FALSE)
      if (is.na(eff_cores) || eff_cores < 2L) eff_cores <- 1L
    } else {
      eff_cores <- 1L
    }
  } else {
    eff_cores <- cores
  }
  if (eff_cores > 1L && .Platform$OS.type == "windows") { # nocov start
    eff_cores <- 1L
  } # nocov end
  eff_cores <- min(eff_cores, m)

  if (!is.null(native_backend) && isTRUE(add_priors)) {
    .cpp_prior_fid    <- as.integer(prior_cache$free_id - 1L)
    .cpp_prior_trans  <- as.integer(prior_cache$trans_type)
    .cpp_prior_type   <- as.integer(prior_cache$prior_type)
    .cpp_prior_p1     <- prior_cache$p1
    .cpp_prior_p2     <- prior_cache$p2
    .cpp_prior_is_sd  <- prior_cache$is_sd_prior
    .cpp_prior_is_prec <- if (!is.null(prior_cache$is_prec_prior))
      prior_cache$is_prec_prior else rep(FALSE, length(prior_cache$p1))
  } else {
    .cpp_prior_fid <- .cpp_prior_trans <- .cpp_prior_type <- integer(0)
    .cpp_prior_p1 <- .cpp_prior_p2 <- numeric(0)
    .cpp_prior_is_sd <- .cpp_prior_is_prec <- logical(0)
  }

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

    # Prepare C++ prior params (0-based indices; empty if no priors)
    if (!is.null(native_backend) && !is.null(native_theta_transforms)) {
      # C++ batch VB: compute f and g together, cache last result to avoid
      # computing twice per nlminb iteration.
      # For ceq.simple, pass ceq_K so C++ expands theta_r → theta_full internally.
      .vb_ceq_K <- matrix(0, 0, 0)  # always m_r space; backend handles eq constraints
      .vb_transforms <- native_theta_transforms
      .vb_cvi1 <- cov_var_idx1
      .vb_cvi2 <- cov_var_idx2
      .vb_cache <- new.env(parent = emptyenv())
      .vb_cache$delta <- NULL
      .vb_cache$res <- NULL
      .vb_call_cpp <- function(delta) {
        if (!identical(delta, .vb_cache$delta)) {
          .vb_cache$delta <- delta
          .vb_cache$res <- cpp_vb_fg(
            delta, theta_star, L, zs, native_backend,
            .vb_transforms, .vb_cvi1, .vb_cvi2,
            .cpp_prior_fid, .cpp_prior_trans, .cpp_prior_type,
            .cpp_prior_p1, .cpp_prior_p2, .cpp_prior_is_sd, .cpp_prior_is_prec,
            .vb_ceq_K,
            as.integer(eff_cores)
          )
        }
        .vb_cache$res
      }
      vb_ob <- function(delta, mu0, Z) .vb_call_cpp(delta)$f
      vb_gr <- function(delta, mu0, Z) .vb_call_cpp(delta)$g
    } else {
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
    }

    vb_opt <- suppressWarnings(
      tryCatch(
        nlminb(
          start = rep(0, m),
          objective = vb_ob,
          gradient = vb_gr,
          mu0 = theta_star,
          Z = zs,
          control = list(rel.tol = 1e-4)
        ),
        error = function(e) structure(list(error = conditionMessage(e)), class = "inlavaan_vb_error")
      )
    )

    if (inherits(vb_opt, "inlavaan_vb_error") ||
        anyNA(vb_opt$par) ||
        any(!is.finite(vb_opt$par)) ||
        !is.finite(vb_opt$objective)) {
      if (isTRUE(verbose)) {
        cli_alert_warning("VB correction failed; continuing without VB correction.")
      }
      vb_opt <- vb_shift <- vb_kld <- vb_kld_global <- NA
    } else {
      vb_shift <- as.numeric(L %*% vb_opt$par)
      vb_kld <- (vb_shift)^2 / (2 * diag(Sigma_theta))
      vb_kld_global <- lp_max + vb_opt$objective
    }
  } 

  vb <- list(
    opt = vb_opt,
    n_qmc = n_qmc,
    correction = vb_shift,
    kld = vb_kld,
    kld_global = vb_kld_global
  )
  timing <- add_timing(timing, "vb")

  mloglik <- lp_max + (m / 2) * log(2 * pi) - sum(log(diag(R_prec)))
  if (isTRUE(vb_correction) &&
      is.finite(vb_kld_global)) {
    mloglik <- mloglik - vb_kld_global
  }

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
    # eff_cores already computed above (before VB section)
    .use_cpp_marginals <- !is.null(native_backend) &&
      marginal_correction == "shortcut" &&
      !isTRUE(getOption("inlavaan.force_r_marginals", FALSE))

    # All paths (including ceq.simple) use m_r space: the native backend
    # handles equality constraints implicitly via pt$free indices.
    if (.use_cpp_marginals) {
      .theta_star_cpp   <- theta_star
      .Vscan_cpp  <- Vscan
      .L_cpp      <- L
      .tr_cpp     <- native_theta_transforms
      .cvi1_cpp   <- cov_var_idx1
      .cvi2_cpp   <- cov_var_idx2
      .theta_star_prior <- theta_star
      .Vscan_prior      <- Vscan
    }

    if (!exists("cov_var_idx1", inherits = FALSE)) {
      cov_var_idx1 <- integer(m)
      cov_var_idx2 <- integer(m)
    }

    .H_prior_neg <- if (isTRUE(add_priors)) {
      fast_jacobian(
        function(x) -prior_grad_vectorized(x, prior_cache),
        theta_star
      )
    } else {
      matrix(0, m, m)
    }
    # H_prior_neg is always m_r × m_r; no expansion needed
    .H_prior_neg_cpp <- .H_prior_neg

    if (marginal_method == "asymgaus") {
      if (.use_cpp_marginals) {
        if (isTRUE(verbose)) {
          cli_progress_step("Computing asymmetric-Gaussian marginals in C++ (batch scan, {eff_cores}\U00D7).")
        }
        z_ag <- c(-2, 2)
        batch_res <- cpp_marginals_batch(
          model        = native_backend,
          theta_star   = .theta_star_cpp,
          Vscan        = .Vscan_cpp,
          L_chol       = .L_cpp,
          transforms   = .tr_cpp,
          z_grid       = z_ag,
          cov_var_idx1 = .cvi1_cpp,
          cov_var_idx2 = .cvi2_cpp,
          H_prior_neg  = .H_prior_neg_cpp,
          delta_outer  = delta_outer,
          h_inner      = 1e-5,
          nthreads     = as.integer(eff_cores)
        )
        loglik_scan_mat <- batch_res$loglik_scan
        gamma1_cpp <- batch_res$gamma1
        if (isTRUE(add_priors) && !has_gcp) {
          prior_scan <- cpp_prior_logdens_scan(
            .theta_star_prior, .Vscan_prior, z_ag,
            .cpp_prior_fid, .cpp_prior_trans, .cpp_prior_type,
            .cpp_prior_p1, .cpp_prior_p2, .cpp_prior_is_sd, .cpp_prior_is_prec,
            as.integer(eff_cores)
          )
          loglik_scan_mat <- loglik_scan_mat + prior_scan
        }
        loglik_scan_mat[!is.finite(loglik_scan_mat)] <- -1e40
        approx_data <- t(vapply(seq_len(m), function(j) {
          gamma1j <- gamma1_cpp[j]
          dminus <- max(0.01, lp_max - loglik_scan_mat[j, 1] + gamma1j * 2)
          dplus  <- max(0.01, lp_max - loglik_scan_mat[j, 2] + gamma1j * 2)
          c(
            sigma_plus = sqrt(4 / (2 * dplus)),
            sigma_minus = sqrt(4 / (2 * dminus))
          )
        }, numeric(2)))
      } else {
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
      }

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
      # Fast path: C++ batch scan + gamma1, optionally adding prior contributions in R.
      # Set option inlavaan.force_r_marginals=TRUE to force R path (for benchmarking).
      if (.use_cpp_marginals) {
        if (isTRUE(verbose)) {
          cli_progress_step("Computing marginals in C++ (batch scan, {eff_cores}\U00D7).")
        }
        z <- seq(-4, 4, length.out = sn_fit_ngrid)

        batch_res <- cpp_marginals_batch(
          model        = native_backend,
          theta_star   = .theta_star_cpp,
          Vscan        = .Vscan_cpp,
          L_chol       = .L_cpp,
          transforms   = .tr_cpp,
          z_grid       = z,
          cov_var_idx1 = .cvi1_cpp,
          cov_var_idx2 = .cvi2_cpp,
          H_prior_neg  = .H_prior_neg_cpp,
          delta_outer  = delta_outer,
          h_inner      = 1e-5,
          nthreads     = as.integer(eff_cores)
        )
        loglik_scan_mat <- batch_res$loglik_scan  # m x K (loglik only)
        gamma1_cpp      <- batch_res$gamma1        # m-vector (includes prior correction)

        # Add exact prior log-density to the scan (C++ only computed loglik part)
        if (isTRUE(add_priors) && !has_gcp) {
          prior_scan <- cpp_prior_logdens_scan(
            .theta_star_prior, .Vscan_prior, z,
            .cpp_prior_fid, .cpp_prior_trans, .cpp_prior_type,
            .cpp_prior_p1, .cpp_prior_p2, .cpp_prior_is_sd, .cpp_prior_is_prec,
            as.integer(eff_cores)
          )
          loglik_scan_mat <- loglik_scan_mat + prior_scan
        }
        loglik_scan_mat[!is.finite(loglik_scan_mat)] <- -1e40

        fit_one_sn <- function(j) {
          gamma1j <- gamma1_cpp[j]
          yync    <- loglik_scan_mat[j, ]
          yy      <- yync + gamma1j * z

          fit_sn <- fit_skew_normal(
            x = z, y = yy - max(yy),
            threshold_log_drop = sn_fit_logthresh,
            temp = sn_fit_temp
          )

          vd <- data.frame(
            x         = z,
            Original  = exp(yync - max(yync)),
            Corrected = exp(yy - max(yy)),
            SN_Fit    = dsnorm(z, fit_sn$xi, fit_sn$omega,
                               fit_sn$alpha, fit_sn$logC)
          )

          fit_sn$xi    <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
          fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])

          list(fit = c(unlist(fit_sn), gamma1 = gamma1j), visual_debug = vd)
        }
        all_results <- run_parallel_or_serial(
          m = m,
          FUN = fit_one_sn,
          cores = eff_cores,
          verbose = verbose,
          msg_serial = "Fitting {j}/{m} skew-normal marginal{?s}.",
          msg_parallel = "Fitting {done}/{m} skew-normal marginals ({cores}\U00D7)."
        )
      } else {
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
      }

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
          theta_star = theta_star,
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
    theta_star  = theta_star,
    Sigma_theta = Sigma_theta,
    method      = if (isTRUE(samp_copula)) marginal_method else "sampling",
    approx_data = approx_data,
    pt          = pt,
    lavmodel    = lavmodel,
    nsamp       = nsamp,
    R_star      = R_star,
    integration_data = inla_integration,
    native_theta_transforms = native_theta_transforms,
    cov_var_idx1 = cov_var_idx1,
    cov_var_idx2 = cov_var_idx2,
    nthreads    = as.integer(eff_cores)
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

  # GCP correlation parameters: recompute marginals from posterior samples.
  # post_marg uses ginv (identity for GCP), which reports raw GCP theta values
  # instead of actual correlations. For "psi_cov" GCP params, the covariance
  # section above already corrects this; here we handle "psi_cor" GCP params.
  if (length(attr(pt, "gcp_blocks")) > 0) {
    gcp_cor_pt_idx <- unlist(lapply(attr(pt, "gcp_blocks"), `[[`, "pt_cor_idx"))
    is_cor <- grepl("_cor$", pt$mat[gcp_cor_pt_idx])
    gcp_cor_pt_idx <- gcp_cor_pt_idx[is_cor]

    if (length(gcp_cor_pt_idx) > 0) {
      gcp_free_idx <- pt$free[gcp_cor_pt_idx]
      gcp_names <- pt$names[gcp_cor_pt_idx]

      if (nsamp >= 2) {
        gcp_samp <- x_samp[, gcp_free_idx, drop = FALSE]
        colnames(gcp_samp) <- gcp_names
        gcp_marg <- apply(gcp_samp, 2, summarise_samples)

        for (nm in names(gcp_marg)) {
          summ[nm, names(gcp_marg[[nm]]$summary)] <- gcp_marg[[nm]]$summary
          pdf_data[[nm]] <- gcp_marg[[nm]]$pdf_data
        }
      } else {
        # With < 2 samples, use pars_to_x(theta_star) for the point estimate
        x_mode <- pars_to_x(theta_star, pt, compute_jac = FALSE)
        for (k in seq_along(gcp_cor_pt_idx)) {
          nm <- gcp_names[k]
          summ[nm, "Mean"] <- x_mode[gcp_free_idx[k]]
          summ[nm, "Mode"] <- x_mode[gcp_free_idx[k]]
          summ[nm, "50%"]  <- x_mode[gcp_free_idx[k]]
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
      native_backend = native_backend,
      cli_env = samp_env
    )
    dic_list <- get_dic(
      x_samp = x_samp,
      theta_star = theta_star,
      pt = pt,
      lavmodel = lavmodel,
      loglik = function(x) {
        model_loglik_dispatch(
          x,
          lavmodel,
          lavsamplestats,
          lavdata,
          lavoptions,
          lavcache,
          native_backend = native_backend
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
    DIC = dic_list,
    summary = summ,
    ppp = ppp,
    optim_method = optim_method,
    marginal_method = marginal_method,
    theta_star_novbc = as.numeric(theta_star),
    theta_star = as.numeric(theta_star),
    Sigma_theta = Sigma_theta,
    R_star = R_star,
    vcov_x = vcov_x,
    approx_data = approx_data,
    nsamp = nsamp,
    pdf_data = pdf_data,
    mloglik = mloglik,
    partable = pt,
    init_method = init_method,
    lavmodel = lavmodel,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavoptions = lavoptions,
    lavpartable = lavpartable,
    lavcache = lavcache,
    native_backend = native_backend,
    native_theta_transforms = if (!is.null(native_backend)) native_theta_transforms else NULL,
    cov_var_idx1 = if (!is.null(native_backend)) cov_var_idx1 else NULL,
    cov_var_idx2 = if (!is.null(native_backend)) cov_var_idx2 else NULL,
    opt = opt,
    timing = timing[-1], # remove start.time
    visual_debug = visual_debug,
    vb = vb,
    joint_lp = joint_lp
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
  use_gcp = FALSE,
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
  use_gcp = FALSE,
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
  use_gcp = FALSE,
  cores = NULL,
  ...
) {
  sc <- sys.call()
  sc[["model.type"]] <- quote("growth")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}
