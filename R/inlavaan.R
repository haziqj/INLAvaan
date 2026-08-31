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
#' @param model.type The lavaan entry point used to fit `model`: `"cfa"`,
#'   `"sem"`, or `"growth"` (matching lavaan's model-specific wrapper
#'   functions), or `"lavaan"` for the general-purpose interface. Set
#'   automatically by [acfa()], [asem()], and [agrowth()]; documented
#'   explicitly here because lavaan >= 0.7-1 renamed the corresponding
#'   `simulateData()` argument to `model_type`, so it can no longer be
#'   inherited from there.
#'
#' @param dp Default prior distributions for the different types of model
#'   parameters; a named character vector as returned by [priors_for()].
#' @param test Character indicating which post-estimation quantities to
#'   compute. Defaults to "standard": posterior fit indices (PPP and DIC),
#'   plus -- for models supported by the casewise machinery and fitted with
#'   a mean structure -- a full leave-one-out cross-validation whenever its
#'   predicted serial cost is within a 10-second budget, with the WAIC
#'   derived from the same computation at no extra cost; both are stored
#'   with the fit (see [loo()] and [waic()]). "none" skips all of these.
#'   Include "loo" (e.g. `test = c("standard", "loo")`, or `test = "loo"`
#'   alone) to force the full LOO regardless of the budget.
#' @param vb_correction Logical indicating whether to apply a variational Bayes
#'   correction for the posterior mean vector of estimates. Defaults to `TRUE`.
#' @param n_qmc Number of quasi-Monte Carlo nodes used by the VB mean
#'   correction. The correction solves an integral over these nodes, so it
#'   carries an integration error that falls as `n_qmc` rises. The default of
#'   `64` keeps that error at roughly 0.05 posterior SDs or less -- on par
#'   with the Monte Carlo error of a routine MCMC run and well below the size
#'   of the shifts being corrected; `128` (the size of the stored Sobol
#'   table; larger values require the \pkg{qrng} package) roughly halves it
#'   again for models where the correction itself is of particular interest.
#'   `diagnostics()` reports the realised error as `vb_mcse_sigma` per
#'   parameter and `vb_mcse_max` globally, both in posterior-SD units, so the
#'   setting can be checked rather than guessed. Ignored when
#'   `vb_correction = FALSE`.
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
#'   drawn using the copula method with the fitted marginals (e.g. skew-normal
#'   or asymmetric Gaussian), with NORTA correlation adjustment. When `FALSE`,
#'   samples are drawn from the Gaussian (Laplace) approximation. Only re
#' @param cov_as_cor Logical. Residual and latent-disturbance covariance
#'   parameters (`~~` between two observed or two latent variables) are
#'   always estimated on the correlation scale internally (an `atanh` link,
#'   the same as for `std.ov`/`std.lv`-standardised parameters); by default
#'   their reported marginal is then re-derived on the covariance scale
#'   \eqn{\sigma_i \sigma_j \rho} from a posterior sample (see
#'   `samp_copula`), because that is the scale lavaan/blavaan report by
#'   default. When `TRUE`, that re-derivation is skipped and each such
#'   parameter's own directly profiled correlation-scale marginal
#'   \eqn{\rho \in (-1, 1)} is reported instead -- useful for comparing the
#'   profiling machinery (skew-normal fit, VB, ...) against a
#'   correlation-scale reference without the sampling/copula step in
#'   between. Model estimation is identical either way; only what is
#'   reported for these parameters changes (and, correspondingly, their
#'   `mat` classification in the returned partable, `theta_cov`/`psi_cov`
#'   vs. `theta_cor`/`psi_cor`). Not the same as lavaan's `std.ov`/`std.lv`,
#'   which re-parameterises the whole model on a standardised scale.
#'   Defaults to `FALSE`.
#' @param sn_fit_ngrid Number of grid points to lay out per dimension when
#'   fitting the skew-normal marginals. A finer grid gives a better fit at the
#'   cost of more joint-log-posterior evaluations. Defaults to `21`.
#' @param sn_fit_logthresh The log-threshold for fitting the skew-normal. Points
#'   with log-posterior drop below this threshold (relative to the maximum) will
#'   be excluded from the fit. Defaults to `-6`.
#' @param sn_fit_temp Temperature parameter for fitting the skew-normal.
#'   Defaults to `1` (weights are the density values themselves). If `NA`, the
#'   temperature is included as an additional optimisation parameter.
#' @param sn_fit_sample Logical. When `TRUE` (default), a parametric skew-normal
#'   is fitted to the posterior samples for covariance and defined parameters.
#'   When `FALSE`, these are summarised using kernel density estimation instead.
#' @param control A list of control parameters for the optimiser. For the
#'   default `"nlminb"`, INLAvaan raises the stock iteration ceilings to
#'   `iter.max = 1000` and `eval.max = 2000` (complex models can exhaust
#'   `nlminb()`'s own defaults of 150 and 200); any value supplied here
#'   overrides these.
#' @param verbose Logical indicating whether to print progress messages.
#' @param debug Logical indicating whether to return debug information.
#' @param add_priors Logical indicating whether to include prior densities in
#'   the posterior computation.
#' @param optim_method The optimisation method to use for finding the posterior
#'   mode. Options include `"nlminb"` (default), `"ucminf"`, and `"optim"`
#'   (BFGS).
#' @param numerical_grad Logical indicating whether to use numerical gradients
#'   for the optimisation. Defaults to `FALSE` to use analytical gradients.
#' @param start Optional numeric vector of starting values for the optimiser,
#'   given as a full vector of free parameters in the internal (unconstrained)
#'   parameterisation. Mainly for internal use by [update()], which warm-starts
#'   mode-finding from a previous fit's posterior mode; supplying a hand-built
#'   vector requires knowledge of the internal parameter ordering. Its length
#'   must equal the number of free parameters or an error is raised.
#' @param cores Integer or `NULL`. Number of cores for parallel marginal
#'   fitting. When `NULL` (default), serial execution is used unless the number
#'   of free parameters exceeds 120, in which case parallelisation is enabled
#'   automatically using all available physical cores. Set to `1L` to force
#'   serial execution. If `cores > 1`, marginal fits are distributed across
#'   cores using [parallel::mclapply()] (fork-based; no parallelism on Windows).
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
  n_qmc = 64L,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  cov_as_cor = FALSE,
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
  start = NULL,
  cores = NULL,
  ...
) {
  mc <- match.call()
  mc$start <- NULL # warm start is transient; keep it out of the recorded call
  start_time0 <- proc.time()[3]
  timing <- list(start_time = start_time0)

  ## ----- Check arguments -----------------------------------------------------
  if (!is.null(cores)) {
    # nocov start
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
  # "loo" is INLAvaan-specific: strip it before `test` reaches lavaan
  do_loo <- "loo" %in% test
  test <- setdiff(test, "loo")
  if (length(test) == 0L) {
    test <- "none"
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
    if (!(lavargs$estimator %in% c("ML", "PML"))) {
      # nocov
      cli_abort("Only 'ML' and 'PML' estimators are supported currently.")
    }
  }

  # Two-level models cannot drop the mean structure: the between-level
  # statistics are the cluster means, and the marginalised treatment of
  # covariance-only analyses is single-level (and multigroup) only. lavaan
  # force-enables meanstructure for multilevel data anyway; if the user
  # *explicitly* asked for FALSE, say so rather than silently complying.
  if (isFALSE(lavargs$meanstructure) && !is.null(lavargs$cluster)) {
    cli_warn(c(
      "Two-level models require a mean structure; fitting with
       {.code meanstructure = TRUE}.",
      "i" = "See {.code vignette(\"meanstructure\", package = \"INLAvaan\")}
       for how INLAvaan treats mean structures."
    ))
    lavargs$meanstructure <- TRUE
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

  # Partable and check for equality constraints
  pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions)
  PTFREEIDX <- which(pt$free > 0L)
  if (isTRUE(ceq.simple)) {
    # Note: Always work in the reduced space
    PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
  }
  m <- length(PTFREEIDX)
  parnames <- pt$names[PTFREEIDX]

  # Cache partable for prior logdens and grad
  prior_cache <- prepare_priors_for_optim(pt)

  # Saturated-means fast path (see saturated_mean_idx): along the free
  # intercept axes the posterior is exactly Gaussian and block-diagonal at
  # the mode, so the Hessian block is analytic and the marginal scans are
  # redundant for those coordinates.
  fastpath <- saturated_mean_idx(
    pt,
    lavmodel,
    lavsamplestats,
    lavdata,
    ceq.simple
  )
  fp_idx <- if (is.null(fastpath)) integer(0) else fastpath$idx

  ## ----- Prep work for approximation -----------------------------------------
  joint_lp <- function(pars) {
    if (isTRUE(ceq.simple)) {
      pars_unpacked <- as.numeric(ceq.K %*% pars)
      x <- pars_to_x(pars_unpacked, pt)
    } else {
      x <- pars_to_x(pars, pt)
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
      jcb <- mapply(
        function(f, x) f(x),
        pt$ginv_prime[pt$free > 0],
        pars_unpacked
      )
    } else {
      x <- pars_to_x(pars, pt)
      jcb <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
    }
    gll <- inlav_model_grad(x, lavmodel, lavsamplestats, lavdata, lavcache)

    # Jacobian adjustment: d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ.
    # The chain-rule Jacobian is a diagonal (per-parameter ginv_prime, times
    # sd1sd2 for the correlation parameters) plus a handful of off-diagonal
    # variance-into-covariance terms listed in jcb_mat, so the product is
    # applied as vector work plus a short loop over those terms rather than
    # ever forming the dense m x m matrix -- this sits inside every gradient
    # call (optimiser, Hessian columns, VB node sweeps, marginal scans).
    gll_th <- jcb * attr(x, "sd1sd2") * gll
    jcb_mat <- attr(x, "jcb_mat")

    if (!is.null(jcb_mat)) {
      for (k in seq_len(nrow(jcb_mat))) {
        i <- jcb_mat[k, 1]
        j <- jcb_mat[k, 2]
        gll_th[i] <- gll_th[i] + jcb_mat[k, 3] * gll[j]
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
    optim_stage <- "Mode finding and Hessian computation"
    cli_progress_step(
      "{optim_stage}.",
      msg_done = "Posterior mode and Hessian."
    )
  }

  ob <- function(x) -1 * joint_lp(x)
  gr <- if (isTRUE(numerical_grad)) NULL else function(x) -1 * joint_lp_grad(x)
  parstart <- pt$parstart[PTFREEIDX]

  # Warm start: `start` is a full vector of free parameters in the internal
  # (unconstrained) parameterisation, e.g. the posterior mode of an earlier
  # fit reused by `update()`. Only valid when the parameter structure matches.
  if (!is.null(start)) {
    if (length(start) != length(parstart)) {
      cli_abort(c(
        "{.arg start} has length {length(start)} but the model has
         {length(parstart)} free parameter{?s}.",
        "i" = "{.arg start} must be a full vector of free parameters in the
               internal (unconstrained) parameterisation."
      ))
    }
    parstart <- as.numeric(start)
  }

  if (optim_method == "nlminb") {
    # nlminb()'s own defaults (iter.max = 150, eval.max = 200) are too tight
    # for complex models, and running out is quiet: convergence = 1 surfaces
    # only through diagnostics() or the fit-time warning. Raise the ceiling,
    # letting an explicit user `control` win.
    ctrl <- utils::modifyList(
      list(iter.max = 1000L, eval.max = 2000L),
      control
    )
    opt <- nlminb(
      start = parstart,
      objective = ob,
      gradient = gr,
      control = ctrl
    )
    theta_star <- opt$par
    if (isTRUE(verbose)) {
      optim_stage <- "Computing the Hessian"
      cli_progress_update()
    }
    if (isTRUE(numerical_grad)) {
      H_neg <- fast_hessian(ob, theta_star)
    } else if (length(fp_idx)) {
      # assemble in blocks: finite differences over the covariance columns
      # only; the intercept block is analytic (n Sigma^{-1} restricted to
      # the free-intercept variables, plus the prior precision) and the
      # cross block is exactly zero at the mode
      cc <- setdiff(seq_len(m), fp_idx)
      H_neg <- fast_jacobian(
        function(x) -1 * joint_lp_grad(x),
        theta_star,
        cols = cc
      )
      H_neg[fp_idx, ] <- 0
      Sg_hat <- lavaan::lav_model_implied(
        lavaan::lav_model_set_parameters(lavmodel, pars_to_x(theta_star, pt))
      )$cov[[1L]]
      sp <- fastpath$sigma_pos
      H_neg[fp_idx, fp_idx] <- n *
        chol2inv(chol(Sg_hat))[sp, sp, drop = FALSE] +
        diag(fastpath$prec, length(fp_idx))
    } else {
      # H_neg <- numDeriv::jacobian(function(x) -1 * joint_lp_grad(x), theta_star)
      H_neg <- fast_jacobian(function(x) -1 * joint_lp_grad(x), theta_star)
    }
  } else if (optim_method == "ucminf") {
    # nocov start
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
  } else {
    # nocov end
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
  opt$dx_analytic <- -1 * joint_lp_grad(theta_star) # analytic grad
  if (isTRUE(debug)) {
    tab <- data.frame(
      analytic = round(opt$dx_analytic, 6),
      fd = round(opt$dx, 6),
      diff = round(opt$dx_analytic - opt$dx, 6),
      row.names = parnames
    )
    cli::cli_rule(left = "{.strong Gradient check at posterior mode}")
    print(tab)
    cli::cli_rule()
  }

  timing <- add_timing(timing, "optim")

  ## ----- VB correction -------------------------------------------------------
  vb_opt <- vb_shift <- vb_kld <- vb_kld_global <- vb_mcse <- NA
  vb_n_qmc <- NA_integer_
  if (isTRUE(vb_correction)) {
    if (isTRUE(verbose)) {
      cli_progress_step(
        "Performing VB correction.",
        msg_done = "VB correction; mean |\U03B4| = {formatC(mean(abs(vb_shift) / sqrt(diag(Sigma_theta))),
                    format = 'f', digits = 3)}\U03C3."
      )
    }

    # QMC nodes (scrambled Sobol). The count is deliberately flat rather than
    # scaled with m: the quadrature error is governed by the effective
    # dimension and the smoothness of the integrand, not by m directly, and
    # scaling down for small models simply starves them.
    if (length(n_qmc) != 1L) {
      cli_abort("{.arg n_qmc} must be a single integer of at least 2.")
    }
    vb_n_qmc <- suppressWarnings(as.integer(n_qmc))
    if (is.na(vb_n_qmc) || vb_n_qmc < 2L) {
      cli_abort("{.arg n_qmc} must be a single integer of at least 2.")
    }
    zs <- vb_nodes(vb_n_qmc, L)

    # Fixed-point solver settings; see the iteration below. Convergence is
    # judged on the step measured in the units the shift is reported in --
    # posterior SDs -- rather than on the whitened step, so the tolerance is
    # directly comparable to the size of correction that matters downstream.
    vb_maxit <- 25L
    vb_tol <- 1e-3
    vb_sd <- sqrt(diag(Sigma_theta))
    # Cap on how far a single step may travel, in posterior SDs. The step is a
    # Newton step for a curvature of -H, which is only valid while the
    # third-order remainder is small; on strongly skewed posteriors the first
    # step can otherwise land outside the region where Sigma(theta) stays
    # positive definite. Genuine shifts are a fraction of an SD, so this never
    # binds on a well-behaved fit.
    vb_maxstep <- 1

    vb_ob_shift <- function(shift, mu0, Z) {
      mu_new <- mu0 + shift
      ns <- nrow(Z)
      lp_total <- 0
      for (b in seq_len(ns)) {
        thetab <- mu_new + Z[b, , drop = TRUE]
        lp_total <- lp_total + joint_lp(thetab)
      }
      -1 * (lp_total / ns)
    }

    vb_ob <- function(delta, mu0, Z) {
      vb_ob_shift(as.numeric(L %*% delta), mu0, Z)
    }

    # One sweep over the nodes: the mean score in the original parameter
    # scale, plus its two half-set means. The halves come along free -- both
    # are already computed here -- and their disagreement at the solution
    # measures the quadrature error in the shift. Sobol points are nested, so
    # the two halves are each a valid node set in their own right.
    vb_sweep <- function(shift, mu0, Z) {
      mu_new <- mu0 + shift
      ns <- nrow(Z)
      nhalf <- floor(ns / 2)
      gA <- gB <- numeric(length(mu0))
      for (b in seq_len(ns)) {
        g <- joint_lp_grad(mu_new + Z[b, , drop = TRUE])
        if (b <= nhalf) {
          gA <- gA + g
        } else {
          gB <- gB + g
        }
      }
      list(
        score = (gA + gB) / ns,
        gA = gA / nhalf,
        gB = gB / (ns - nhalf)
      )
    }

    vb_gA <- vb_gB <- numeric(m)
    vb_gr <- function(delta, mu0, Z) {
      sw <- vb_sweep(as.numeric(L %*% delta), mu0, Z)
      vb_gA <<- sw$gA
      vb_gB <<- sw$gB
      as.numeric(t(L) %*% (-1 * sw$score))
    }

    # Fast path: fixed-point iteration. Splitting the objective into its
    # quadratic part and a remainder r, and using t(L) %*% H %*% L = I with
    # centred nodes, stationarity reduces to
    # shift = shift + Sigma_theta %*% E[grad log pi] -- the Newton step for a
    # curvature of -H is one multiplication by Sigma_theta. Where r really is
    # third-order small this contracts in a few iterations and needs no
    # objective evaluations, which is where nlminb spent half its node sweeps
    # while still stopping short of convergence at its default tolerance.
    #
    # That premise fails on strongly skewed posteriors, where the first step can
    # overshoot the region in which Sigma(theta) stays positive definite. An
    # oversized step or a non-finite score is taken as the signal, and the solve
    # falls back to nlminb, whose line search handles those cases.
    # Anderson(1) acceleration on top of the fixed-point iteration. The plain
    # map contracts at the spectral radius of I - Sigma_theta %*% Hbar, where
    # Hbar is the curvature averaged over the node cloud rather than at the
    # mode; that mismatch costs a near-constant factor per sweep, a sweep is
    # a full pass of the gradient over the nodes, and on flat problems (the
    # two-group models are the known case) the factor approaches 1 and the
    # plain map stalls into the nlminb fallback. A secant estimate from
    # consecutive steps removes the dominant error mode, roughly halving the
    # sweep count and un-stalling the flat case. Convergence is still
    # declared on the size of the raw Newton step -- the fixed-point
    # residual -- so the accelerated solve stops at exactly the same
    # criterion, and the same solution, as the plain one; acceleration only
    # changes how fast it gets there. The extrapolation is skipped (plain
    # step taken) whenever the secant is degenerate or would move further
    # than the plain step allows.
    vb_shift <- numeric(m)
    vb_iter <- 0L
    vb_move <- Inf
    vb_fallback <- FALSE
    vb_step_prev <- NULL
    vb_shift_prev <- NULL
    for (it in seq_len(vb_maxit)) {
      vb_sw <- vb_sweep(vb_shift, theta_star, zs)
      if (!all(is.finite(vb_sw$score))) {
        vb_fallback <- TRUE
        break
      }
      vb_gA <- vb_sw$gA
      vb_gB <- vb_sw$gB
      vb_step <- as.numeric(Sigma_theta %*% vb_sw$score)
      vb_step[fp_idx] <- 0
      vb_move <- max(abs(vb_step) / vb_sd)
      if (vb_move > vb_maxstep) {
        vb_fallback <- TRUE
        break
      }
      if (vb_move < vb_tol) {
        vb_shift <- vb_shift + vb_step
        vb_iter <- it
        break
      }
      vb_update <- vb_step
      if (!is.null(vb_step_prev)) {
        df <- vb_step - vb_step_prev
        dx <- vb_shift - vb_shift_prev
        denom <- sum(df^2)
        if (denom > 0) {
          gam <- sum(vb_step * df) / denom
          cand <- vb_step - gam * (dx + df)
          cand[fp_idx] <- 0
          if (
            all(is.finite(cand)) &&
              max(abs(cand) / vb_sd) <= min(vb_maxstep, 2 * vb_move)
          ) {
            vb_update <- cand
          }
        }
      }
      vb_step_prev <- vb_step
      vb_shift_prev <- vb_shift
      vb_shift <- vb_shift + vb_update
      vb_iter <- it
    }
    if (vb_move >= vb_tol) {
      vb_fallback <- TRUE
    }

    if (isTRUE(vb_fallback)) {
      # Optimise in whitened coordinates, where the problem is well conditioned.
      vb_nl <- nlminb(
        start = numeric(m),
        objective = vb_ob,
        gradient = vb_gr,
        mu0 = theta_star,
        Z = zs,
        control = list(rel.tol = 1e-8)
      )
      vb_shift <- as.numeric(L %*% vb_nl$par)
      # Under the fast path this block is independent of the rest, so imposing
      # its known-zero optimum after the fact is exact.
      vb_shift[fp_idx] <- 0
      vb_iter <- vb_nl$iterations
    }

    # Quadrature error in the shift. With two half-sets the standard error of
    # their mean is half their difference; a Newton step maps a score error
    # into a shift error. QMC halves are negatively correlated and QMC error
    # falls faster than root-n, so this errs on the conservative side.
    vb_mcse <- abs(as.numeric(Sigma_theta %*% (vb_gA - vb_gB))) / 2
    vb_mcse[fp_idx] <- 0

    vb_opt <- list(
      par = vb_shift,
      objective = vb_ob_shift(vb_shift, theta_star, zs),
      iterations = vb_iter,
      fallback = vb_fallback
    )

    vb_kld <- (vb_shift)^2 / (2 * diag(Sigma_theta))
    vb_kld_global <- lp_max + vb_opt$objective
  }

  vb <- list(
    opt = vb_opt,
    n_qmc = vb_n_qmc,
    correction = vb_shift,
    mcse = vb_mcse,
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
    theta_star_trans <- pars_to_x(as.numeric(ceq.K %*% theta_star_vbc), pt)
  } else {
    theta_star_trans <- pars_to_x(theta_star_vbc, pt)
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
    if (eff_cores > 1L && .Platform$OS.type == "windows") {
      # nocov start
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
        msg_parallel = "Calibrating {done}/{m} asymmetric Gaussians ({cores}\U00D7).",
        msg_done = "Calibrate {m}/{m} asymmetric Gaussian{?s}."
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
        if (j %in% fp_idx) {
          # saturated-means fast path: this axis is exactly Gaussian, and
          # the scan would reproduce the Laplace marginal to numerical
          # precision -- emit it directly
          return(list(
            fit = c(
              xi = theta_star[j],
              omega = sqrt(Sigma_theta[j, j]),
              alpha = 0,
              logC = 0,
              k = 0,
              rmse = 0,
              nmad = 0,
              gamma1 = 0
            ),
            visual_debug = NULL
          ))
        }
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
        msg_parallel = "Fitting {done}/{m} skew-normal marginals ({cores}\U00D7).",
        msg_done = "Fit {m}/{m} skew-normal marginal{?s}."
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
      cli_progress_step(
        "Adjusting copula correlations (NORTA).",
        msg_done = "Adjust copula correlations (NORTA)."
      )
    }
    R_star <- norta_adjust_R(cov2cor(Sigma_theta), approx_data)
  }
  timing <- add_timing(timing, "norta")

  ## ----- Draw posterior samples (once) ---------------------------------------
  # Draw-based summaries: covariances, defined (:=) and delta (~*~) parameters,
  # or (for the pure sampling method) every marginal
  needs_draw_summaries <-
    marginal_method == "sampling" ||
    sum(pt$free > 0 & grepl("cov", pt$mat)) > 0 ||
    any(pt$op == ":=") ||
    any(pt$op == "~*~")
  has_extra_samp_work <- needs_draw_summaries || test != "none"
  samp_env <- NULL
  if (isTRUE(verbose)) {
    samp_stage <- if (has_extra_samp_work) {
      "Posterior sampling and summarising"
    } else {
      "Drawing posterior samples"
    }
    # Rewritten at the end of the block with an inventory of what the draws
    # were used for
    samp_done <- paste0(samp_stage, ".")
    samp_env <- environment()
    cli_progress_step(
      "{samp_stage}.",
      msg_done = "{samp_done}",
      spinner = TRUE,
      .envir = samp_env
    )
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
  summ <- cbind(
    summ,
    kld = vb$kld,
    vb_shift_sigma = vb$correction / sqrt(diag(Sigma_theta))
  )

  pdf_data <- lapply(postmargres, function(x) x$pdf_data)
  names(pdf_data) <- parnames

  coefs <- summ[, "Mean"]
  names(coefs) <- parnames

  summ <- as.data.frame(summ)
  summ$Prior <- pt$prior[PTFREEIDX]

  ## ----- Sampling for covariances and defined params -------------------------
  # cov_as_cor skips this re-derivation entirely: the per-axis marginal
  # already computed above (postmargres) is left as the final reported
  # value for these rows, which -- since g/ginv is atanh/tanh for
  # theta_cov/psi_cov exactly as for theta_cor/psi_cor -- is already the
  # correlation, not the covariance. Nothing upstream (pars_to_x(), priors,
  # gradients) reads cov_as_cor, so estimation is unaffected either way.
  if (!isTRUE(cov_as_cor) && sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
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
        # keep the coefficient vector on the covariance scale too; the
        # per-parameter marginal mean is on the correlation (tanh) scale
        coefs[cov_name] <- tmp_new_summ[["Mean"]]
        pdf_data[[cov_name]] <- samp_cov[[cov_name]]$pdf_data
      }
    }
  }
  timing <- add_timing(timing, "covariances")

  # Defined parameters
  if (any(pt$op == ":=")) {
    if (marginal_method == "skewnorm" && isTRUE(sn_fit_sample)) {
      # nocov start
      defpars <- get_defpars_fit_sn(x_samp, pt)
      sn_rows <- do.call(rbind, lapply(defpars, `[[`, "sn_params"))
      approx_data <- rbind(approx_data, sn_rows)
    } else {
      # nocov end
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

  ## ----- Compute ppp and dic -------------------------------------------------
  if (test != "none") {
    if (isTRUE(verbose)) {
      samp_stage <- "Computing fit indices (PPP/DIC)"
      cli_progress_update(.envir = samp_env)
    }
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

  ## ----- Fit-time LOO and WAIC -------------------------------------------------
  # Minimal internal view of the fit for the casewise machinery
  int_fit <- list(
    partable = pt,
    lavmodel = lavmodel,
    lavdata = lavdata,
    lavsamplestats = lavsamplestats,
    theta_star = as.numeric(theta_star_vbc),
    Sigma_theta = Sigma_theta,
    marginal_method = marginal_method,
    approx_data = approx_data,
    nsamp = nsamp,
    R_star = R_star
  )
  # The default path (test = "standard") computes LOO/WAIC only for models
  # the casewise kernels support, quietly, and (for LOO) only when the
  # predicted serial cost fits a 10 s budget. An explicit "loo" in `test`
  # always computes the full LOO.
  casewise_ok <- tryCatch(
    {
      suppressWarnings(check_loo_model(int_fit))
      TRUE
    },
    error = function(e) FALSE
  )

  loo_res <- NULL
  if (isTRUE(do_loo) || (test != "none" && casewise_ok)) {
    if (isTRUE(verbose)) {
      samp_stage <- "Computing Taylor LOO"
      cli_progress_update(.envir = samp_env)
    }
    loo_res <- tryCatch(
      inlav_loo(
        int = int_fit,
        eff_cores = resolve_loo_cores(cores),
        verbose = FALSE,
        max_seconds = if (isTRUE(do_loo)) Inf else 10
      ),
      inlavaan_loo_budget = function(e) {
        if (isTRUE(verbose)) {
          cli_alert_info(
            "Skipping fit-time LOO (predicted cost exceeds 10 s); compute it
             post hoc with {.fn loo} or {.fn add_loo}."
          )
        }
        NULL
      },
      error = function(e) {
        if (isTRUE(do_loo)) {
          cli_warn(c(
            "Skipping the fit-time LOO computation.",
            "x" = conditionMessage(e)
          ))
        }
        NULL
      }
    )
    timing <- add_timing(timing, "loo")
  }

  # WAIC comes free from the same Taylor pass as the LOO: identical per-unit
  # quantities, aggregated on the lpd side instead of the case-deletion side
  # (see waic_from_taylor). When the LOO was skipped -- unsupported model or
  # over budget -- the WAIC is skipped with it; waic() computes it post hoc.
  waic_res <- NULL
  if (test != "none" && !is.null(loo_res)) {
    waic_res <- waic_from_taylor(loo_res)
    timing <- add_timing(timing, "waic")
  }

  if (isTRUE(verbose)) {
    # Close the sampling step with an overview; the specific fit measures
    # computed are listed on a separate info line below
    fit_measures <- c(
      if (!is.null(ppp)) c("PPP", "DIC"),
      if (!is.null(loo_res)) "LOO",
      if (!is.null(waic_res)) "WAIC"
    )
    samp_done <- if (needs_draw_summaries || length(fit_measures)) {
      paste0("Summarise ", nsamp, " posterior draws.")
    } else {
      paste0("Draw ", nsamp, " posterior samples.")
    }
    cli_progress_done(.envir = samp_env)
    if (length(fit_measures)) {
      cli_alert_info(
        paste0("Fit measures: ", paste(fit_measures, collapse = ", "), ".")
      )
    }
  }

  ## ----- Output --------------------------------------------------------------
  # Cosmetic only, applied last: relabel theta_cov/psi_cov as theta_cor/
  # psi_cor in the RETURNED partable so it honestly reflects what was
  # reported above. Nothing upstream reads pt$mat again after this point.
  if (isTRUE(cov_as_cor)) {
    pt$mat[pt$mat == "theta_cov"] <- "theta_cor"
    pt$mat[pt$mat == "psi_cov"] <- "psi_cor"
  }

  out <- list(
    coefficients = coefs,
    mloglik = mloglik,
    DIC = dic_list,
    summary = summ,
    ppp = ppp,
    loo = loo_res,
    waic = waic_res,
    optim_method = optim_method,
    marginal_method = marginal_method,
    samp_copula = samp_copula,
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
    vb = vb,
    call = mc,
    version = as.character(utils::packageVersion("INLAvaan"))
  )
  class(out) <- "inlavaan_internal"

  # Warn (once, consolidated) if the convergence/approximation diagnostics
  # look off; see warn_fit_diagnostics() for the checks and thresholds
  warn_fit_diagnostics(out)

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
  n_qmc = 64L,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  cov_as_cor = FALSE,
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
  n_qmc = 64L,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  cov_as_cor = FALSE,
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
  n_qmc = 64L,
  marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"),
  marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"),
  nsamp = 1000,
  samp_copula = TRUE,
  cov_as_cor = FALSE,
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
  cores = NULL,
  ...
) {
  sc <- sys.call()
  sc[["model.type"]] <- quote("growth")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}
