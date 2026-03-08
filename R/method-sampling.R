#' Draw Samples from the Generative Model
#'
#' Sample model parameters, latent variables, or observed variables from the
#' generative model underlying a fitted INLAvaan model. By default, parameters
#' are drawn from the **posterior** distribution; set `prior = TRUE` to draw
#' from the **prior** instead (useful for prior predictive checks).
#'
#' The generative chain is:
#' \deqn{\boldsymbol\theta \sim \pi(\boldsymbol\theta \mid \mathbf{y})}
#' \deqn{\boldsymbol\eta \sim N((\mathbf{I} - \mathbf{B})^{-1}\boldsymbol\alpha,\,\boldsymbol\Phi)}
#' \deqn{\mathbf{y}^* \sim N(\boldsymbol\Lambda\boldsymbol\eta + \boldsymbol\nu,\,\boldsymbol\Theta)}
#'
#' This is distinct from [predict()], which computes individual-specific
#' factor scores \eqn{\boldsymbol\eta \mid \mathbf{y},\boldsymbol\theta}
#' conditional on observed data.
#'
#' @param object An object of class [INLAvaan] (or `inlavaan_internal`).
#' @param type Character string specifying what to sample:
#'   \describe{
#'     \item{`"lavaan"`}{(Default) The lavaan-side (constrained) model
#'       parameters. Returns an `nsamp` by `npar` matrix.}
#'     \item{`"theta"`}{The INLAvaan-side unconstrained parameters.
#'       Returns an `nsamp` by `npar` matrix.}
#'     \item{`"latent"`}{Latent variables from the model-implied
#'       distribution. Returns an `nsamp` by `nlv` matrix (one draw per
#'       posterior sample, not tied to any individual).}
#'     \item{`"observed"`}{Observed variables generated from the full
#'       model. Returns an `nsamp` by `nobs_vars` matrix.}
#'     \item{`"implied"`}{Model-implied moments. Returns a length-`nsamp`
#'       list, each element a list with `cov` (model-implied covariance
#'       matrix) and, when `meanstructure = TRUE`, `mean` (model-implied
#'       mean vector). For multi-group models each element is itself a
#'       list of groups.}
#'     \item{`"all"`}{A named list with elements `lavaan`, `theta`,
#'       `latent`, `observed`, and `implied`.}
#'   }
#' @param nsamp Number of samples to draw.
#' @param samp_copula Logical. When `TRUE` (default), posterior parameter
#'   samples use the copula method with the fitted marginals. When `FALSE`,
#'   samples are drawn from the joint Gaussian (Laplace) approximation.
#'   Ignored when `prior = TRUE`.
#' @param prior Logical. When `TRUE`, parameters are drawn from the prior
#'   distribution and then propagated through the generative model. When
#'   `FALSE` (default), parameters come from the posterior.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A matrix or named list, depending on `type`.
#'
#' @seealso [predict()], [bfit_indices()]
#'
#' @example inst/examples/ex-sampling.R
#' @name sampling
#' @rdname sampling
#' @export
setGeneric("sampling", function(object, ...) standardGeneric("sampling"))

#' @rdname sampling
#' @export
setMethod("sampling", "INLAvaan", function(
  object,
  type = c("lavaan", "theta", "latent", "observed", "implied", "all"),
  nsamp = 500L,
  samp_copula = TRUE,
  prior = FALSE,
  ...
) {
  sampling_impl(
    object@external$inlavaan_internal,
    type = type, nsamp = nsamp, samp_copula = samp_copula, prior = prior,
    meanstructure = isTRUE(object@Options$meanstructure),
    ...
  )
})

#' @exportS3Method sampling inlavaan_internal
sampling.inlavaan_internal <- function(
  object,
  type = c("lavaan", "theta", "latent", "observed", "implied", "all"),
  nsamp = 500L,
  samp_copula = TRUE,
  prior = FALSE,
  ...
) {
  sampling_impl(
    object,
    type = type, nsamp = nsamp, samp_copula = samp_copula, prior = prior,
    ...
  )
}

# ---- Internal: draw parameter samples (theta/x) -----------------------------

sample_params_posterior <- function(int, nsamp, samp_copula) {
  method <- if (isTRUE(samp_copula)) int$marginal_method else "sampling"
  sample_params(
    theta_star  = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method      = method,
    approx_data = int$approx_data,
    pt          = int$partable,
    lavmodel    = int$lavmodel,
    nsamp       = nsamp,
    R_star      = int$R_star
  )
}

# Draw from the prior: independent draws per free parameter, map to x-space.
sample_params_prior <- function(int, nsamp) {
  pt <- int$partable
  lavmodel <- int$lavmodel

  # All unique free parameter indices in the partable
  PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
  m <- length(PTFREEIDX)

  # Draw in the interpretable/natural scale for each parameter
  x_natural <- matrix(NA_real_, nrow = nsamp, ncol = m)

  for (j in seq_len(m)) {
    prior_str <- pt$prior[PTFREEIDX[j]]

    if (is.na(prior_str) || prior_str == "") {
      # Fallback: wide normal for params without an explicit prior
      x_natural[, j] <- stats::rnorm(nsamp, 0, 10)
    } else if (grepl("^normal", prior_str)) {
      par <- as.numeric(strsplit(
        gsub("normal\\(|\\)", "", prior_str), ","
      )[[1]])
      x_natural[, j] <- stats::rnorm(nsamp, par[1], par[2])
    } else if (grepl("^gamma", prior_str)) {
      is_sd <- grepl("\\[sd\\]", prior_str)
      par <- as.numeric(strsplit(
        gsub("gamma\\(|\\)|\\[sd\\]", "", prior_str), ","
      )[[1]])
      raw <- stats::rgamma(nsamp, shape = par[1], rate = par[2])
      x_natural[, j] <- if (is_sd) raw^2 else raw
    } else if (grepl("^beta", prior_str)) {
      par <- as.numeric(strsplit(
        gsub("beta\\(|\\)", "", prior_str), ","
      )[[1]])
      x_natural[, j] <- stats::rbeta(nsamp, par[1], par[2]) * 2 - 1
    } else {
      x_natural[, j] <- stats::rnorm(nsamp, 0, 10)
    }
  }

  # Map natural scale → unconstrained theta-space via g()
  theta_samp <- x_natural
  for (j in seq_len(m)) {
    theta_samp[, j] <- vapply(
      x_natural[, j], pt$g[[PTFREEIDX[j]]], numeric(1)
    )
  }

  # Apply equality constraints if present
  if (lavmodel@ceq.simple.only) {
    K <- lavmodel@ceq.simple.K
    theta_samp <- t(apply(theta_samp, 1, function(p) as.numeric(K %*% p)))
  }

  # Map theta → lavaan x-space (handles covariance = cor * sqrt(var1 * var2))
  x_samp <- t(apply(theta_samp, 1, pars_to_x, pt = pt))

  list(theta_samp = theta_samp, x_samp = x_samp)
}

# ---- Internal: generate eta from model-implied distribution ------------------

sample_latent_from_model <- function(x_row, lavmodel, strict = FALSE) {
  GLIST <- get_SEM_param_matrix(x_row, "all", lavmodel)
  nG <- lavmodel@ngroups
  eta_list <- vector("list", nG)

  for (g in seq_len(nG)) {
    glist  <- GLIST[[g]]
    Psi    <- glist$psi
    B      <- glist$beta
    alpha  <- glist$alpha

    IminB <- if (is.null(B)) diag(nrow(Psi)) else (diag(nrow(B)) - B)
    if (is.null(alpha)) alpha <- rep(0, nrow(Psi))
    IminB_inv <- solve(IminB)

    mu_eta <- as.numeric(IminB_inv %*% alpha)
    Phi    <- IminB_inv %*% Psi %*% t(IminB_inv)
    chol_Phi <- if (strict) {
      t(chol(Phi))  # error propagates if non-PD
    } else {
      tryCatch(t(chol(Phi)), error = function(e) t(chol(make_pd(Phi))))
    }

    eta <- mu_eta + as.numeric(chol_Phi %*% stats::rnorm(length(mu_eta)))
    names(eta) <- colnames(Psi)
    eta_list[[g]] <- eta
  }

  if (nG == 1L) eta_list[[1L]] else eta_list
}

# ---- Internal: generate y from model given eta ------------------------------

sample_observed_from_model <- function(x_row, eta, lavmodel, strict = FALSE) {
  GLIST <- get_SEM_param_matrix(x_row, "all", lavmodel)
  nG <- lavmodel@ngroups
  y_list <- vector("list", nG)

  for (g in seq_len(nG)) {
    glist  <- GLIST[[g]]
    Lambda <- glist$lambda
    Theta  <- glist$theta
    nu     <- glist$nu

    eta_g <- if (nG == 1L) eta else eta[[g]]
    if (is.null(nu)) nu <- rep(0, nrow(Lambda))

    mu_y <- as.numeric(Lambda %*% eta_g + nu)
    chol_Theta <- if (strict) {
      t(chol(Theta))  # error propagates if non-PD
    } else {
      tryCatch(t(chol(Theta)), error = function(e) t(chol(make_pd(Theta))))
    }
    y <- mu_y + as.numeric(chol_Theta %*% stats::rnorm(length(mu_y)))
    names(y) <- rownames(Lambda)
    y_list[[g]] <- y
  }

  if (nG == 1L) y_list[[1L]] else y_list
}

# ---- Internal: compute model-implied moments --------------------------------

compute_implied_moments <- function(x_row, lavmodel, meanstructure = FALSE) {
  GLIST <- get_SEM_param_matrix(x_row, "all", lavmodel)
  nG <- lavmodel@ngroups
  out_list <- vector("list", nG)

  for (g in seq_len(nG)) {
    glist  <- GLIST[[g]]
    Lambda <- glist$lambda
    Psi    <- glist$psi
    Theta  <- glist$theta
    B      <- glist$beta
    alpha  <- glist$alpha
    nu     <- glist$nu

    IminB <- if (is.null(B)) diag(nrow(Psi)) else (diag(nrow(B)) - B)
    IminB_inv <- solve(IminB)

    # Sigma_y = Lambda (I-B)^{-1} Psi [(I-B)^{-1}]' Lambda' + Theta
    front <- Lambda %*% IminB_inv
    Sigma_y <- front %*% Psi %*% t(front) + Theta
    rownames(Sigma_y) <- colnames(Sigma_y) <- rownames(Lambda)

    res <- list(cov = Sigma_y)

    if (meanstructure) {
      if (is.null(alpha)) alpha <- rep(0, nrow(Psi))
      if (is.null(nu))    nu    <- rep(0, nrow(Lambda))
      mu_y <- as.numeric(Lambda %*% IminB_inv %*% alpha + nu)
      names(mu_y) <- rownames(Lambda)
      res$mean <- mu_y
    }

    out_list[[g]] <- res
  }

  if (nG == 1L) out_list[[1L]] else out_list
}

# ---- Internal: prior generative sampling with reject-and-redraw --------------
#
# When prior = TRUE and we need latent/observed draws, parameter vectors that
# produce non-positive-definite model-implied covariance matrices are rejected
# and redrawn.  This preserves the exact prior distribution rather than silently
# projecting non-PD matrices to PD space via make_pd().

sampling_prior_generative <- function(int, type, nsamp, meanstructure = FALSE) {
  pt       <- int$partable
  xnames   <- pt$names[pt$free > 0 & !duplicated(pt$free)]
  lavmodel <- int$lavmodel
  nG       <- lavmodel@ngroups

  # For 'implied' alone, no Cholesky decomposition is needed — just sample
  # parameters and compute the moments directly (no rejection required).
  if (type == "implied") {
    samp <- sample_params_prior(int, nsamp)
    colnames(samp$x_samp) <- xnames
    return(lapply(seq_len(nsamp), function(i) {
      compute_implied_moments(samp$x_samp[i, ], lavmodel, meanstructure)
    }))
  }

  need_obs     <- type %in% c("observed", "all")
  need_implied <- type == "all"

  # Pre-compute dimensions from a single draw
  samp0  <- sample_params_prior(int, 1L)
  GLIST0 <- get_SEM_param_matrix(samp0$x_samp[1, ], "all", lavmodel)
  nlv      <- ncol(GLIST0[[1]]$psi)
  nobs     <- nrow(GLIST0[[1]]$lambda)
  lv_names <- colnames(GLIST0[[1]]$psi)
  ov_names <- rownames(GLIST0[[1]]$lambda)

  # Column names for output matrices
  if (nG == 1L) {
    eta_cn <- lv_names
    y_cn   <- ov_names
  } else {
    eta_cn <- paste0(rep(lv_names, nG), ".g", rep(seq_len(nG), each = nlv))
    y_cn   <- paste0(rep(ov_names, nG), ".g", rep(seq_len(nG), each = nobs))
  }

  # Pre-allocate storage
  npar      <- length(xnames)
  x_mat     <- matrix(NA_real_, nsamp, npar)
  theta_mat <- matrix(NA_real_, nsamp, npar)
  eta_mat   <- matrix(NA_real_, nsamp, length(eta_cn))
  y_mat     <- if (need_obs) matrix(NA_real_, nsamp, length(y_cn)) else NULL
  colnames(x_mat)     <- xnames
  colnames(theta_mat) <- xnames
  colnames(eta_mat)   <- eta_cn
  if (need_obs) colnames(y_mat) <- y_cn
  implied_list <- if (need_implied) vector("list", nsamp) else NULL

  max_attempts <- nsamp * 20L  # tolerate up to ~95% rejection rate
  collected    <- 0L
  attempts     <- 0L

  while (collected < nsamp && attempts < max_attempts) {
    # Draw a batch of parameters (draw what we still need)
    batch_n    <- nsamp - collected
    samp_batch <- sample_params_prior(int, batch_n)

    for (i in seq_len(batch_n)) {
      attempts <- attempts + 1L
      if (attempts > max_attempts) break

      x1 <- samp_batch$x_samp[i, ]

      # Try latent draw (strict = TRUE: no make_pd fallback)
      eta1 <- tryCatch(
        sample_latent_from_model(x1, lavmodel, strict = TRUE),
        error = function(e) NULL
      )
      if (is.null(eta1)) next

      # Try observed draw if needed
      if (need_obs) {
        y1 <- tryCatch(
          sample_observed_from_model(x1, eta1, lavmodel, strict = TRUE),
          error = function(e) NULL
        )
        if (is.null(y1)) next
      }

      # Valid draw -- store it
      collected <- collected + 1L
      x_mat[collected, ]     <- x1
      theta_mat[collected, ] <- samp_batch$theta_samp[i, ]

      if (nG == 1L) {
        eta_mat[collected, ] <- eta1
        if (need_obs) y_mat[collected, ] <- y1
      } else {
        eta_mat[collected, ] <- unlist(eta1)
        if (need_obs) y_mat[collected, ] <- unlist(y1)
      }

      if (need_implied) {
        implied_list[[collected]] <- compute_implied_moments(
          x1, lavmodel, meanstructure
        )
      }

      if (collected >= nsamp) break
    }
  }

  rejected <- attempts - collected
  if (rejected > 0L) {
    rej_pct <- round(100 * rejected / attempts, 1)
    cli_inform(
      "Prior sampling: {rejected} of {attempts} draw{?s} ({rej_pct}%) rejected (non-PD model-implied covariance)."
    )
  }

  if (collected < nsamp) {
    cli_warn(c(
      "Prior rejection sampling fell short of the requested sample size.",
      "i" = "Only {collected} of {nsamp} samples obtained after {attempts} attempts.",
      "i" = "Consider using more informative priors."
    ))
    if (collected == 0L) {
      cli_abort("No valid prior draws obtained. Priors may be too vague.")
    }
    # Trim to valid rows
    x_mat     <- x_mat[seq_len(collected), , drop = FALSE]
    theta_mat <- theta_mat[seq_len(collected), , drop = FALSE]
    eta_mat   <- eta_mat[seq_len(collected), , drop = FALSE]
    if (need_obs) y_mat <- y_mat[seq_len(collected), , drop = FALSE]
    if (need_implied) implied_list <- implied_list[seq_len(collected)]
  }

  if (type == "latent")   return(eta_mat)
  if (type == "observed") return(y_mat)

  # type == "all"
  list(
    lavaan   = x_mat,
    theta    = theta_mat,
    latent   = eta_mat,
    observed = y_mat,
    implied  = implied_list
  )
}

# ---- Main workhorse ----------------------------------------------------------

sampling_impl <- function(
  int,
  type = c("lavaan", "theta", "latent", "observed", "implied", "all"),
  nsamp = 500L,
  samp_copula = TRUE,
  prior = FALSE,
  meanstructure = FALSE,
  ...
) {
  type <- match.arg(type)

  # For prior sampling with generative draws, use reject-and-redraw to preserve
  # the exact prior (no silent PD projection).
  if (isTRUE(prior) && type %in% c("latent", "observed", "implied", "all")) {
    return(sampling_prior_generative(int, type, nsamp, meanstructure))
  }

  # Step 1: draw parameters
  if (isTRUE(prior)) {
    samp <- sample_params_prior(int, nsamp)
  } else {
    samp <- sample_params_posterior(int, nsamp, samp_copula)
  }

  pt <- int$partable
  xnames <- pt$names[pt$free > 0 & !duplicated(pt$free)]
  lavmodel <- int$lavmodel

  colnames(samp$x_samp)     <- xnames
  colnames(samp$theta_samp) <- xnames

  # Early return for parameter-only types
  if (type == "lavaan")  return(samp$x_samp)
  if (type == "theta")   return(samp$theta_samp)

  # Compute model-implied moments if requested
  if (type == "implied" || type == "all") {
    implied_list <- lapply(seq_len(nsamp), function(i) {
      compute_implied_moments(samp$x_samp[i, ], lavmodel, meanstructure)
    })
    if (type == "implied") return(implied_list)
  }

  # Pre-compute dimensions from the first draw
  nG     <- lavmodel@ngroups
  GLIST0 <- get_SEM_param_matrix(samp$x_samp[1, ], "all", lavmodel)
  nlv    <- ncol(GLIST0[[1]]$psi)
  nobs   <- nrow(GLIST0[[1]]$lambda)
  lv_names <- colnames(GLIST0[[1]]$psi)
  ov_names <- rownames(GLIST0[[1]]$lambda)

  # Step 2: draw latent variables from model-implied distribution
  if (nG == 1L) {
    eta_mat <- t(vapply(seq_len(nsamp), function(i) {
      sample_latent_from_model(samp$x_samp[i, ], lavmodel)
    }, numeric(nlv)))
    colnames(eta_mat) <- lv_names
  } else {
    eta_list <- lapply(seq_len(nsamp), function(i) {
      sample_latent_from_model(samp$x_samp[i, ], lavmodel)
    })
    eta_mat <- do.call(rbind, lapply(eta_list, function(el) {
      unlist(el)
    }))
    colnames(eta_mat) <- paste0(
      rep(lv_names, nG), ".g", rep(seq_len(nG), each = nlv)
    )
  }

  if (type == "latent") return(eta_mat)

  # Step 3: draw observed variables from model given eta
  if (nG == 1L) {
    y_mat <- t(vapply(seq_len(nsamp), function(i) {
      sample_observed_from_model(samp$x_samp[i, ], eta_mat[i, ], lavmodel)
    }, numeric(nobs)))
    colnames(y_mat) <- ov_names
  } else {
    y_mat <- t(vapply(seq_len(nsamp), function(i) {
      eta_per_group <- split(eta_mat[i, ], rep(seq_len(nG), each = nlv))
      eta_per_group <- lapply(eta_per_group, unname)
      unlist(sample_observed_from_model(
        samp$x_samp[i, ], eta_per_group, lavmodel
      ))
    }, numeric(nobs * nG)))
    colnames(y_mat) <- paste0(
      rep(ov_names, nG), ".g", rep(seq_len(nG), each = nobs)
    )
  }

  if (type == "observed") return(y_mat)

  # type == "all"
  list(
    lavaan   = samp$x_samp,
    theta    = samp$theta_samp,
    latent   = eta_mat,
    observed = y_mat,
    implied  = implied_list
  )
}
