# ---------------------------------------------------------------------------
# Shared helper: summarise a numeric vector of posterior samples into the
# standard (Mean, SD, 2.5%, 50%, 97.5%, Mode) list + pdf_data table.
# ---------------------------------------------------------------------------
summarise_samples <- function(y) {
  y <- y[is.finite(y)]
  if (length(y) == 0L) {
    res <- rep(NA_real_, 8L)
    names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
    return(list(
      summary = res,
      pdf_data = data.frame(x = numeric(0), y = numeric(0))
    ))
  }
  if (length(y) == 1L) {
    res <- c(y, 0, y, y, y, y, y, y)
    names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
    return(list(
      summary = res,
      pdf_data = data.frame(
        x = c(y - 1e-8, y, y + 1e-8),
        y = c(0, 1, 0)
      )
    ))
  }
  Ex <- mean(y)
  SDx <- stats::sd(y)
  qq <- stats::quantile(y, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  dens <- stats::density(y)
  xmax <- dens$x[which.max(dens$y)]
  res <- c(Ex, SDx, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
  list(
    summary = res,
    pdf_data = data.frame(x = dens$x, y = dens$y)
  )
}

# ---------------------------------------------------------------------------
# Core sampling function: draw from the Gaussian-copula posterior.
# Returns a list with:
#   $theta_samp  - nsamp x m matrix in the internal (theta) parameterisation
#   $x_samp      - nsamp x p matrix in the lavaan (x) parameterisation
# ---------------------------------------------------------------------------
sample_params <- function(
  theta_star,
  Sigma_theta,
  method,
  approx_data,
  pt,
  lavmodel,
  nsamp = 1000,
  R_star = NULL, # NORTA-adjusted correlation matrix for SN copula
  integration_data = NULL,
  native_theta_transforms = NULL,
  cov_var_idx1 = NULL,
  cov_var_idx2 = NULL,
  nthreads = 1L
) {
  safe_theta_to_x <- function(theta_mat) {
    x_list <- lapply(seq_len(nrow(theta_mat)), function(i) {
      tryCatch(
        pars_to_x(theta_mat[i, ], pt = pt, compute_jac = FALSE),
        error = function(e) rep(NA_real_, ncol(theta_mat))
      )
    })
    x_mat <- do.call(rbind, x_list)
    valid <- stats::complete.cases(x_mat)
    list(
      theta = theta_mat[valid, , drop = FALSE],
      x = x_mat[valid, , drop = FALSE]
    )
  }

  align_approx_data <- function(approx_data, theta_star, Sigma_theta, R_use) {
    if (is.null(approx_data)) {
      return(NULL)
    }

    m <- length(theta_star)

    if (!all(c("xi", "omega", "alpha") %in% colnames(approx_data))) {
      cli_abort(
        "{.arg approx_data} must contain columns {.val xi}, {.val omega}, and {.val alpha}."
      )
    }

    theta_names <- names(theta_star)

    if (is.null(theta_names)) {
      theta_names <- colnames(Sigma_theta)
    }

    if (is.null(theta_names)) {
      theta_names <- colnames(R_use)
    }

    if (!is.null(theta_names) && !is.null(rownames(approx_data))) {
      missing_rows <- setdiff(theta_names, rownames(approx_data))

      if (length(missing_rows) > 0L) {
        cli_abort(c(
          "Could not align {.arg approx_data} with the sampled parameter vector.",
          "i" = "Missing rows in approx_data: {paste(missing_rows, collapse = ', ')}"
        ))
      }

      approx_data_use <- approx_data[theta_names, , drop = FALSE]
    } else if (nrow(approx_data) >= m) {
      approx_data_use <- approx_data[seq_len(m), , drop = FALSE]
    } else {
      approx_data_use <- approx_data
    }

    if (nrow(approx_data_use) != m) {
      cli_abort(c(
        "Posterior sampler received inconsistent marginal dimensions.",
        "i" = "length(theta_star) = {m}",
        "i" = "nrow(approx_data_use) = {nrow(approx_data_use)}",
        "i" = "nrow(approx_data) = {nrow(approx_data)}"
      ))
    }

    if (anyNA(approx_data_use[, c("xi", "omega", "alpha"), drop = FALSE])) {
      cli_abort(
        "Posterior sampler received NA skew-normal marginal parameters."
      )
    }

    approx_data_use
  }

  m <- length(theta_star)
  R <- cov2cor(Sigma_theta)
  R_use <- if (method == "skewnorm" && !is.null(R_star)) R_star else R

  has_gcp <- length(attr(pt, "gcp_blocks")) > 0
  have_native_sampler_meta <- (
    !is.null(native_theta_transforms) &&
      !is.null(cov_var_idx1) &&
      !is.null(cov_var_idx2) &&
      length(native_theta_transforms) == m &&
      length(cov_var_idx1) == m &&
      length(cov_var_idx2) == m
  )

  # Use the fast C++ batch sampler when conditions are met:
  # - skewnorm marginals (approx_data has xi/omega/alpha)
  # - no GCP blocks (all transforms are identity/exp/tanh)
  # - native transform info provided
  use_cpp_sample <- (
    method == "skewnorm" &&
    !has_gcp &&
    have_native_sampler_meta &&
    !is.null(approx_data) &&
    !inlavaan_force_r_path()
  )

  # if (method == "skewnorm" &&
  #     !has_gcp &&
  #     !inlavaan_force_r_path() &&
  #     !have_native_sampler_meta) {
  #   cli_abort(
  #     paste(
  #       "Native skew-normal sampling metadata is missing or malformed.",
  #       "Refusing the R fallback.",
  #       "Set `options(inlavaan.backend = \"r\")` or `options(inlavaan.force_r_path = TRUE)` to use the R path explicitly."
  #     )
  #   )
  # }

  if (use_cpp_sample) {
    approx_data_use <- align_approx_data(
      approx_data = approx_data,
      theta_star = theta_star,
      Sigma_theta = Sigma_theta,
      R_use = R_use
    )

    z_raw <- matrix(rnorm(nsamp * m), nrow = nsamp)

    res <- cpp_sample_batch(
      z_raw        = z_raw,
      R_chol_ut    = chol(R_use),
      sn_xi        = as.numeric(approx_data_use[, "xi"]),
      sn_omega     = as.numeric(approx_data_use[, "omega"]),
      sn_alpha     = as.numeric(approx_data_use[, "alpha"]),
      transforms   = as.numeric(native_theta_transforms),
      cov_var_idx1 = as.integer(cov_var_idx1),
      cov_var_idx2 = as.integer(cov_var_idx2),
      nthreads     = nthreads,
      n_grid       = max(2000L, 2L * as.integer(nsamp))
    )

    theta <- res$theta
    x <- res$x

    if (lavmodel@ceq.simple.only) {
      theta <- theta %*% t(lavmodel@ceq.simple.K)
    }

    if (length(names(theta_star)) == ncol(theta)) {
      colnames(theta) <- names(theta_star)
    }

    if (length(names(theta_star)) == ncol(x)) {
      colnames(x) <- names(theta_star)
    }

    return(list(theta_samp = theta, x_samp = x))
  }

  if (method == "sampling" && !is.null(integration_data)) {
    theta_grid <- integration_data$theta_grid %||% NULL
    full_w <- integration_data$full_grid_norm_weights %||% NULL
    if (!is.null(theta_grid)) {
      theta_grid <- as.matrix(theta_grid)
      if (nrow(theta_grid) > 0L && ncol(theta_grid) == m) {
        if (is.null(full_w) || length(full_w) != nrow(theta_grid)) {
          full_w <- numeric(nrow(theta_grid))
          active_idx <- as.integer(integration_data$active_idx %||% integer(0))
          active_w <- as.numeric(integration_data$grid_norm_weights %||% numeric(0))
          keep <- active_idx >= 1L & active_idx <= nrow(theta_grid)
          if (any(keep) && length(active_w) >= sum(keep)) {
            full_w[active_idx[keep]] <- active_w[seq_len(sum(keep))]
          }
        }
        full_w <- as.numeric(full_w)
        full_w[!is.finite(full_w) | full_w < 0] <- 0
        w_sum <- sum(full_w)
        if (w_sum > 0) {
          full_w <- full_w / w_sum
          idx <- sample.int(nrow(theta_grid), size = nsamp, replace = TRUE, prob = full_w)
          theta <- theta_grid[idx, , drop = FALSE]

          if (lavmodel@ceq.simple.only) {
            theta <- theta %*% t(lavmodel@ceq.simple.K)
          }

          sx <- safe_theta_to_x(theta)
          theta <- sx$theta
          x <- sx$x
          colnames(theta) <- names(theta_star)
          colnames(x) <- names(theta_star)
          return(list(theta_samp = theta, x_samp = x))
        }
      }
    }
  }

  # ------- fallback R path -------
  Lt <- chol(R_use)
  z_raw <- matrix(rnorm(nsamp * m), nrow = nsamp)
  z <- z_raw %*% Lt
  u <- apply(z, 2, pnorm)
  if (!is.matrix(u)) {
    u <- matrix(u, nrow = 1)
  } # nocov – nsamp == 1 edge case

  # Use marginals to get theta
  if (method == "sampling") {
    D <- diag(sqrt(diag(Sigma_theta)))
    theta <- t(D %*% qnorm(t(u)) + theta_star)
  } else {
    if (method == "skewnorm") {
      approx_data_use <- align_approx_data(
        approx_data = approx_data,
        theta_star = theta_star,
        Sigma_theta = Sigma_theta,
        R_use = R_use
      )

      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          xi <- approx_data_use[j, "xi"]
          omega <- approx_data_use[j, "omega"]
          alpha <- approx_data_use[j, "alpha"]
          qsnorm_fast(u[, j], xi = xi, omega = omega, alpha = alpha)
        })
      )
    } else if (method == "asymgaus") {
      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          tt <- theta_star[j] +
            seq(-4, 4, length = 100) * sqrt(Sigma_theta[j, j])
          yy <- marg_lp(
            tt,
            j = j,
            theta_star = theta_star,
            Sigma_theta = Sigma_theta,
            sigma_asym = approx_data
          )
          yy <- yy - max(yy) # stabilise
          fj_lp <- stats::splinefun(tt, yy)
          fj <- function(par) exp(fj_lp(par)) # unnormalised
          dt <- diff(tt)
          ft <- fj(tt)
          fmid <- (head(ft, -1) + tail(ft, -1)) / 2
          ymid <- (head(tt, -1) + tail(tt, -1)) / 2
          C <- sum(fmid * dt)
          ft <- ft / C
          Ft <- c(0, cumsum(fmid * dt))
          Ft <- Ft / tail(Ft, 1)
          qfj <- splinefun(Ft, tt, method = "monoH.FC")
          qfj(u[, j])
        })
      )
    } else if (method == "marggaus") {
      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          mu_j <- theta_star[j]
          sd_j <- sqrt(Sigma_theta[j, j])
          qnorm(u[, j], mean = mu_j, sd = sd_j)
        })
      )
    }
  }

  if (lavmodel@ceq.simple.only) {
    theta <- theta %*% t(lavmodel@ceq.simple.K)
  }

  sx <- safe_theta_to_x(theta)
  theta <- sx$theta
  x <- sx$x

  if (length(names(theta_star)) == ncol(theta)) {
    colnames(theta) <- names(theta_star)
  }
  if (length(names(theta_star)) == ncol(x)) {
    colnames(x) <- names(theta_star)
  }

  list(theta_samp = theta, x_samp = x)
}

# 1. Get correlation matrix R = cov2cor(Sigma_theta)
# 2. Sample z ~ N(0, R)
# 3. Compute u = Phi^{-1}(z)
# 4. For each j, compute theta_j = F^{-1}_j(u_j) where F_j is the marginal posterior CDF for parameter j
# 5. Get x = pars_to_x(theta)
# 6. Compute implied covariance matrix Sigma(x)
# 7. Sample Srep ~ Wishart(n - 1, Sigma(x))
# 7. Repeat 1-6 and get samples of Sigma, and Srep
#
# Define F(S, Sigma) = log |Sigma| + trace(S Sigma^{-1}) - log |S| - p
# and the test statistic is T(S, Sigma) = (n - 1) / 2 * F(S, Sigma)
# Then ppp = P(T(Srep, Sigma) >= T(S, Sigma))

get_ppp <- function(
  x_samp,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavpartable = NULL,
  native_backend = NULL,
  cli_env = NULL
) {
  n_blocks <- lavmodel@nblocks
  n_levels <- lavdata@nlevels

  # Pre-compute per-block observed variable names (needed for alignment)
  ov_names_block <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    g <- ceiling(b / n_levels)
    ov_all <- lavdata@ov.names[[g]]
    if (!is.null(lavpartable)) {
      ov_names_block[[b]] <- unique(
        lavpartable$lhs[
          lavpartable$block == b &
            lavpartable$op == "~~" &
            lavpartable$lhs == lavpartable$rhs &
            lavpartable$lhs %in% ov_all
        ]
      )
    }
  }

  # Pre-compute per-block observed stats (S, n, logdet_S) — invariant across
  # samples, so we avoid redundant extraction and determinant calls in the loop.
  block_obs <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    g <- ceiling(b / n_levels)
    l <- (b - 1) %% n_levels + 1

    if (n_levels > 1) {
      # nocov start
      cluster_stats <- lavsamplestats@YLp[[g]][[2]]
      ov_all <- lavdata@ov.names[[g]]
      if (l == 1) {
        n <- lavdata@nobs[[g]]
        S <- cluster_stats$Sigma.W
      } else {
        n <- lavdata@Lp[[g]]$nclusters[[l]]
        S <- cluster_stats$Sigma.B
      }
      rownames(S) <- colnames(S) <- ov_all
      keep <- rowSums(S != 0) > 0
      S <- S[keep, keep, drop = FALSE]
      # nocov end
    } else {
      n <- lavsamplestats@nobs[[g]]
      S <- lavsamplestats@cov[[g]]
    }

    logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
    block_obs[[b]] <- list(S = S, n = n, logdet_S = logdet_S)
  }

  res <- vector("numeric", length = nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env) # nocov
    }

    xx <- x_samp[i, ]
    # The C++ implied-cov helper only supports single-level models (one block per
    # group). For multilevel models (nlevels > 1) the block list has entries for
    # each level, so fall back to lavaan's own computation which handles all cases.
    implied_cov <- if (!is.null(native_backend) &&
      n_levels == 1L &&
      identical(native_backend$type, "lisrel_ml_single_group")) {
      native_model_implied_cov_list(xx, native_backend)
    } else {
      lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
      lavaan::lav_model_implied(lavmodel_x)$cov
    }

    Tobs <- 0
    Trep <- 0

    for (b in seq_len(n_blocks)) {
      S <- block_obs[[b]]$S
      n <- block_obs[[b]]$n
      logdet_S <- block_obs[[b]]$logdet_S

      Sigma <- implied_cov[[b]]

      # Align dimensions for multilevel models
      if (n_levels > 1 && length(ov_names_block[[b]]) == nrow(Sigma)) {
        # nocov start
        rownames(Sigma) <- colnames(Sigma) <- ov_names_block[[b]]
        shared <- intersect(rownames(S), rownames(Sigma))
        if (length(shared) > 0) {
          S <- S[shared, shared, drop = FALSE]
          Sigma <- Sigma[shared, shared, drop = FALSE]
          logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
        }
        # nocov end
      }

      if (!is_bad_cov(Sigma)) {
        p <- nrow(Sigma)

        # Cholesky of Sigma — shared for logdet, inverse, and both discrepancies
        L <- chol(Sigma)
        logdet_Sigma <- 2 * sum(log(diag(L)))
        Sigma_inv <- chol2inv(L)

        # Wishart draw for replicated data
        W <- matrix(stats::rWishart(1, df = n - 1, Sigma = Sigma)[,, 1], p, p)
        Srep <- W / (n - 1)
        logdet_Srep <- as.numeric(determinant(Srep, logarithm = TRUE)$modulus)

        # F(S, Sigma) = log|Sigma| + tr(Sigma^{-1} S) - log|S| - p
        Tobs <- Tobs + logdet_Sigma + sum(Sigma_inv * S) - logdet_S - p
        Trep <- Trep + logdet_Sigma + sum(Sigma_inv * Srep) - logdet_Srep - p
      }
    }

    res[i] <- as.numeric(Trep >= Tobs)
  }

  mean(res)
}

sample_covariances <- function(x_samp, pt) {
  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  cov_samp <- x_samp[, idxcov, drop = FALSE]
  colnames(cov_samp) <- pt$names[pt_cov_free_rows]

  apply(cov_samp, 2, summarise_samples)
}

get_defpars <- function(x_samp, pt) {
  pt_def_rows <- which(pt$op == ":=")
  param_map <- setNames(pt$free[pt$free > 0], pt$label[pt$free > 0])
  param_map <- param_map[names(param_map) != ""]

  def_funs <- lapply(pt_def_rows, function(i) {
    expr <- parse(text = pt$rhs[i])

    function(theta) {
      env_vals <- as.list(theta[param_map])
      names(env_vals) <- names(param_map)
      eval(expr, envir = env_vals)
    }
  })

  # Apply each function in def_funs to every row of x_samp
  def_samp <- sapply(def_funs, function(fn) {
    apply(x_samp, 1, fn)
  })
  colnames(def_samp) <- pt$names[pt_def_rows]

  apply(def_samp, 2, summarise_samples)
}

get_defpars_fit_sn <- function(x_samp, pt) { # nocov start
  pt_def_rows <- which(pt$op == ":=")
  param_map <- setNames(pt$free[pt$free > 0], pt$label[pt$free > 0])
  param_map <- param_map[names(param_map) != ""]

  def_funs <- lapply(pt_def_rows, function(i) {
    expr <- parse(text = pt$rhs[i])

    function(theta) {
      env_vals <- as.list(theta[param_map])
      names(env_vals) <- names(param_map)
      eval(expr, envir = env_vals)
    }
  })

  def_samp <- sapply(def_funs, function(fn) {
    apply(x_samp, 1, fn)
  })
  colnames(def_samp) <- pt$names[pt_def_rows]

  sn_params <- apply(def_samp, 2, fit_skew_normal_samp)
  sn_params <- do.call("rbind", lapply(sn_params, unlist))
  sn_params <- cbind(sn_params, logC = 0, k = 0, rmse = 0, nmad = 0, gamma1 = 0)

  apply(sn_params, 1, function(y) {
    xi <- y["xi"]
    omega <- y["omega"]
    alpha <- y["alpha"]
    if (!all(is.finite(c(xi, omega, alpha)))) {
      res <- rep(NA_real_, 8L)
      names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
      return(list(
        summary = res,
        pdf_data = data.frame(x = numeric(0), y = numeric(0)),
        sn_params = c(
          xi = xi,
          omega = omega,
          alpha = alpha,
          logC = NA_real_,
          k = NA_real_,
          rmse = NA_real_,
          nmad = NA_real_,
          gamma1 = NA_real_
        )
      ))
    }
    delta <- alpha / sqrt(1 + alpha^2)

    Ex <- xi + omega * delta * sqrt(2 / pi)
    Vx <- omega^2 * (1 - 2 * delta^2 / pi)
    SDx <- safe_sd_from_var(Vx)
    qq <- qsnorm_fast(
      c(0.025, 0.25, 0.5, 0.75, 0.975),
      xi = xi,
      omega = omega,
      alpha = alpha
    )

    span <- max(SDx, abs(omega), sqrt(.Machine$double.eps))
    x <- seq(Ex - 4 * span, Ex + 4 * span, length.out = 200)
    fx <- dsnorm(x, xi = xi, omega = omega, alpha = alpha)

    xmax <- nlminb(
      start = Ex,
      objective = function(x) -dsnorm(x, xi = xi, omega = omega, alpha = alpha, log = TRUE),
      lower = range(x)[1],
      upper = range(x)[2]
    )$par

    res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

    list(
      summary = res,
      pdf_data = data.frame(x = x, y = fx),
      sn_params = c(
        xi = xi,
        omega = omega,
        alpha = alpha,
        logC = NA_real_,
        k = NA_real_,
        rmse = NA_real_,
        nmad = NA_real_,
        gamma1 = NA_real_
      )
    )
  })
} # nocov end

sample_covariances_fit_sn <- function(x_samp, pt) {
  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  cov_samp <- x_samp[, idxcov, drop = FALSE]
  colnames(cov_samp) <- pt$names[pt_cov_free_rows]

  sn_params <- apply(cov_samp, 2, fit_skew_normal_samp)
  sn_params <- do.call("rbind", lapply(sn_params, unlist))

  # FIXME: Repeated code in post_marg_skewnorm
  apply(sn_params, 1, function(y) {
    xi <- y["xi"]
    omega <- y["omega"]
    alpha <- y["alpha"]
    if (!all(is.finite(c(xi, omega, alpha)))) {
      res <- rep(NA_real_, 8L)
      names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
      return(list(
        summary = res,
        pdf_data = data.frame(x = numeric(0), y = numeric(0)),
        sn_params = c(
          xi = xi,
          omega = omega,
          alpha = alpha,
          logC = NA_real_,
          k = NA_real_,
          rmse = NA_real_,
          nmad = NA_real_,
          gamma1 = NA_real_
        )
      ))
    }
    delta <- alpha / sqrt(1 + alpha^2)

    Ex <- xi + omega * delta * sqrt(2 / pi)
    Vx <- omega^2 * (1 - 2 * delta^2 / pi)
    SDx <- safe_sd_from_var(Vx)
    qq <- qsnorm_fast(
      c(0.025, 0.25, 0.5, 0.75, 0.975),
      xi = xi,
      omega = omega,
      alpha = alpha
    )

    span <- max(SDx, abs(omega), sqrt(.Machine$double.eps))
    x <- seq(Ex - 4 * span, Ex + 4 * span, length.out = 200)
    fx <- dsnorm(x, xi = xi, omega = omega, alpha = alpha)

    xmax <- nlminb(
      start = Ex,
      objective = function(x) -dsnorm(x, xi = xi, omega = omega, alpha = alpha, log = TRUE),
      lower = range(x)[1],
      upper = range(x)[2]
    )$par

    res <- res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

    list(
      summary = res,
      pdf_data = data.frame(x = x, y = fx),
      sn_params = c(
        xi = xi,
        omega = omega,
        alpha = alpha,
        logC = NA_real_,
        k = NA_real_,
        rmse = NA_real_,
        nmad = NA_real_,
        gamma1 = NA_real_
      )
    )
  })
}

get_dic <- function(
  x_samp,
  theta_star,
  pt,
  lavmodel,
  loglik,
  cli_env = NULL
) {
  if (lavmodel@ceq.simple.only) {
    # nocov start
    xhat <- pars_to_x(as.numeric(lavmodel@ceq.simple.K %*% theta_star), pt,
                       compute_jac = FALSE)
  } else {
    # nocov end
    xhat <- pars_to_x(theta_star, pt, compute_jac = FALSE)
  }

  Dhat <- -2 * loglik(xhat)
  Dbar_samp <- sapply(seq_len(nrow(x_samp)), function(i) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env)
    }
    xx <- x_samp[i, ]
    -2 * loglik(xx)
  })
  Dbar <- mean(Dbar_samp[Dbar_samp < 1e40])
  pD <- Dbar - Dhat

  list(dic = Dbar + pD, Dbar = Dbar, Dhat = Dhat, pD = pD)
}


get_thetaparamerization_deltas <- function(x_samp, lavmodel) {
  delta_samp <- apply(x_samp, 1, function(xx) {
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x, delta = FALSE)
    Sigma_list <- lavimplied$cov
    ngroups <- length(Sigma_list)
    delta_list <- vector("list", ngroups)
    for (g in seq_len(ngroups)) {
      delta_list[[g]] <- sqrt(diag(Sigma_list[[g]]))
    }
    unlist(delta_list)
  })

  apply(delta_samp, 1, summarise_samples)
}
