# ---------------------------------------------------------------------------
# Shared helper: summarise a numeric vector of posterior samples into the
# standard (Mean, SD, 2.5%, 50%, 97.5%, Mode) list + pdf_data table.
# ---------------------------------------------------------------------------
summarise_samples <- function(y) {
  Ex <- mean(y)
  SDx <- stats::sd(y)
  qq <- stats::quantile(y, probs = c(0.025, 0.5, 0.975))
  dens <- stats::density(y)
  xmax <- dens$x[which.max(dens$y)]
  res <- c(Ex, SDx, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")
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
  R_star = NULL # NORTA-adjusted correlation matrix for SN copula
) {
  R <- cov2cor(Sigma_theta)
  R_use <- if (method == "skewnorm" && !is.null(R_star)) R_star else R
  Lt <- chol(R_use)
  z_raw <- matrix(rnorm(nsamp * ncol(R)), nrow = nsamp)
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
      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          xi <- approx_data[j, "xi"]
          omega <- approx_data[j, "omega"]
          alpha <- approx_data[j, "alpha"]
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
    K <- lavmodel@ceq.simple.K
    theta <- t(apply(theta, 1, function(pars) as.numeric(K %*% pars)))
  }

  x <- t(apply(theta, 1, pars_to_x, pt = pt))

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
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x)

    Tobs <- 0
    Trep <- 0

    for (b in seq_len(n_blocks)) {
      S <- block_obs[[b]]$S
      n <- block_obs[[b]]$n
      logdet_S <- block_obs[[b]]$logdet_S

      Sigma <- lavimplied$cov[[b]]

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

get_defpars_fit_sn <- function(x_samp, pt) {
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
    delta <- alpha / sqrt(1 + alpha^2)

    Ex <- xi + omega * delta * sqrt(2 / pi)
    Vx <- omega^2 * (1 - 2 * delta^2 / pi)
    SDx <- sqrt(Vx)
    qq <- qsnorm_fast(
      c(0.025, 0.5, 0.975),
      xi = xi,
      omega = omega,
      alpha = alpha
    )

    x <- seq(Ex - 4 * SDx, Ex + 4 * SDx, length.out = 200)
    fx <- dsnorm(x, xi = xi, omega = omega, alpha = alpha)

    xmax <- optimize(
      function(x) dsnorm(x, xi = xi, omega = omega, alpha = alpha),
      interval = range(x),
      maximum = TRUE
    )$maximum

    res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

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
    delta <- alpha / sqrt(1 + alpha^2)

    Ex <- xi + omega * delta * sqrt(2 / pi)
    Vx <- omega^2 * (1 - 2 * delta^2 / pi)
    SDx <- sqrt(Vx)
    qq <- qsnorm_fast(
      c(0.025, 0.5, 0.975),
      xi = xi,
      omega = omega,
      alpha = alpha
    )

    x <- seq(Ex - 4 * SDx, Ex + 4 * SDx, length.out = 200)
    fx <- dsnorm(x, xi = xi, omega = omega, alpha = alpha)

    xmax <- optimize(
      function(x) dsnorm(x, xi = xi, omega = omega, alpha = alpha),
      interval = range(x),
      maximum = TRUE
    )$maximum

    res <- res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

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
    xhat <- pars_to_x(as.numeric(lavmodel@ceq.simple.K %*% theta_star), pt)
  } else {
    # nocov end
    xhat <- pars_to_x(theta_star, pt)
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
