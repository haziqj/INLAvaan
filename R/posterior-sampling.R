sample_params <- function(
  theta_star,
  Sigma_theta,
  method,
  approx_data,
  pt,
  lavmodel,
  nsamp = 1000,
  return_theta = FALSE
) {
  R <- cov2cor(Sigma_theta)
  z <- mvtnorm::rmvnorm(n = nsamp, sigma = R)
  u <- apply(z, 2, pnorm)

  # FIXME: Repeated code in post_marg_skewnorm and post_marg_marggaus
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
          sn::qsn(u[, j], xi = xi, omega = omega, alpha = alpha)
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

  if (return_theta) {
    return(theta)
  } else {
    x <- t(apply(theta, 1, pars_to_x, pt = pt))
    return(x)
  }
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
  theta_star,
  Sigma_theta,
  method,
  approx_data,
  pt,
  lavmodel,
  lavsamplestats,
  nsamp = 250,
  cli_env = NULL
) {
  x <- sample_params(
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    method = method,
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp,
    return_theta = FALSE
  )

  #log |Sigma| + trace(S Sigma^{-1}) - log |S| - p
  Fdiscrp <- function(S, Sigma) {
    logdet_Sigma <- as.numeric(determinant(Sigma, logarithm = TRUE)$modulus)
    logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
    trace_term <- sum(diag(solve(Sigma, S)))
    logdet_Sigma + trace_term - logdet_S - nrow(S)
  }

  nG <- max(pt$group)
  res <- vector("numeric", length = nrow(x))
  for (i in seq_len(nrow(x))) {
    if (!is.null(cli_env)) {
      cli::cli_progress_update(.envir = cli_env)
    }
    xx <- x[i, ]
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x)

    Tobs <- 0
    Trep <- 0

    for (g in seq_len(nG)) {
      n <- lavsamplestats@nobs[[g]]
      S <- lavsamplestats@cov[[g]]
      Sigma <- lavimplied$cov[[g]]
      if (check_mat(Sigma)) {
        next
      }

      W <- stats::rWishart(1, df = n - 1, Sigma = Sigma)[,, 1]
      Srep <- W / (n - 1)

      Tobs <- Tobs + Fdiscrp(S, Sigma)
      Trep <- Trep + Fdiscrp(Srep, Sigma)
    }

    res[i] <- as.numeric(Trep >= Tobs)
  }

  mean(res)
}

sample_covariances <- function(theta, Sigma_theta, pt, K, nsamp = 1000) {
  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  theta_samp <- mvtnorm::rmvnorm(nsamp, mean = theta, sigma = Sigma_theta)
  if (!all(dim(K) == 0)) {
    theta_samp <- t(apply(theta_samp, 1, function(pars) as.numeric(K %*% pars)))
  }
  x_samp <- apply(theta_samp, 1, pars_to_x, pt = pt)

  cov_samp <- x_samp[idxcov, , drop = FALSE]
  rownames(cov_samp) <- pt$names[pt_cov_free_rows]

  # FIXME: Repeated code in post_marg_sampling
  apply(cov_samp, 1, function(y) {
    Ex <- mean(y)
    SDx <- sd(y)
    qq <- quantile(y, probs = c(0.025, 0.5, 0.975))
    dens <- density(y)
    xmax <- dens$x[which.max(dens$y)]
    res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

    list(
      summary = res,
      pdf_data = data.frame(x = dens$x, y = dens$y)
    )
  })
}

sample_covariances_fit_sn <- function(
  theta,
  Sigma_theta,
  pt,
  K,
  nsamp = 10000
) {
  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  theta_samp <- mvtnorm::rmvnorm(nsamp, mean = theta, sigma = Sigma_theta)
  if (!all(dim(K) == 0)) {
    theta_samp <- t(apply(theta_samp, 1, function(pars) as.numeric(K %*% pars)))
  }
  x_samp <- apply(theta_samp, 1, pars_to_x, pt = pt)
  cov_samp <- x_samp[idxcov, , drop = FALSE]
  rownames(cov_samp) <- pt$names[pt_cov_free_rows]

  sn_params <- apply(cov_samp, 1, fit_skew_normal_samp)
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
    qq <- sn::qsn(
      c(0.025, 0.5, 0.975),
      xi = xi,
      omega = omega,
      alpha = alpha,
      solver = "RFB"
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
      pdf_data = data.frame(x = x, y = fx)
    )
  })
}

get_dic <- function(
  theta_star,
  Sigma_theta,
  method,
  approx_data,
  pt,
  lavmodel,
  lavsamplestats,
  loglik,
  nsamp = 250,
  cli_env = NULL
) {
  x <- sample_params(
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    method = method,
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp,
    return_theta = FALSE
  )

  if (lavmodel@ceq.simple.only) {
    xhat <- pars_to_x(as.numeric(lavmodel@ceq.simple.K %*% theta_star), pt)
  } else {
    xhat <- pars_to_x(theta_star, pt)
  }

  Dhat <- -2 * loglik(xhat)
  Dbar <- mean(sapply(seq_len(nrow(x)), function(i) {
    if (!is.null(cli_env)) {
      cli::cli_progress_update(.envir = cli_env)
    }
    xx <- x[i, ]
    -2 * loglik(xx)
  }))
  pD <- Dbar - Dhat

  list(dic = Dbar + pD, Dbar = Dbar, Dhat = Dhat, pD = pD)
}
