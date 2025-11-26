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

get_ppp <- function(theta_star, Sigma_theta, sn_params, pt, lavmodel,
                    lavsamplestats, nsamp = 250) {
  n <- lavsamplestats@nobs[[1]]
  S <- lavsamplestats@cov[[1]]

  R <- cov2cor(Sigma_theta)
  z <- mvtnorm::rmvnorm(n = nsamp, sigma = R)
  u <- apply(z, 2, pnorm)

  theta <- do.call("cbind", lapply(seq_len(ncol(u)), function(j) {
    xi    <- sn_params[j, "xi"]
    omega <- sn_params[j, "omega"]
    alpha <- sn_params[j, "alpha"]
    sn::qsn(u[, j], xi = xi, omega = omega, alpha = alpha)
  }))

  x <- t(apply(theta, 1, pars_to_x, pt = pt))

  Sigma_list <- lapply(seq_len(nrow(x)), function(i) {
    xx <- x[i, ]
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x)
    Sigma <- lavimplied$cov[[1]]
    Sigma
  })

  Srep <- lapply(Sigma_list, function(Sigma) {
    W <- stats::rWishart(1, n - 1, Sigma)[,,1]
    W / (n - 1)
  })

  #log |Sigma| + trace(S Sigma^{-1}) - log |S| - p
  Fdiscrp <- function(S, Sigma) {
    logdet_Sigma <- as.numeric(determinant(Sigma, logarithm = TRUE)$modulus)
    logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
    trace_term <- sum(diag(solve(Sigma, S)))
    logdet_Sigma + trace_term - logdet_S - nrow(S)
  }

  TT <- Map(
    function(Srep, Sigma) {
      Tobs <- Fdiscrp(S, Sigma)
      Trep <- Fdiscrp(Srep, Sigma)
      as.numeric(Trep >= Tobs)
    },
    Srep = Srep,
    Sigma = Sigma_list
  )
  mean(unlist(TT))

}

sample_covariances <- function(theta, Sigma_theta, pt, nsamp = 1000) {

  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  theta_samp <- mvtnorm::rmvnorm(nsamp, mean = theta, sigma = Sigma_theta)
  x_samp <- apply(theta_samp, 1, pars_to_x, pt = pt)

  cov_samp <- x_samp[idxcov, , drop = FALSE]
  rownames(cov_samp) <- pt$names[pt_cov_free_rows]

  # RES
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

sample_covariances_fit_sn <- function(theta, Sigma_theta, pt, nsamp = 10000) {
  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  theta_samp <- mvtnorm::rmvnorm(nsamp, mean = theta, sigma = Sigma_theta)
  x_samp <- apply(theta_samp, 1, pars_to_x, pt = pt)
  cov_samp <- x_samp[idxcov, , drop = FALSE]
  rownames(cov_samp) <- pt$names[pt_cov_free_rows]

  sn_params <- apply(cov_samp, 1, fit_skew_normal_samp)
  sn_params <- do.call("rbind", lapply(sn_params, unlist))

  apply(sn_params, 1, function(y) {
    xi <- y["xi"]
    omega <- y["omega"]
    alpha <- y["alpha"]
    delta <- alpha / sqrt(1 + alpha ^ 2)

    Ex <- xi + omega * delta * sqrt(2 / pi)
    Vx <- omega ^ 2 * (1 - 2 * delta ^ 2 / pi)
    SDx <- sqrt(Vx)
    qq <- sn::qsn(c(0.025, 0.5, 0.975), xi = xi, omega = omega, alpha = alpha)

    x  <- seq(Ex - 4 * SDx, Ex + 4 * SDx, length.out = 200)
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
