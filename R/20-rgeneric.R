inla_sem <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL) {

  # In the environment require
  # - n (sample size)
  # - p (no of items)
  # - q (no of factors)
  # - init (initial values)
  # - partable

  interpret.theta <- function() {

    idx_lam   <- partable$free[partable$mat == "lambda" & partable$free > 0]
    idx_beta  <- partable$free[partable$mat == "beta" & partable$free > 0]
    idx_theta <- partable$free[partable$mat == "theta" & partable$free > 0]
    idx_rho   <- partable$free[partable$mat == "rho" & partable$free > 0]
    idx_psi   <- partable$free[partable$mat == "psi" & partable$free > 0]
    idx_lvrho <- partable$free[partable$mat == "lvrho" & partable$free > 0]

    lambda <- theta[idx_lam]
    beta <- theta[idx_beta]
    sd_e <- sqrt(exp(theta[idx_theta]))  # sd_e = sd_e ^ 2 (item sd)
    rho <- 1 / (1 + exp(-theta[idx_rho]))
    sd_z <- sqrt(exp(theta[idx_psi]))  # sd_z = sd_z ^ 2 (latent sd)
    lvrho <- 1 / (1 + exp(-theta[idx_lvrho]))

    list(
      lambda = lambda,
      beta = beta,
      sd_e = sd_e,
      rho = rho,
      sd_z = sd_z,
      lvrho = lvrho
    )
  }

  Q <- function() {
    params <- interpret.theta()

    # Lambda matrix
    Lam_df <- partable[partable$mat == "lambda", ]
    Lam_df$est[Lam_df$free > 0] <- params$lambda
    Lambda <- matrix(0, nrow = max(Lam_df$row), ncol = max(Lam_df$col))
    Lambda[cbind(Lam_df$row, Lam_df$col)] <- Lam_df$est

    # B matrix
    B_df <- partable[partable$mat == "beta", ]
    B_df$est[B_df$free > 0] <- params$beta
    if (length(params$beta) > 0) {
      sizeB <- max(B_df$row, B_df$col)
      B <- matrix(0, nrow = sizeB, ncol = sizeB)
      B[cbind(B_df$row, B_df$col)] <- B_df$est
    } else {
      B <- 0
    }

    # Theta matrix
    if (length(params$rho) > 0) {
      SD <- diag(x = params$sd_e)
      Rho_df <- partable[partable$mat == "rho", ]
      Rho_df$est[Rho_df$free > 0] <- params$rho
      sizeRho <- length(params$sd_e)
      Rho <- matrix(0, nrow = sizeRho, ncol = sizeRho)
      Rho[cbind(Rho_df$row, Rho_df$col)] <- Rho_df$est
      Rho <- Rho + t(Rho)
      diag(Rho) <- 1
      Theta <- SD %*% Rho %*% SD
    } else {
      Theta <- diag(params$sd_e ^ 2)
    }


    # Psi matrix
    if (length(params$lvrho) > 0) {
      SD <- diag(x = params$sd_z)
      LVRho_df <- partable[partable$mat == "lvrho", ]
      LVRho_df$est[LVRho_df$free > 0] <- params$lvrho
      sizeLVRho <- length(params$sd_z)
      LVRho <- matrix(0, nrow = sizeLVRho, ncol = sizeLVRho)
      LVRho[cbind(LVRho_df$row, LVRho_df$col)] <- LVRho_df$est
      LVRho <- LVRho + t(LVRho)
      diag(LVRho) <- 1
      Psi <- SD %*% LVRho %*% SD
    } else {
      Psi <- diag(params$sd_z ^ 2)
    }

    # Using Woodbury Matrix identity
    # I_q <- diag(q)
    # inv_I_minus_B <- solve(I_q - B)
    # U <- Lambda %*% inv_I_minus_B
    # C_inv <- diag(x = 1 / params$psi)  # solve(Psi)
    # A_inv <- diag(x = 1 / params$sd_e)
    # int_mat <- C_inv + t(U) %*% A_inv %*% U
    # Sigma_inv <- A_inv - A_inv %*% U %*% solve(int_mat) %*% t(U) %*% A_inv

    # Regular way
    front <- Lambda %*% solve(diag(1, nrow = q) - B)
    Sigma <- front %*% Psi %*% t(front) + Theta
    solve(Sigma)
  }

  mu <- function() {
    return(numeric(0))
  }

  log.norm.const = function() { numeric(0) }

  log.prior = function() {
    params <- interpret.theta()

    # If sigma ~ gamma(shape,rate) then this is the logpdf transform of theta = log(sigma^2)
    log_pdf_scale <- function(x, shape = 1, rate = 0.5) {
      sigma <- sqrt(exp(x))
      gamma_density <- dgamma(sigma, shape, rate, log = TRUE)
      log_jacobian <- x
      gamma_density + log_jacobian
    }

    # If rho ~ beta(a,b) then this is the logpdf of theta = log(rho / (1 - rho))
    log_pdf_rho <- function(x, shape1 = 1, shape2 = 1) {
      rho <- exp(x) / (1 + exp(x))
      log_beta_density <- dbeta(rho, shape1, shape2, log = TRUE)
      log_jacobian <- x - 2 * log1p(exp(x)) # log(Jacobian) = theta - 2 * log(1 + exp(theta))
      log_beta_density + log_jacobian
    }

    # FIXME: Adjust priors in the future
    res <-
      sum(dnorm(params$lambda, mean = 0, sd = 10, log = TRUE)) +
      sum(dnorm(params$beta, mean = 0, sd = 10, log = TRUE)) +
      # Variances
      # sum(dgamma(params$sd_e, shape = 1, rate = 5e-05, log = TRUE)) +
      # sum(dbeta(params$rho, shape1 = 1, shape2 = 1, log = TRUE)) +
      sum(log_pdf_scale(params$sd_e, shape = 1, rate = 0.5)) +
      sum(log_pdf_rho(params$rho, shape1 = 1, shape2 = 1)) +
      # sum(dgamma(params$sd_z, shape = 1, rate = 5e-05, log = TRUE)) +
      # sum(dbeta(params$lvrho, shape1 = 1, shape2 = 1, log = TRUE)) +
      sum(log_pdf_scale(params$sd_z, shape = 1, rate = 0.5)) +
      sum(log_pdf_rho(params$lvrho, shape1 = 1, shape2 = 1))
    res
  }

  initial = function() {
    init
  }

  graph <- function() {
    Q <- matrix(1, nrow = p, ncol = p)
    INLA::inla.as.sparse(Q)
  }

  quit = function() { return(invisible()) }

  if (!length(theta)) theta = initial()
  val = do.call(match.arg(cmd), args = list())
  return (val)
}
