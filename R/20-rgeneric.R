inla_sem <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL) {

  # In the environment require
  # - n (sample size)
  # - p (no of items)
  # - q (no of factors)
  # - init (initial values)
  # - partable

  # CACHE
  envir <- parent.env(environment())
  if (!exists("INLAvaan_SEM_cache", envir = envir)) {

    # FIXME: These are indices for free parameters theta. But sometimes, with
    # fixed parameter values we still need the indices...

    # Indices
    idx_lam   <- partable$free[partable$mat == "lambda" & partable$free > 0]
    idx_beta  <- partable$free[partable$mat == "beta" & partable$free > 0]
    idx_theta <- partable$free[partable$mat == "theta" & partable$free > 0]
    idx_rho   <- partable$free[partable$mat == "rho" & partable$free > 0]
    idx_psi   <- partable$free[partable$mat == "psi" & partable$free > 0]
    idx_lvrho <- partable$free[partable$mat == "lvrho" & partable$free > 0]
    assign("idx_lam", idx_lam, envir = envir)
    assign("idx_beta", idx_beta, envir = envir)
    assign("idx_theta", idx_theta, envir = envir)
    assign("idx_rho", idx_rho, envir = envir)
    assign("idx_psi", idx_psi, envir = envir)
    assign("idx_lvrho", idx_lvrho, envir = envir)

    # Lambda matrix
    Lam_df <- partable[partable$mat == "lambda", ]
    Lambda <- matrix(0, nrow = max(Lam_df$row), ncol = max(Lam_df$col))
    Lambda[cbind(Lam_df$row, Lam_df$col)] <- Lam_df$est
    LAM_IDX <- as.matrix(Lam_df[Lam_df$free > 0, c("row", "col")])
    assign("Lambda", Lambda, envir = envir)
    assign("LAM_IDX", LAM_IDX, envir = envir)

    # (I-B) matrix
    B_df <- partable[partable$mat == "beta", ]
    B_IDX <- as.matrix(B_df[B_df$free > 0, c("row", "col")])
    IminB <- 1
    if (length(idx_beta) > 0) {
      IminB <- diag(q)
      IminB[B_IDX] <- -B_df$est
    }
    assign("IminB", IminB, envir = envir)
    assign("B_IDX", B_IDX, envir = envir)

    # Rho and Theta matrix
    Rho_df <- partable[partable$mat == "rho", ]
    Theta <- diag(p)
    RHO_IDX <- cbind(Rho_df$row, Rho_df$col)
    assign("RHO_IDX", RHO_IDX, envir = envir)
    assign("Theta", Theta, envir = envir)

    # LVRho and Psi matrix
    LVRho_df <- partable[partable$mat == "lvrho", ]
    Psi <- diag(q)
    LVRHO_IDX <- cbind(LVRho_df$row, LVRho_df$col)
    assign("LVRHO_IDX", LVRHO_IDX, envir = envir)
    assign("Psi", Psi, envir = envir)

    assign("INLAvaan_SEM_cache", TRUE, envir = envir)
  }

  interpret.theta <- function() {

    lambda <- theta[idx_lam]
    beta <- theta[idx_beta]
    sd_e <- sqrt(exp(theta[idx_theta]))  # sd_e = sd_e ^ 2 (item sd)
    rho <- theta_to_rho(theta[idx_rho])
    sd_z <- sqrt(exp(theta[idx_psi]))  # sd_z = sd_z ^ 2 (latent sd)
    lvrho <- theta_to_rho(theta[idx_lvrho])

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

    # The matrices are cached and updated here
    Lambda[LAM_IDX] <- params$lambda
    if (length(idx_beta) > 0) IminB[B_IDX] <- -params$beta

    # Theta matrix
    diag(Theta) <- params$sd_e ^ 2
    if (length(idx_rho) > 0) {
      for (k in seq_len(nrow(RHO_IDX))) {
        i <- RHO_IDX[k, 1]
        j <- RHO_IDX[k, 2]
        Theta[i, j] <- Theta[j, i] <-
          params$rho[k] * params$sd_e[i] * params$sd_e[j]
      }
    }

    # Psi matrix
    diag(Psi) <- params$sd_z ^ 2
    if (length(idx_lvrho) > 0) {
      for (k in seq_len(nrow(LVRHO_IDX))) {
        i <- LVRHO_IDX[k, 1]
        j <- LVRHO_IDX[k, 2]
        Psi[i, j] <- Psi[j, i] <-
          params$lvrho[k] * params$sd_z[i] * params$sd_z[j]
      }
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
    if (is.vector(IminB)) {
      front <- Lambda
    } else {
      front <- Lambda %*% solve(IminB)
    }
    Sigma <- front %*% tcrossprod(Psi, front) + Theta
    Sigma <- Sigma + diag(1e-10, nrow(Sigma))  # for stability
    chol2inv(chol(Sigma))
    # MASS::ginv(Sigma)
  }

  mu <- function() { numeric(0) }

  log.norm.const <- function() { numeric(0) }

  log.prior = function() {
    params <- interpret.theta()

    # If sigma ~ gamma(shape,rate) then this is the logpdf transform of theta = log(sigma^2)
    log_pdf_scale <- function(x, shape = 1, rate = 0.5) {
      dgamma(sqrt(exp(x)), shape, rate, log = TRUE) + x
    }

    # If rho ~ beta(a,b) then this is the logpdf of theta = log(rho / (1 - rho))
    log_pdf_rho <- function(x, shape1 = 1, shape2 = 1) {
      u <- 1 / (1 + exp(-x))
      log_beta_density <- dbeta(u, shape1, shape2, log = TRUE)
      log_jacobian <- -log(2 * u * (1 - u)) # log(Jacobian) = theta - 2 * log(1 + exp(theta))
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
