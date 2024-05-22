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
    idx_psi   <- partable$free[partable$mat == "psi" & partable$free > 0]
    idx_rho   <- partable$free[partable$mat == "rho" & partable$free > 0]

    lambda <- theta[idx_lam]
    beta <- theta[idx_beta]
    theta_e <- exp(theta[idx_theta])
    psi <- exp(theta[idx_psi])
    rho <- exp(theta[idx_rho])
    rho <- rho / (1 + rho)

    list(
      lambda = lambda,
      beta = beta,
      theta_e = theta_e,
      psi = psi,
      rho = rho
    )
  }

  Q <- function() {
    params <- interpret.theta()

    # Lambda matrix
    Lam_df <- partable[partable$mat == "lambda", ]
    Lam_df$est[Lam_df$free > 0] <- params$lambda
    Lambda <- with(Lam_df, sparseMatrix(
      i = row,
      j = col,
      x = est,
      dims = c(max(row), max(col))
    ))

    # B matrix
    B_df <- partable[partable$mat == "beta", ]
    B_df$est[B_df$free > 0] <- params$beta
    B <- with(B_df, sparseMatrix(
      i = row,
      j = col,
      x = est,
      dims = rep(max(c(row, col)), 2)
    ))

    # Theta matrix
    SD <- Diagonal(x = params$theta_e)
    Rho_df <- partable[partable$mat == "rho", ]
    Rho_df$est[Rho_df$free > 0] <- params$rho
    Rho <- with(Rho_df, sparseMatrix(
      i = row,
      j = col,
      x = est,
      dims = rep(length(params$theta_e), 2),
      symmetric = TRUE
    ))
    Rho <- Rho + diag(1, nrow = length(params$theta_e))
    Theta <- SD %*% Rho %*% SD

    # Psi matrix
    Psi_df <- partable[partable$mat == "psi", ]
    Psi_df$est[Psi_df$free > 0] <- params$psi
    Psi <- with(Psi_df, sparseMatrix(
      i = row,
      j = col,
      x = est,
      dims = rep(max(c(row, col)), 2),
      symmetric = TRUE
    ))

    # Using Woodbury Matrix identity
    # I_q <- diag(q)
    # inv_I_minus_B <- solve(I_q - B)
    # U <- Lambda %*% inv_I_minus_B
    # C_inv <- diag(x = 1 / params$psi)  # solve(Psi)
    # A_inv <- diag(x = 1 / params$theta_e)
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

    # FIXME: Adjust priors in the future
    res <-
      sum(dnorm(params$lambda, mean = 0, sd = 10, log = TRUE)) +
      sum(dnorm(params$beta, mean = 0, sd = 10, log = TRUE)) +
      sum(dgamma(params$theta_e, shape = 1, rate = 5e-05, log = TRUE)) +
      sum(dgamma(params$psi, shape = 1, rate = 5e-05, log = TRUE)) +
      sum(dbeta(params$rho, shape1 = 1, shape2 = 1, log = TRUE))
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
