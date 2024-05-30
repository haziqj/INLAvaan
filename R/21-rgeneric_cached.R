inla_sem_cached <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"),
    theta = NULL,
    .debug = FALSE) {

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

    force_pd <- function(x) {
      ed <- eigen(x, symmetric = TRUE, only.values = TRUE)
      if (any (ed$values < 0)) {
        ed <- eigen(x, symmetric = TRUE)
        eval <- ed$values
        evec <- ed$vectors
        eval[eval < 0] <- .Machine$double.eps
        out <- evec %*% diag(eval) %*% t(evec)
      } else {
        out <- x
      }
      out
    }
    assign("force_pd", force_pd, envir = envir)

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
    Theta <- diag(subset(partable, mat == "theta" & row == col)$start)
    RHO_IDX <- cbind(Rho_df$row, Rho_df$col)
    assign("RHO_IDX", RHO_IDX, envir = envir)
    assign("Theta", Theta, envir = envir)

    # LVRho and Psi matrix
    LVRho_df <- partable[partable$mat == "lvrho", ]
    Psi <- diag(subset(partable, mat == "psi" & row == col)$start)
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

  Q <- function(debug = .debug) {
    params <- interpret.theta()

    # The matrices are cached and updated here
    Lambda[LAM_IDX] <- params$lambda
    if (length(idx_beta) > 0) IminB[B_IDX] <- -params$beta

    # Theta matrix
    diag(Theta) <- params$sd_e ^ 2
    if (length(idx_rho) > 0) {
      I <- RHO_IDX[, 1]
      J <- RHO_IDX[, 2]
      values <- params$rho * params$sd_e[I] * params$sd_e[J]
      Theta[cbind(I, J)] <- values
      Theta[cbind(J, I)] <- values

      Theta <- force_pd(Theta)  # force pd
    }

    # Psi matrix
    diag(Psi) <- params$sd_z ^ 2
    if (length(idx_lvrho) > 0) {
      I <- LVRHO_IDX[, 1]
      J <- LVRHO_IDX[, 2]
      values <- params$lvrho * params$sd_z[I] * params$sd_z[J]
      Psi[cbind(I, J)] <- values
      Psi[cbind(J, I)] <- values

      Psi <- force_pd(Psi)  # force pd
    }

    if (isTRUE(debug)) {
      return(list(
        Lambda = Lambda,
        IminB = IminB,
        Theta = Theta,
        Psi = Psi
      ))
    }

    # Return inverse Sigma
    if (is.matrix(IminB)) {
      front <- Lambda %*% solve(IminB)
    } else {
      front <- Lambda
    }
    Sigma <- front %*% tcrossprod(Psi, front) + Theta
    Sigma <- Sigma + 1e-10 * diag(nrow(Sigma))
    solve(Sigma)
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
      pos_only <- FALSE
      u <- 1 / (1 + exp(-x))
      log_beta_density <- dbeta(u, shape1, shape2, log = TRUE)
      if (pos_only) {
        log_jacobian <- 0
      } else {
        log_jacobian <- -(log(2) + log(u) + log(1-u)) # log(Jacobian) = theta - 2 * log(1 + exp(theta))
      }

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
    matrix(1, nrow = p, ncol = p)
  }

  quit = function() { return(invisible()) }

  if (!length(theta)) theta = initial()
  val = do.call(match.arg(cmd), args = list())
  return (val)
}
