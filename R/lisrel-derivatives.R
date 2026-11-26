# Derivatives of a function of the implied moments of one LISREL block with
# respect to the model matrices of that block. lisrel_df_dmlist() is lavaan's
# lav_lisrel_df_dmlist(), and lisrel_ibinv() its lav_lisrel_ibinv(), with the
# same arithmetic. `omega` is -2 times the derivative with respect to Sigma
# (every element taken on its own) and `omega_mu` -1 times the derivative with
# respect to mu, the conventions of lavaan's ML gradient.

# (I - B)^-1, with the shortcuts lavaan takes for a triangular or acyclic B
lisrel_ibinv <- function(mlist) {
  beta <- mlist$beta
  nr <- nrow(mlist$psi)
  if (is.null(beta)) {
    return(diag(nr))
  }
  diag(beta) <- 0
  if (is.complex(beta) || anyNA(beta)) {
    tmp <- -beta
    diag(tmp) <- 1
    return(solve(tmp))
  }
  if (all(beta == 0)) {
    return(diag(nr))
  }
  if (all(beta[upper.tri(beta)] == 0)) {
    return(forwardsolve(diag(nr) - beta, diag(nr)))
  }
  if (all(beta[lower.tri(beta)] == 0)) {
    return(backsolve(diag(nr) - beta, diag(nr)))
  }
  # A topological order of the paths, if there is one
  indegree <- rowSums(beta != 0)
  result <- integer(nr)
  k <- 0L
  queue <- which(indegree == 0)
  while (length(queue) > 0L) {
    v <- queue[1L]
    queue <- queue[-1L]
    k <- k + 1L
    result[k] <- v
    for (u in which(beta[, v] != 0)) {
      indegree[u] <- indegree[u] - 1L
      if (indegree[u] == 0L) {
        queue <- c(queue, u)
      }
    }
  }
  if (k == nr) {
    ib_inv <- forwardsolve(diag(nr) - beta[result, result], diag(nr))
    inv_order <- order(result)
    return(ib_inv[inv_order, inv_order, drop = FALSE])
  }
  tmp <- -beta
  diag(tmp) <- 1
  solve(tmp)
}

lisrel_df_dmlist <- function(mlist, omega, omega_mu = NULL) {
  lambda <- mlist$lambda
  psi <- mlist$psi
  beta <- mlist$beta
  alpha <- mlist$alpha
  if (is.null(beta)) {
    lambda_ib_inv <- lambda
  } else {
    ib_inv <- lisrel_ibinv(mlist)
    lambda_ib_inv <- lambda %*% ib_inv
  }
  meanstructure <- !is.null(omega_mu)
  t_lambda_ib_inv <- t(lambda_ib_inv)
  if (!is.null(beta)) {
    omega_lip <- omega %*% lambda_ib_inv %*% psi %*% t(ib_inv)
    lambda_deriv <- if (meanstructure) {
      -1 * (omega_mu %*% t(alpha) %*% t(ib_inv) + omega_lip)
    } else {
      -1 * omega_lip
    }
    beta_deriv <- if (meanstructure) {
      -1 *
        ((t(ib_inv) %*% (t(lambda) %*% omega_mu %*% t(alpha)) %*% t(ib_inv)) +
          (t_lambda_ib_inv %*% omega_lip))
    } else {
      -1 * (t_lambda_ib_inv %*% omega_lip)
    }
  } else {
    omega_lambda <- omega %*% lambda
    lambda_deriv <- if (meanstructure) {
      -1 * (omega_mu %*% t(alpha) + omega_lambda %*% psi)
    } else {
      -1 * (omega_lambda %*% psi)
    }
    beta_deriv <- NULL
  }
  psi_deriv <- -1 * (t_lambda_ib_inv %*% omega %*% lambda_ib_inv)
  diag(psi_deriv) <- 0.5 * diag(psi_deriv)
  theta_deriv <- -1 * omega
  diag(theta_deriv) <- 0.5 * diag(theta_deriv)
  nu_deriv <- alpha_deriv <- NULL
  if (meanstructure) {
    nu_deriv <- -1 * omega_mu
    alpha_deriv <- -1 * t(t(omega_mu) %*% lambda_ib_inv)
  }
  list(
    lambda = lambda_deriv,
    wmat = NULL,
    beta = beta_deriv,
    theta = theta_deriv,
    psi = psi_deriv,
    nu = nu_deriv,
    alpha = alpha_deriv,
    gw = if (!is.null(mlist$gw)) 0 else NULL
  )
}
