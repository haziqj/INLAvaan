# ==============================================================================
# Fast Laplace-within-Laplace: 1-factor, 5 binary items (probit CFA)
# - Uses response-pattern collapsing (<= 32 patterns)
# - Uses warm-started inner Newton per pattern
# - Uses caching so nlminb doesn't re-run inner opt for obj + grad at same theta
# - FIXES the Hessian sign bug in get_eta_mode()
# ==============================================================================

set.seed(123)

# ==============================================================================
# 1) SETUP & SIMULATION
# ==============================================================================
N <- 1000
n_items <- 5
J <- n_items

true_lambda <- rep(1.5, J)
true_tau <- seq(-0.5, 0.5, length.out = J)

eta_true <- rnorm(N)
data <- matrix(0L, N, J)
for (j in 1:J) {
  ystar <- true_lambda[j] * eta_true - true_tau[j] + rnorm(N)
  data[, j] <- as.integer(ystar > 0)
}

# ==============================================================================
# 2) ROBUST MILLS RATIOS (log-space)
# ==============================================================================
robust_mills <- function(z) {
  log_phi <- dnorm(z, log = TRUE)
  log_Phi <- pnorm(z, log.p = TRUE)
  exp(log_phi - log_Phi) # phi/Phi
}

robust_mills_comp <- function(z) {
  log_phi <- dnorm(z, log = TRUE)
  log_Q <- pnorm(z, lower.tail = FALSE, log.p = TRUE) # log(1-Phi)
  exp(log_phi - log_Q) # phi/(1-Phi)
}

# ==============================================================================
# 3) INNER LOOP: MODE eta* VIA (DAMPED) NEWTON
# ==============================================================================
get_eta_mode <- function(y_i, lambda, tau, eta0 = 0) {
  eta <- eta0

  # Optional: clamp to prevent Mills blowups from destroying curvature early
  clamp_mills <- 100

  for (iter in 1:20) {
    z <- lambda * eta - tau

    w1 <- robust_mills(z)
    w0 <- robust_mills_comp(z)

    # clamp
    w1 <- pmin(w1, clamp_mills)
    w0 <- pmin(w0, clamp_mills)

    # Gradient: sum lambda*(y*w1 - (1-y)*w0) - eta
    score_data <- sum((y_i * w1 - (1 - y_i) * w0) * lambda)
    grad <- score_data - eta

    # Hessian pieces
    # y=1: h1 = -w1*(z + w1)
    # y=0: h0 =  w0*(z - w0)
    h1 <- -w1 * (z + w1)
    h0 <- w0 * (z - w0)

    # *** FIX: (1-y) term is PLUS, not minus ***
    hess_data <- sum((y_i * h1 + (1 - y_i) * h0) * (lambda^2))
    hess <- hess_data - 1

    # Keep concave
    if (!is.finite(hess) || hess >= -1e-6) {
      hess <- -1
    }

    # Newton step with clamping
    step <- grad / hess
    if (!is.finite(step)) {
      step <- 0
    }
    step <- pmax(pmin(step, 1.0), -1.0)

    eta_new <- eta - step

    if (abs(step) < 1e-6) {
      return(list(eta = eta_new, hess = hess))
    }
    eta <- eta_new
  }

  list(eta = eta, hess = hess)
}

# ==============================================================================
# 4) COLLAPSE DATA INTO RESPONSE PATTERNS (<= 2^J patterns)
# ==============================================================================
# encode each row as integer in [0, 2^J - 1]
pow2 <- 2^(0:(J - 1))
code <- as.integer(data %*% pow2)

tab <- table(code)
codes <- as.integer(names(tab))
counts <- as.integer(tab)
K <- length(codes)

# reconstruct pattern matrix K x J
patterns <- matrix(0L, K, J)
for (k in 1:K) {
  ck <- codes[k]
  # bit decode
  patterns[k, ] <- as.integer(bitwAnd(ck, pow2) > 0)
}

# ==============================================================================
# 5) CACHED EVALUATOR: returns (neg logpost, neg grad) in ONE pass
# ==============================================================================
cache <- new.env(parent = emptyenv())
cache$theta_last <- NULL
cache$val_last <- NULL
cache$grad_last <- NULL

# warm-start cache for eta per pattern
cache$eta_start <- rep(0, K)

# log Phi and log(1-Phi) stable
logPhi <- function(z) pnorm(z, log.p = TRUE)
log1mPhi <- function(z) pnorm(z, lower.tail = FALSE, log.p = TRUE)

eval_obj_and_grad <- function(theta) {
  lambda <- theta[1:J]
  tau <- theta[(J + 1):(2 * J)]

  total_lp <- 0
  grad_accum <- numeric(length(theta))

  for (k in 1:K) {
    y_k <- patterns[k, ]
    nk <- counts[k]

    # inner mode (warm start)
    if (FALSE) {
      res <- get_eta_mode(y_k, lambda, tau, eta0 = cache$eta_start[k])
      eta_star <- res$eta
      H_star <- res$hess

      cache$eta_start[k] <- eta_star
    } else {
      res <- get_eta_mode(y_k, lambda, tau, eta0 = 0)
      eta_star <- res$eta
      H_star <- res$hess

      # cache$eta_start[k] <- eta_star
    }

    z <- lambda * eta_star - tau

    # loglik + logprior(eta)
    ll <- sum(y_k * logPhi(z) + (1 - y_k) * log1mPhi(z))
    lp_eta <- dnorm(eta_star, log = TRUE)
    g_eta <- ll + lp_eta

    if (!is.finite(H_star) || H_star >= 0) {
      return(list(val = 1e100, grad = rep(0, length(theta))))
    }

    lap <- -0.5 * log(-H_star) # omit +0.5*log(2pi) constant
    total_lp <- total_lp + nk * (g_eta + lap)

    # ---------- SIMPLE GRADIENT term (envelope theorem, omit laplace derivative) ----------
    w1 <- robust_mills(z)
    w0 <- robust_mills_comp(z)
    w1 <- pmin(w1, 100)
    w0 <- pmin(w0, 100)

    term <- (y_k * w1 - (1 - y_k) * w0) # length J
    d_lambda <- term * eta_star
    d_tau <- -term

    grad_accum[1:J] <- grad_accum[1:J] + nk * d_lambda
    grad_accum[(J + 1):(2 * J)] <- grad_accum[(J + 1):(2 * J)] + nk * d_tau
  }

  # prior on theta: N(0,10^2)
  lp_theta <- sum(dnorm(theta, mean = 0, sd = 10, log = TRUE))
  total_lp <- total_lp + lp_theta

  # gradient of log prior: -theta/100
  grad_prior <- -theta / 100
  total_grad <- grad_accum + grad_prior

  # return negatives (nlminb minimizes)
  list(val = -total_lp, grad = -total_grad)
}

# ==============================================================================
# 6) OBJECTIVE + GRADIENT wrappers for nlminb with caching
# ==============================================================================
joint_lp <- function(theta) {
  if (!is.null(cache$theta_last) && identical(theta, cache$theta_last)) {
    return(cache$val_last)
  }
  out <- eval_obj_and_grad(theta)
  cache$theta_last <- theta
  cache$val_last <- out$val
  cache$grad_last <- out$grad
  out$val
}

joint_grad <- function(theta) {
  if (
    !is.null(cache$theta_last) &&
      identical(theta, cache$theta_last) &&
      !is.null(cache$grad_last)
  ) {
    return(cache$grad_last)
  }
  out <- eval_obj_and_grad(theta)
  cache$theta_last <- theta
  cache$val_last <- out$val
  cache$grad_last <- out$grad
  out$grad
}

# ==============================================================================
# 7) RUN OPTIMIZATION (nlminb)
# ==============================================================================
# decent starts
p_hat <- colMeans(data)
p_hat <- pmin(pmax(p_hat, 1e-4), 1 - 1e-4)
lambda_start <- rep(1, J)
tau_start <- -qnorm(p_hat)

theta_start <- c(lambda_start, tau_start)

cat(
  "Starting Optimization (Laplace Approximation) with pattern-collapsing...\n"
)
time_start <- Sys.time()

opt <- nlminb(
  theta_start,
  joint_lp,
  joint_grad
  # method = "BFGS",
  # hessian = TRUE
  # control = list(
  #   eval.max = 200,
  #   iter.max = 200,
  #   rel.tol = 1e-8
  # )
)

time_end <- Sys.time()
cat("Done in", time_end - time_start, "\n")
cat("Convergence:", opt$convergence, "  Message:", opt$message, "\n\n")

# ==============================================================================
# 8) Hessian at mode (only once; ok if this takes a bit)
# ==============================================================================
# fd_hessian <- function(fn, par, eps = 1e-4) {
#   p <- length(par)
#   H <- matrix(0, p, p)
#   for (j in 1:p) {
#     ej <- rep(0, p)
#     ej[j] <- 1
#     for (k in j:p) {
#       ek <- rep(0, p)
#       ek[k] <- 1
#       fpp <- fn(par + eps * ej + eps * ek)
#       fpm <- fn(par + eps * ej - eps * ek)
#       fmp <- fn(par - eps * ej + eps * ek)
#       fmm <- fn(par - eps * ej - eps * ek)
#       Hjk <- (fpp - fpm - fmp + fmm) / (4 * eps^2)
#       H[j, k] <- Hjk
#       H[k, j] <- Hjk
#     }
#   }
#   H
# }
#
H_theta <- fd_hessian(joint_lp, opt$par, eps = 1e-4)
# H_theta <- opt$hessian
V_theta <- tryCatch(solve(H_theta), error = function(e) NULL)

# ==============================================================================
# 9) RESULTS
# ==============================================================================
results <- data.frame(
  True = c(true_lambda, true_tau),
  Estimated = round(opt$par, 3)
)
rownames(results) <- c(paste0("Lam", 1:J), paste0("Tau", 1:J))
print(results)

if (is.null(V_theta)) {
  cat(
    "\nWARNING: Hessian not invertible (near-singular). Try eps=5e-4 or stronger priors.\n"
  )
} else {
  cat("\nInverse Hessian computed. (Approx posterior covariance)\n")
}
