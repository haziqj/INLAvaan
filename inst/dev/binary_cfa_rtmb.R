# ==============================================================================
# Probit CFA (1 factor, 5 binary items) via RTMB (Laplace over eta)
# - eta_i are random effects (integrated out by Laplace)
# - lambda_j, tau_j are fixed effects
# - analytic gradient of the full Laplace objective is automatic
# ==============================================================================

# install.packages("RTMB")  # if needed
library(RTMB)

set.seed(123)

# ------------------------------------------------------------------------------
# 1) Simulate data (same model as your notes)
# ------------------------------------------------------------------------------
N <- 5000
J <- 5

true_lambda <- rep(1.5, J)
true_tau <- seq(-0.5, 0.5, length.out = J)

eta_true <- rnorm(N)
Y <- matrix(0L, N, J)
for (j in 1:J) {
  ystar <- true_lambda[j] * eta_true - true_tau[j] + rnorm(N)
  Y[, j] <- as.integer(ystar > 0)
}

dat <- list(Y = Y, N = N, J = J)


# Y is N x J (0/1)
Y <- dat$Y
N <- nrow(Y)
J <- ncol(Y)

# --- collapse to patterns ---
pow2 <- 2^(0:(J - 1))
code <- as.integer(Y %*% pow2)
tab <- table(code)
codes <- as.integer(names(tab))
counts <- as.numeric(tab)
K <- length(codes)

patterns <- matrix(0L, K, J)
for (k in 1:K) {
  ck <- codes[k]
  patterns[k, ] <- as.integer(bitwAnd(ck, pow2) > 0)
}

dat2 <- list(patterns = patterns, counts = counts, K = K, J = J)

# --- parameters (eta is length K now, NOT N) ---
p_hat <- colMeans(Y)
p_hat <- pmin(pmax(p_hat, 1e-4), 1 - 1e-4)

parameters <- list(
  log_lambda1 = 0,
  lambda_free = rep(1, J - 1),
  tau = -qnorm(p_hat),
  eta = rep(0, K)
)

f <- function(parms) {
  getAll(dat2, parms, warn = FALSE)

  lambda <- c(exp(log_lambda1), lambda_free)

  # K x J matrix: z_kj = lambda_j * eta_k - tau_j
  z <- outer(eta, lambda, "*") - matrix(rep(tau, each = K), nrow = K)
  p <- pnorm(z)

  nll <- 0

  # prior: each pattern represents 'counts[k]' independent people
  nll <- nll - sum(counts * dnorm(eta, 0, 1, log = TRUE))

  # likelihood: weighted Bernoulli loglik
  nll <- nll - sum(counts * rowSums(dbinom(patterns, 1, p, log = TRUE)))

  # weak priors on fixed effects
  nll <- nll - dnorm(log_lambda1, 0, 10, log = TRUE)
  nll <- nll - sum(dnorm(lambda_free, 0, 10, log = TRUE))
  nll <- nll - sum(dnorm(tau, 0, 10, log = TRUE))

  nll
}

obj <- MakeADFun(f, parameters, random = "eta")

opt <- nlminb(
  obj$par,
  obj$fn,
  obj$gr,
  control = list(iter.max = 200, eval.max = 200, rel.tol = 1e-10)
)

opt$convergence
opt$message
max(abs(obj$gr(opt$par)))
