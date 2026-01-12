library(tidyverse)
library(INLA)

## ----- IRT data --------------------------------------------------------------
n <- 10000 # sample size
p <- 20 # test length

# Simple truncated normal generator (rejection sampling)
rtruncnorm <- function(n, mean = 0, sd = 1, a = -Inf, b = Inf) {
  out <- numeric(n)
  i <- 1
  while (i <= n) {
    x <- rnorm(n - i + 1, mean = mean, sd = sd)
    x <- x[x >= a & x <= b]
    k <- length(x)
    if (k > 0) {
      out[i:min(n, i + k - 1)] <- x[1:min(k, n - i + 1)]
      i <- i + min(k, n - i + 1)
    }
  }
  out
}

disc <- rtruncnorm(p, mean = 1.2, sd = 0.3, a = 0.3, b = 2.5)
diff <- rtruncnorm(p, mean = 0, sd = 1.0, a = -2.5, b = 2.5)
beta <- disc
alpha <- -diff * disc

# Generate 2PL IRT data from above diff and disc params
theta <- rnorm(n)
logits <- sweep(outer(theta, diff, "-"), 2, disc, "*")
probs <- plogis(logits)
resp <- matrix(rbinom(n * p, 1, probs), n, p)[, 1:p]

## ----- Prepare data for INLA -------------------------------------------------

rows <- seq_len(n * p)
item <- rep(seq_len(p), each = n)
id_person <- rep(seq_len(n), times = p)
Y_mat <- matrix(NA_integer_, nrow = n * p, ncol = p)
Y_mat[cbind(rows, item)] <- as.integer(resp) # resp is n x p, column-major matches stacking

colnames(Y_mat) <- paste0("y", seq_len(p))
Y <- as_tibble(Y_mat)

idy_mat <- matrix(NA_integer_, nrow = n * p, ncol = p)
idy_mat[cbind(rows, item)] <- id_person
colnames(idy_mat) <- paste0("idy", seq_len(p))

IDX <- bind_cols(
  data.frame(
    id = rep(1:n, times = p),
    alpha = as.character(rep(1:p, each = n))
  ),
  as_tibble(idy_mat)
)

datl <- c(list(Y = Y), as.list(IDX))

## ----- Fit INLA --------------------------------------------------------------
form <- Y ~ -1 +
  alpha +
  f(
    idy1,
    model = "iid",
    hyper = list(prec = list(initial = 0, fixed = !TRUE))
  ) +
  f(idy2, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy3, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy4, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy5, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy6, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy7, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy8, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy9, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy10, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy11, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy12, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy13, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy14, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy15, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy16, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy17, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy18, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy19, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy20, copy = "idy1", hyper = list(beta = list(fixed = FALSE)))
# f(idy21, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy22, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy23, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy24, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy25, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy26, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy27, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy28, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy29, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy30, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy31, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy32, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy33, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy34, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy35, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy36, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy37, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy38, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy39, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
# f(idy40, copy = "idy1", hyper = list(beta = list(fixed = FALSE)))

fit <- inla(form, data = datl, family = c(rep("binomial", p)), verbose = TRUE)

## ----- Compare params --------------------------------------------------------
cat("Difficulty parameters comparison:\n\n")
data.frame(
  truth = alpha,
  est = round(fit$summary.fixed$mean, 3)
)

cat("\nDiscrimination parameters comparison:\n\n")
data.frame(
  truth = beta,
  est = c(
    round(1 / fit$summary.hyperpar$mean[1]^0.5, 3), # not exactly, but roughly
    round(fit$summary.hyperpar$mean[-1], 3)
  )
)
