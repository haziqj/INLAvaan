library(tidyverse)
library(INLA)

## ----- IRT data --------------------------------------------------------------
n <- 1000 # sample size
p <- 5 # test length
diff <- round(seq(-2, 2, length = p), 3)
disc <- round(seq(0.5, 1.5, length = p), 3)
beta <- disc
alpha <- -diff * disc

# Generate 2PL IRT data from above diff and disc params
theta <- rnorm(n)
logits <- sweep(outer(theta, diff, "-"), 2, disc, "*")
probs <- plogis(logits)
resp <- matrix(rbinom(n * p, 1, probs), n, p)

## ----- Prepare data for INLA -------------------------------------------------
Y <- bind_rows(
  data.frame(y1 = resp[, 1]),
  data.frame(y2 = resp[, 2]),
  data.frame(y3 = resp[, 3]),
  data.frame(y4 = resp[, 4]),
  data.frame(y5 = resp[, 5])
)
IDX <- bind_cols(
  data.frame(
    id = rep(1:n, times = p),
    alpha = as.character(rep(1:p, each = n))
  ),
  bind_rows(
    data.frame(idy1 = 1:n),
    data.frame(idy2 = 1:n),
    data.frame(idy3 = 1:n),
    data.frame(idy4 = 1:n),
    data.frame(idy5 = 1:n)
  )
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
  f(idy5, copy = "idy1", hyper = list(beta = list(fixed = FALSE)))
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
