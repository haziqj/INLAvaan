library(dplyr)
library(INLA)

n <- 2000
x <- rnorm(n)
s <- 1
y1 <- x + rnorm(n, sd = s)
beta2 <- 2.0
y2 <- rnorm(n, mean = beta2 * x, sd = sqrt(1 + beta2^2))
beta3 <- 4.0
y3 <- rnorm(n, mean = beta3 * x, sd = sqrt(1 + beta3^2))

Y <- bind_rows(
  data.frame(y1 = y1),
  data.frame(y2 = y2),
  data.frame(y3 = y3)
)
IDX <- bind_rows(
  data.frame(idx1 = 1:n, intercept1 = 1),
  data.frame(idx2 = 1:n, intercept2 = 1),
  data.frame(idx3 = 1:n, intercept3 = 1)
)

r <- inla(
  Y ~ -1 +
    # intercept1 +
    f(idx1, model = "iid", hyper = list(prec = list(initial = 1, fixed = !TRUE))) +
    # intercept2 +
    f(idx2, copy = "idx1", hyper = list(beta = list(fixed = FALSE))) +
    # intercept3 +
    f(idx3, copy = "idx1", hyper = list(beta = list(fixed = FALSE))),
  data = list(
    Y = Y,
    idx1 = IDX$idx1,
    idx2 = IDX$idx2,
    idx3 = IDX$idx3,
    intercept1 = IDX$intercept1,
    intercept2 = IDX$intercept2,
    intercept3 = IDX$intercept3
  ),
  family = c("gaussian", "sem", "sem"),
  control.family = list(
    list(hyper = list(prec = list(initial = log(1 / s^2), fixed = !TRUE))),
    list(control.sem = list(B = matrix(c("", "", "idx2", ""), 2, 2), idx = 2)),
    list(control.sem = list(B = matrix(c("", "", "idx3", ""), 2, 2), idx = 2))
  ),
  safe = FALSE,
  verbose = TRUE
)

c(1, 1 / (1 + beta2^2), 1 / (1 + beta3^2))
summary(r)
