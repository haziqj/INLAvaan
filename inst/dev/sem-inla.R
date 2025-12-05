library(tidyverse)
library(mvtnorm)
library(INLA)

# Two factor SEM data ----------------------------------------------------------
n <- 2000
p <- 6
sd_y <- 0.01

eta1 <- rnorm(n, sd = 1)
eta2 <- 0.3 * eta1 + rnorm(n, sd = 1)
Lambda <- matrix(c(1, 1.2, 1.5, 0,   0,   0,
                   0,   0,   0, 1, 1.2, 1.5), ncol = 2)
dat <- t(Lambda %*% rbind(eta1, eta2)) + rmvnorm(n, sigma = diag(rep(sd_y, p)))

# Prepare data for INLA --------------------------------------------------------
Y <- bind_rows(
  data.frame(y1 = dat[, 1]),
  data.frame(y2 = dat[, 2]),
  data.frame(y3 = dat[, 3]),
  data.frame(y4 = dat[, 4]),
  data.frame(y5 = dat[, 5]),
  data.frame(y6 = dat[, 6]),
  data.frame(zero = rep(0, n))
)
IDX <- bind_rows(
  data.frame(idy1 = 1:n, nu1 = 1),
  data.frame(idy2 = 1:n, nu2 = 1),
  data.frame(idy3 = 1:n, nu3 = 1),
  data.frame(idy4 = 1:n, nu4 = 1),
  data.frame(idy5 = 1:n, nu5 = 1),
  data.frame(idy6 = 1:n, nu6 = 1),
  data.frame(idzero1 = 1:n, idzero2 = 1:n, idxNA1 = NA, idxNA2 = NA)
)

# Create INLA formula ----------------------------------------------------------

# This version has fixed precision for latent variables and fixed loadings for
# first item (lambda1 := 1 and lambda4 := 1).
# form <- Y ~ -1 +
#   f(idy1, hyper = list(prec = list(initial = 0, fixed = TRUE))) +
#   f(idy2, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
#   f(idy3, copy = "idy1", hyper = list(beta = list(fixed = FALSE))) +
#
#   f(idy4, hyper = list(prec = list(initial = 0, fixed = TRUE))) +
#   f(idy5, copy = "idy4", hyper = list(beta = list(fixed = FALSE))) +
#   f(idy6, copy = "idy4", hyper = list(beta = list(fixed = FALSE))) +
#
#   f(idzero1, neg_one, copy = "idy1", hyper = list(beta = list(initial = 0.3, fixed = FALSE))) +
#   f(idzero2, copy = "idy4")

# This version has "anchor" latent variables with fixed precision (:= 1)
form <- Y ~ -1 +
  f(idxNA1, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)),
    values = 1:n, constr = TRUE) +
  f(idy1, copy = "idxNA1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy2, copy = "idxNA1", hyper = list(beta = list(fixed = FALSE))) +
  f(idy3, copy = "idxNA1", hyper = list(beta = list(fixed = FALSE))) +

  f(idxNA2, model = "iid", hyper = list(prec = list(initial = 0, fixed = TRUE)),
    values = 1:n, constr = TRUE) +
  f(idy4, copy = "idxNA2", hyper = list(beta = list(fixed = FALSE))) +
  f(idy5, copy = "idxNA2", hyper = list(beta = list(fixed = FALSE))) +
  f(idy6, copy = "idxNA2", hyper = list(beta = list(fixed = FALSE))) +

  f(idzero1, neg_one, copy = "idxNA1", hyper = list(beta = list(initial = 0.3, fixed = FALSE))) +
  f(idzero2, copy = "idxNA2")

# INLA call --------------------------------------------------------------------
fit_inla <- inla(
  formula = form,
  data = c(list(Y = Y, neg_one = rep(-1, nrow(Y))), as.list(IDX)),
  family = c(rep("gaussian", p), "sem"),
  control.family = c(
    rep(list(list()), p),
    list(list(control.sem = list(B = matrix(c("", "idzero1", "", ""), 2, 2), idx = 2)))
  ),
  control.inla = list(int.strategy = "eb"),
  safe = FALSE,
  verbose = TRUE
)
summary(fit_inla)

plot(fit_inla$summary.random$idxNA2$mean, eta2)
abline(0, 1)

plot(fit_inla$summary.random$idxNA1$mean, eta1)
abline(0, 1)

library(lavaan)
dat_lav <- as.data.frame(dat)
names(dat_lav) <- paste0("y", 1:p)
fit_lav <- sem(
  mod = "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  eta2 ~ eta1
  ",
  data = dat_lav,
  std.lv = TRUE
)

