library(tidyverse)
library(ggrepel)

## ----- Define the log pdf ----------------------------------------------------
# p(x, y) = p(x) * p(y|x)
# p(x)    = Normal(0, 1)
# p(y|x)  = Normal(0, sd = sqrt(exp(x)))

joint_lp <- function(theta) {
  x <- theta[1]
  y <- theta[2]
  lp_x <- dnorm(x, mean = 1, sd = 1, log = TRUE)
  lp_y.x <- dnorm(y, mean = 2, sd = sqrt(exp(x)), log = TRUE)
  lp_x + lp_y.x
}

joint_lp_grad <- function(theta) {
  numDeriv::grad(function(.theta) joint_lp(.theta), theta)
}

## ----- Find the joint mode (optimisation) ------------------------------------
opt <- optim(
  c(0, 0),
  function(.theta) -1 * joint_lp(.theta),
  hessian = TRUE
)
joint_mode <- opt$par

## ----- Plot ------------------------------------------------------------------
grid_size <- 0.1
grid <- expand_grid(
  x = 1 + seq(-2, 2, by = grid_size),
  y = 2 + seq(-2, 2, by = grid_size)
)
grid$z <- with(grid, mapply(function(x, y) joint_lp(c(x, y)), x, y))

labels_df <- tibble(
  x = c(1, joint_mode[1]),
  y = c(2, joint_mode[2]),
  label = c("Marginal mode", "Joint mode")
)

p <-
  ggplot(grid, aes(x, y)) +
  geom_contour_filled(aes(z = z), bins = 15, alpha = 0.8) +
  geom_point(data = labels_df, aes(x, y, col = label)) +
  geom_text_repel(
    data = labels_df,
    aes(label = label, col = label),
    min.segment.length = 0,
    box.padding = 1
  ) +
  scale_fill_viridis_d(option = "rocket") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c("#841E5AFF", "#F06043FF")) +
  labs(
    x = "X (Controls Variance)",
    y = "Y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )
p

## ----- Orientation -----------------------------------------------------------
theta_star <- opt$par
H_neg <- opt$hessian
Sigma_theta <- solve(0.5 * (H_neg + t(H_neg)))
m <- length(theta_star)
pars_list <- setNames(as.list(1:m), paste0("theta[", 1:m, "]"))

Lcho <- t(chol(Sigma_theta))
eig <- eigen(Sigma_theta, symmetric = TRUE)
Leig <- eig$vectors %*% diag(sqrt(eig$values))

# Walk in Z-space, projected back
z <- seq(-4, 4, length = 31)
theta_cho <- theta_eig <- list()
for (j in 1:m) {
  tmpcho <- tmpeig <- matrix(NA, nrow = length(z), ncol = m)

  for (k in seq_along(z)) {
    tmpcho[k, ] <- theta_star + Lcho[, j] * z[k]
    tmpeig[k, ] <- theta_star +
      as.numeric(Leig %*% (Leig[j, ] * z[k] / sqrt(Sigma_theta[j, j])))
  }
  theta_cho[[j]] <- as.data.frame(tmp)
  theta_eig[[j]] <- as.data.frame(tmpeig)
}

# Choleski scan
p +
  # geom_hline(yintercept = joint_mode[2], color = "grey30") +
  # geom_vline(xintercept = joint_mode[1], color = "grey30") +
  geom_line(data = theta_cho[[1]], aes(x = V1, y = V2)) +
  geom_line(data = theta_cho[[2]], aes(x = V1, y = V2))

# Eigen scan
p +
  # geom_hline(yintercept = joint_mode[2], color = "grey30") +
  # geom_vline(xintercept = joint_mode[1], color = "grey30") +
  geom_line(data = theta_eig[[1]], aes(x = V1, y = V2)) +
  geom_line(data = theta_eig[[2]], aes(x = V1, y = V2))

## ----- Skew normal marginal fit ----------------------------------------------

get_gamma1 <- function(.j, marginal_correction = "shortcut") {
  delta_outer <- 0.01 # for rate of change of Hessian (3rd deriv)
  delta_inner <- 0.001 # for rate of change of gradients (2nd deriv)

  # Precompute baseline Hessian (diagonal of Hessian_z at mode)
  Hz0 <- numeric(m)
  for (j in 1:m) {
    g_fwd <- -1 * joint_lp_grad(theta_star + L[, j] * delta_inner)
    g_bwd <- -1 * joint_lp_grad(theta_star - L[, j] * delta_inner)
    Hz0[j] <- sum(L[, j] * (g_fwd - g_bwd)) / (2 * delta_inner)
  }

  if (marginal_correction == "none") {
    gamma1j <- 0
  } else {
    th_plus <- theta_star + L[, .j] * delta_outer
    if (marginal_correction == "hessian") {
      Htheta1_full <- numDeriv::jacobian(
        function(x) -1 * joint_lp_grad(x),
        th_plus
      )
      Hz1 <- diag(t(L) %*% Htheta1_full %*% L)
    } else if (marginal_correction == "shortcut") {
      Hz1 <- numeric(m)
      for (jj in 1:m) {
        g_fwd <- -1 * joint_lp_grad(th_plus + L[, jj] * delta_inner)
        g_bwd <- -1 * joint_lp_grad(th_plus - L[, jj] * delta_inner)
        Hz1[jj] <- sum(L[, jj] * (g_fwd - g_bwd)) / (2 * delta_inner)
      }
    }
    dH_dz <- (Hz1 - Hz0) / delta_outer
    gamma1j <- -0.5 * sum(dH_dz[-.j])
  }
  gamma1j
}

obtain_approx_data <- function(j) {
  z <- seq(-4, 4, length = 31)
  yync <- yy <- numeric(length(z))
  gamma1j <- get_gamma1(j)

  for (k in seq_along(z)) {
    yync[k] <- joint_lp(theta_star + L[, j] * z[k])
    yy[k] <- yync[k] + gamma1j * z[k]
  }

  fit_sn <- INLAvaan::fit_skew_normal(x = z, y = yy - max(yy))

  # Adjust back to theta space
  fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
  fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])

  c(unlist(fit_sn), gamma1 = gamma1j)
}
