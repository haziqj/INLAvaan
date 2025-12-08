# testthat::skip()

## ----- 1. Define the Log-PDF -------------------------------------------------
# p(x, y) = p(x) * p(y|x)
# p(x)    = Normal(0, 1)
# p(y|x)  = Normal(0, sd = sqrt(exp(x)))

lp_joint <- function(theta) {
  x <- theta[1]
  y <- theta[2]
  lp_x <- dnorm(x, mean = 0, sd = 1, log = TRUE)
  lp_y.x <- dnorm(y, mean = 0, sd = sqrt(exp(x)), log = TRUE)
  lp_x + lp_y.x
}

## ----- 2. Find the Joint Mode (Optimization) ---------------------------------
opt <- optim(
  c(0, 0),
  function(.theta) -1 * lp_joint(.theta),
  hessian = TRUE
)
joint_mode <- opt$par

## ----- 3. Compare the Modes --------------------------------------------------
cat(
  "=== Results ===\n",
  sprintf(
    "Marginal Mode of X:  %0.4f (By definition, since X ~ N(0,1))\n",
    0
  ),
  sprintf(
    "Joint Mode of X:    %0.4f (Calculated via optimization)\n",
    joint_mode[1]
  )
)

## ----- 4. Plot ---------------------------------------------------------------
library(ggplot2)
library(dplyr)

grid_size <- 0.1
grid <- expand.grid(
  x = seq(-2, 2, by = grid_size),
  y = seq(-2, 2, by = grid_size)
)
grid$z <- with(grid, mapply(function(x, y) lp_joint(c(x, y)), x, y))

ggplot(grid, aes(x, y)) +
  geom_contour_filled(aes(z = z), bins = 15, show.legend = FALSE) +
  # Marginal mode
  annotate("point", x = 0, y = 0, color = "deepskyblue", size = 3) +
  annotate(
    "text",
    x = 0,
    y = 0.4,
    label = "Marginal Mode (0,0)",
    color = "deepskyblue",
    fontface = "bold",
    vjust = 0
  ) +
  # Joint Mode (-0.5, 0) - Peak of density
  annotate("point", x = -0.5, y = 0, color = "red", shape = 17, size = 3) +
  annotate(
    "text",
    x = -0.5,
    y = -0.4,
    label = "Joint Mode (-0.5, 0)",
    color = "red",
    fontface = "bold",
    vjust = 1
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "X (Controls Variance)",
    y = "Y"
  ) +
  theme_minimal()

## ----- 5. Skew normal marginal fit -------------------------------------------
library(numDeriv)
sn_fit_cor <- TRUE
sn_fit_logthresh <- -6
sn_fit_temp <- 1

theta_star <- opt$par
H_neg <- opt$hessian
Sigma_theta <- solve(0.5 * (H_neg + t(H_neg)))
m <- length(theta_star)
pars_list <- setNames(as.list(1:m), paste0("theta[", 1:m, "]"))

# For whitening transformation: z = L^{-1}(theta - theta*)
L <- t(chol(Sigma_theta))
L_inv <- solve(L)

approx_data <- matrix(NA, nrow = m, ncol = 4)
colnames(approx_data) <- c("xi", "omega", "alpha", "logC")

approx_data <-
  do.call(
    what = "rbind",
    lapply(
      pars_list,
      function(j) {
        mv <- seq(-4, 4, length = 31)
        tt <- theta_star[j] + mv * sqrt(Sigma_theta[j, j])
        yy <- numeric(length(mv))
        for (k in seq_along(mv)) {
          if (isTRUE(sn_fit_cor)) {
            # Fit in decoupled Z-space (conditional modes are 0)
            tt[k] <- mv[k]
            yy[k] <- lp_joint(theta_star + L[, j] * mv[k])
          } else {
            # Evaluate lp_joint at theta_j with others fixed at the
            # conditional mode
            theta_new <- rep(NA, length(theta_star))
            theta_new[j] <- tt[k]
            theta_new[-j] <- theta_star[-j] +
              Sigma_theta[-j, j] /
                Sigma_theta[j, j] *
                (tt[k] - theta_star[j])
            yy[k] <- lp_joint(theta_new)
          }
        }
        fit_sn <- fit_skew_normal(
          tt,
          yy - max(yy),
          threshold_log_drop = sn_fit_logthresh,
          temp = sn_fit_temp
        )
        if (isTRUE(sn_fit_cor)) {
          # Adjust back to theta space
          fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
          fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])
        }
        unlist(fit_sn)
      }
    )
  )

# Compare marginal of x and skew normal fit of x. There is a shift because the
# skew normal is fitted at the joint mode, which is biased.
tibble(
  x = seq(-4, 4, length = 100),
  truth = dnorm(x, 0, 1),
  sn_fit = INLAvaan::dsnorm(
    x,
    xi = approx_data["theta[1]", "xi"],
    omega = approx_data["theta[1]", "omega"],
    alpha = approx_data["theta[1]", "alpha"]
  )
) |>
  pivot_longer(
    cols = c("truth", "sn_fit"),
    names_to = "type",
    values_to = "density"
  ) |>
  ggplot(aes(
    x,
    density,
    color = type,
    fill = type,
    alpha = type,
    linewidth = type
  )) +
  geom_area(position = "identity") +
  geom_line(alpha = 1) +
  scale_alpha_manual(values = c(0.3, 0)) +
  scale_linewidth_manual(values = c(0, 0.8)) +
  labs(
    title = "Marginal of X: True vs Skew Normal Approximation",
    y = "Density"
  ) +
  theme_minimal()

## ----- 6. The fix ------------------------------------------------------------

# When we "feel out" and explore the lp_joint(), for each value of x we move,
# the value of y is set at the joint mode (in whitened space this is 0).
# However, the volume of y-values that contribute to the marginal density of x
# changes with x. To correct for this, we apply a Laplace approximation
# correction to account for the curvature (2nd derivative) of the log-pdf wrt y
# at the joint mode.

marginal_objective <- function(x) {
  # A. Find y* for this x (Profile Likelihood)
  y_star <- if (TRUE) {
    # Cheap case: Just set at joint mode
    0
  } else {
    # General case: optimize y for this x
    optim(0, function(y) -1 * lp_joint(c(x, y)))$par
  }

  # B. Get the Joint Density Height at this point
  joint_val <- lp_joint(c(x, y_star))

  # C. Calculate Geometry (Curvature/Hessian) wrt Y
  # We calculate the 2nd derivative of the PDF wrt y at y_star
  H_yy <- hessian(function(y) lp_joint(c(x, y)), y_star)

  # D. Apply Laplace Correction
  # Correction = -0.5 * log(determinant of negative Hessian)
  # Note: H_yy is negative at the peak, so we take -H_yy
  volume_correction <- -0.5 * log(det(-H_yy))

  return(joint_val + volume_correction)
}

# Optimize Corrected (Approximate Marginal)
opt_marginal <- optimize(
  marginal_objective,
  interval = c(-3, 3),
  maximum = TRUE
)

cat(
  sprintf("Joint Mode (Uncorrected): %0.4f\n", opt$par[1]),
  sprintf("Recovered Marginal Mode:  %0.4f\n", opt_marginal$maximum)
)

## ----- 7. Corrected skew normal fit ------------------------------------------

approx_data <-
  do.call(
    what = "rbind",
    lapply(
      pars_list,
      function(j) {
        mv <- seq(-4, 4, length = 31)
        tt <- theta_star[j] + mv * sqrt(Sigma_theta[j, j])
        yy <- numeric(length(mv))
        for (k in seq_along(mv)) {
          if (isTRUE(sn_fit_cor)) {
            yy[k] <- marginal_objective(tt[k])
          } else {
            yy[k] <- marginal_objective(tt[k])
          }
        }
        fit_sn <- fit_skew_normal(
          tt,
          yy - max(yy),
          threshold_log_drop = sn_fit_logthresh,
          temp = sn_fit_temp
        )
        if (isTRUE(sn_fit_cor)) {
          # Adjust back to theta space
          fit_sn$xi <- theta_star[j] + fit_sn$xi * sqrt(Sigma_theta[j, j])
          fit_sn$omega <- fit_sn$omega * sqrt(Sigma_theta[j, j])
        }
        unlist(fit_sn)
      }
    )
  )

tibble(
  x = seq(-4, 4, length = 100),
  truth = dnorm(x, 0, 1),
  sn_fit = INLAvaan::dsnorm(
    x,
    xi = approx_data["theta[1]", "xi"],
    omega = approx_data["theta[1]", "omega"],
    alpha = approx_data["theta[1]", "alpha"]
  )
) |>
  pivot_longer(
    cols = c("truth", "sn_fit"),
    names_to = "type",
    values_to = "density"
  ) |>
  ggplot(aes(
    x,
    density,
    color = type,
    fill = type,
    alpha = type,
    linewidth = type
  )) +
  geom_area(position = "identity") +
  geom_line(alpha = 1) +
  scale_alpha_manual(values = c(0.3, 0)) +
  scale_linewidth_manual(values = c(0, 0.8)) +
  labs(
    title = "Marginal of X: True vs Skew Normal Approximation",
    y = "Density"
  ) +
  theme_minimal()
