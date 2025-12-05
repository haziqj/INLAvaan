library(sn)
library(tidyverse)

joint_lp <- function(x) dgamma(x, shape = 3, rate = 1, log = TRUE)

x_grid <- seq(0.1, 8, length.out = 21)
y_log <- sapply(x_grid, joint_lp)
y_log <- y_log - max(y_log) # normalise to have maximum at zero

res <- fit_skew_normal(x_grid, y_log, temp = NA)
unlist(res)

tibble(
  x = seq(0.1, 8, length.out = 200),
  truth = exp(joint_lp(x)),
  approx = dsnorm(x, xi = res$xi, omega = res$omega, alpha = res$alpha)
) |>
  pivot_longer(
    cols = c("truth", "approx"),
    names_to = "type",
    values_to = "density"
  ) |>
  ggplot(aes(x = x, y = density, color = type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "top")
