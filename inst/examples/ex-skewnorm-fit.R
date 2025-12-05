library(sn)
library(tidyr)
library(ggplot2)

logdens <- function(x) dgamma(x, shape = 3, rate = 1, log = TRUE)

x_grid <- seq(0.1, 8, length.out = 21)
y_log <- sapply(x_grid, logdens)
y_log <- y_log - max(y_log) # normalise to have maximum at zero

res <- fit_skew_normal(x_grid, y_log, temp = 10)
unlist(res)

plot_df <-
  pivot_longer(
    tibble(
      x = seq(0.1, 8, length.out = 200),
      truth = exp(logdens(x)),
      approx = dsnorm(x, xi = res$xi, omega = res$omega, alpha = res$alpha)
    ),
    cols = c("truth", "approx"),
    names_to = "type",
    values_to = "density"
  )

ggplot() +
  # truth as filled area
  geom_area(
    data = subset(plot_df, type == "truth"),
    aes(x, density, fill = "Truth"),
    alpha = 0.38
  ) +
  # approx as blue line
  geom_line(
    data = subset(plot_df, type == "approx"),
    aes(x = x, y = density, col = "SN Approx."),
    linewidth = 1
  ) +
  scale_fill_manual(name = NULL, values = "#131516") +
  scale_colour_manual(name = NULL, values = "#00A6AA") +
  theme_minimal() +
  theme(legend.position = "top")
