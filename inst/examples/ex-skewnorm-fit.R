library(sn)
library(tidyr)
library(ggplot2)

logdens <- function(x) dgamma(x, shape = 3, rate = 1, log = TRUE)

x_grid <- seq(0.1, 8, length.out = 21)
y_log <- sapply(x_grid, logdens)
y_log <- y_log - max(y_log) # normalise to have maximum at zero

res <- fit_skew_normal(x_grid, y_log, temp = NA)
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

ggplot(plot_df, aes(x = x, y = density, color = type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "top")
