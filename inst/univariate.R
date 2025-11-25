library(tidyverse)
library(blavaan)
library(rstan)
library(future)
plan("multisession", workers = parallel::detectCores() - 2)
nsamp <- 1e5

set.seed(123)

n <- 50
mu <- 0
sigma_true <- 0.5
x <- rnorm(n, mean = mu, sd = sigma_true)

stan_mod <- "
data {
  int<lower=1> n;
  vector[n] x;
  real mu;
}

parameters {
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=0> sigma2;
  sigma2 = pow(sigma, 2);
}

model {
  sigma ~ gamma(1, 0.5);
  x ~ normal(mu, sigma);
}
"

stan_data <- list(
  n = n,
  x = x,
  mu = mu
)

fit_stan <- stan(
  model_code = stan_mod,
  data = stan_data,
  chains = 4, iter = nsamp
)

library(blavaan)

mod <- "
eta =~ x
"
dat <- data.frame(x = x)

fit_inlv <- inlavaan(mod, dat, dp = dpriors(psi = "gamma(1,0.5)[sd]"))


# extract stan draws
draws <- as.data.frame(rstan::extract(fit_stan, permuted = TRUE))
unrestricted_params <- c("sigma2")

# plot_df
plot_df_stan <-
  draws |>
  select(all_of(unrestricted_params)) |>
  pivot_longer(everything()) |>
  mutate(
    name = factor(name, levels = unrestricted_params),
    name = factor(names(coef(fit_inlv))[match(name, unrestricted_params)],
                  levels = names(coef(fit_inlv)))
  )


plot_df <-
  list(
    skewnorm = fit_inlv$pdf_data
  ) |>
  map(function(plot_df_list) {
    plot_df <-
      imap(plot_df_list, \(x, idx) mutate(x, name = idx)) |>
      bind_rows() |>
      mutate(name = factor(name, levels = names(coef(fit_inlv))))
  }) |>
  bind_rows(.id = "method") |>
  mutate(method = factor(method, levels = c("skewnorm", "asymgaus", "marggaus", "sampling")))

ggplot() +
  geom_density(
    data = plot_df_stan,
    aes(value, fill = "MCMC"), col = NA, alpha = 0.38
  ) +
  geom_line(
    data = plot_df,
    aes(x, y, group = method, col = method), linewidth = 0.75
  ) +
  facet_wrap(~ name, scales = "free") +
  scale_colour_manual(
    values = c(
      "skewnorm" = "#00A6AA",
      "asymgaus" = "#F18F00",
      "marggaus" = "#adbf04",
      "sampling" = "#9C6FAE"
    )
    # breaks = c("asymgaus", "skewnorm", "MCMC")
  ) +
  scale_fill_manual(
    values = c("MCMC" = "#131516")
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(t = 4, r = 8, b = 8, l = 8),
    legend.position = "top",
    legend.box.spacing = unit(2, "pt"),
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = NULL, y = NULL, col = NULL, fill = NULL)
