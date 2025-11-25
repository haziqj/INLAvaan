library(tidyverse)
library(blavaan)
library(rstan)
library(future)
plan("multisession", workers = parallel::detectCores() - 2)
nsamp <- 1e4

# set.seed(123)

n <- 50
Sigma_true <- matrix(c(1, 0.5, 0.5, 1.2), nrow = 2)
x <- mvtnorm::rmvnorm(n, sigma = Sigma_true)

stan_mod <- "
data {
  int<lower=1> n;
  matrix[n, 2] x;
}

parameters {
  vector<lower=0>[2] sigma;
  real<lower=-1,upper=1> rho;
}

transformed parameters {
  cov_matrix[2] Sigma;
  Sigma[1,1] = square(sigma[1]);
  Sigma[2,2] = square(sigma[2]);
  Sigma[1,2] = rho * sigma[1] * sigma[2];
  Sigma[2,1] = Sigma[1,2];
}

model {
  sigma ~ gamma(1, 0.5);
  rho ~ uniform(-1, 1);
  for (i in 1:n)
    x[i] ~ multi_normal(rep_vector(0, 2), Sigma);
}
"

stan_data <- list(
  n = n,
  x = x
)

fit_stan <- stan(
  model_code = stan_mod,
  data = stan_data,
  chains = 4, iter = nsamp
)



mod <- "
  eta1 =~ x1
  eta2 =~ x2
"
dat <- data.frame(x1 = x[, 1], x2 = x[, 2])

fit_inlv <- inlavaan(mod, dat, dp = dpriors(psi = "gamma(1,0.5)[sd]", theta = "gamma(1,0.5)[sd]"), method = "skewnorm")

# extract stan draws
draws <- as.data.frame(rstan::extract(fit_stan, permuted = TRUE))
unrestricted_params <- c("Sigma.1.1", "Sigma.2.2", "Sigma.1.2")

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
