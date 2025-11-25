library(tidyverse)
library(lavaan)
library(rstan)
data("HolzingerSwineford1939")
nsamp <- 1e5

mod <- "
data {
  int<lower=1> N;             // number of individuals
  int<lower=1> J;             // number of indicators (9)
  int<lower=1> K;             // number of factors (3)
  matrix[N, J] y;             // observed data (centered)
  cov_matrix[J] S;            // sample covariance matrix
}

parameters {
  // Free loadings: visual (x2, x3), textual (x5, x6), speed (x8, x9)
  vector<lower=0>[6] lambda_free;

  // Latent and residual SDs
  vector<lower=0>[K] sd_f;
  vector<lower=0>[J] sd_e;

  // Cholesky factor of the correlation matrix (Guarantees positive definiteness)
  cholesky_factor_corr[K] L_omega;
}

transformed parameters {
  // latent and residual variances
  vector[K] var_f;
  vector[J] var_e;

  // ---- Variances from SDs ----

  for (j in 1:J) {
    var_e[j] = square(sd_e[j]);
  }

  // Factor loading matrix Lambda (J x K)
  matrix[J, K] Lambda = rep_matrix(0, J, K);

  // Latent covariance matrix
  cov_matrix[K] Psi;

  // Residual variance matrix (diagonal)
  cov_matrix[J] Theta = rep_matrix(0, J, J);

  // Model implied covariance
  cov_matrix[J] Sigma;

  // --- Build Lambda (Marker Variable Method) ---
  // Factor 1: visual
  Lambda[1, 1] = 1.0;
  Lambda[2, 1] = lambda_free[1];
  Lambda[3, 1] = lambda_free[2];

  // Factor 2: textual
  Lambda[4, 2] = 1.0;
  Lambda[5, 2] = lambda_free[3];
  Lambda[6, 2] = lambda_free[4];

  // Factor 3: speed
  Lambda[7, 3] = 1.0;
  Lambda[8, 3] = lambda_free[5];
  Lambda[9, 3] = lambda_free[6];

  // --- Build Theta ---
  for (j in 1:J) {
    Theta[j, j] = square(sd_e[j]);
  }

  corr_matrix[K] R;
  R = multiply_lower_tri_self_transpose(L_omega);
  vector[3] cov;


  // --- Build Psi (Covariance of Factors) ---
  // Psi = D * R * D
  // We utilize the Cholesky factor L_omega directly for numerical stability
  // calculation: Psi = diag(sd) * (L * L') * diag(sd)
  // But for the math in Stan, quad_form_diag is efficient:
  // Reconstruct the full correlation matrix R from Cholesky factor
  // multiply_lower_tri_self_transpose(L) = L * L'
  Psi = quad_form_diag(R, sd_f);
  cov[1] = Psi[1,2];
  cov[2] = Psi[1,3];
  cov[3] = Psi[2,3];
  for (k in 1:3) {
    var_f[k] = Psi[k,k];
  }

  // --- Build Sigma (Implied Covariance) ---
  Sigma = Lambda * Psi * Lambda' + Theta;
}

model {
  // --- Priors ---
  lambda_free ~ normal(0, 10); // Slightly weakly informative
  var_f ~ gamma(1,1);
  var_e ~ gamma(1,1);

  // LKJ Prior for correlations
  // eta = 1 implies uniform distribution over valid correlation matrices
  // eta > 1 favors lower correlations (regularization)
  L_omega ~ lkj_corr_cholesky(1);

  // --- Likelihood ---
  // Wishart likelihood on the sufficient statistics (Sample Covariance)
  // (N-1) * S follows a Wishart distribution with N-1 d.f. and scale Sigma
  target += wishart_lpdf((N - 1) * S | N - 1, Sigma);
  // for (n in 1:N)
  //  y[n] ~ multi_normal(rep_vector(0, J), Sigma);
}
"

# Use the 9 indicators x1..x9 and center them (no mean structure in Stan)
y <- scale(HolzingerSwineford1939[, paste0("x", 1:9)], center = TRUE, scale = FALSE)

N <- nrow(y)
J <- ncol(y)
K <- 3

stan_data <- list(
  N = N,
  J = J,
  K = K,
  y = y,
  S = cov(y)
)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit <- stan(
  model_code = mod,
  data = stan_data,
  chains = 4,
  iter = nsamp,
  warmup = nsamp / 2,
  seed = 1234
)

# Print the unrestricted parameters directly
print(fit,
      pars = c(
        "lambda_free",
        "var_f", "var_e",
        "cov"
      ),
      probs = c(0.025, 0.5, 0.975))


DP <- dpriors(psi = "gamma(1,1)", theta = "gamma(1,1)")
fit_inl1 <- inlavaan(
  "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
  ",
  HolzingerSwineford1939, "cfa", dp = DP)

# extract stan draws
draws <- as.data.frame(rstan::extract(fit, permuted = TRUE))

# unrestricted parameters
unrestricted_params <- c(
  paste0("lambda_free.", 1:6),
  paste0("var_e.", 1:9),
  paste0("var_f.", 1:3),
  paste0("cov.", 1:3)
)

# plot_df
plot_df_stan <-
  draws |>
  select(all_of(unrestricted_params)) |>
  pivot_longer(everything()) |>
  mutate(
    name = factor(name, levels = unrestricted_params),
    name = factor(names(coef(fit_inl1))[match(name, unrestricted_params)],
                     levels = names(coef(fit_inl1)))
  )



plot_df <-
  list(
    skewnorm = fit_inl1$pdf_data
    # asymgaus = fit_inl2$pdf_data,
    # marggaus = fit_inl3$pdf_data,
    # sampling = fit_inl4$pdf_data
  ) |>
  map(function(plot_df_list) {
    plot_df <-
      imap(plot_df_list, \(x, idx) mutate(x, name = idx)) |>
      bind_rows() |>
      mutate(name = factor(name, levels = names(coef(fit_inl1))))
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
