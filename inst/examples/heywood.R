library(tidyverse)
library(lavaan)
library(blavaan)
library(INLAvaan)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)
nsamp <- 1e3

# To test advantage of skewnormal fits. Here, y3 has loading 0.9 with small
# residual variance (0.05). So total variance is var(y3) = 0.9^2 + 0.05 = 0.86,
# and communality is 0.81/0.86 = 0.94. So this is almost pure factor with very
# little noise. This creates a curved, near boundary ridge in the
# likelihood/posterior surface. ML happily pushes it below 0 (Heywood), while MCMC produces heavy left skew.

gen_data <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  true_model <- "
    # Measurement model
    eta1 =~ 0.8*y1 + 0.8*y2 + 0.9*y3
    eta2 =~ 0.8*y4 + 0.8*y5 + 0.9*y6

    # Latent covariance (strong = 0.95)
    eta1 ~~ 0.95*eta2

    # Residual variances
    y1 ~~ 0.36*y1
    y2 ~~ 0.36*y2
    y3 ~~ 0.05*y3      # near-Heywood
    y4 ~~ 0.36*y4
    y5 ~~ 0.36*y5
    y6 ~~ 0.20*y6

    # Small cross-load error correlations (to induce curvature)
    y1 ~~ 0.05*y4
    y2 ~~ 0.05*y5
    y3 ~~ 0.05*y6

    # Latent means fixed to zero
    eta1 ~ 0*1
    eta2 ~ 0*1
  "
  lavaan::simulateData(true_model, sample.nobs = n)
}

mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
"
dat <- gen_data(80, seed = 22)

fit_lav  <- sem(mod, dat, std.lv = TRUE)
truval <- coef(fit_lav)
truval[] <- c(0.8, 0.8, 0.9, 0.8, 0.8, 0.9,
              0.36, 0.36, 0.05, 0.36, 0.36, 0.20, 0.95)

fit_blav <- bsem(mod, dat, std.lv = TRUE, bcontrol = list(cores = 3),
                 burnin = nsamp / 2, sample = nsamp)
fit_inl1 <- inlavaan(mod, dat, std.lv = TRUE, method = "skewnorm")
fit_inl2 <- inlavaan(mod, dat, std.lv = TRUE, method = "asymgaus")
# fit_inl3 <- inlavaan(mod, dat, std.lv = TRUE, method = "marggaus")
fit_inl4 <- inlavaan(mod, dat, std.lv = TRUE, method = "sampling")

# Comparison
draws <- do.call("rbind", blavInspect(fit_blav, "mcmc"))
plot_df_blav <-
  as.data.frame(draws) |>
  pivot_longer(everything()) |>
  mutate(name = factor(name, levels = names(coef(fit_blav))))

plot_df <-
  list(
    skewnorm = fit_inl1$pdf_data,
    asymgaus = fit_inl2$pdf_data,
    # marggaus = fit_inl3$pdf_data,
    sampling = fit_inl4$pdf_data
  ) |>
  map(function(plot_df_list) {
    plot_df <-
      imap(plot_df_list, \(x, idx) mutate(x, name = idx)) |>
      bind_rows() |>
      mutate(name = factor(name, levels = names(coef(fit_lav))))
  }) |>
  bind_rows(.id = "method") |>
  mutate(method = factor(method, levels = c("skewnorm", "asymgaus", "marggaus", "sampling")))


ggplot() +
  geom_density(
    data = plot_df_blav, # |> filter(name %in% c("y3~~y3", "eta1~~eta2")),
    aes(value, fill = "MCMC"), col = NA, alpha = 0.38
  ) +
  geom_line(
    data = plot_df, # |> filter(name %in% c("y3~~y3", "eta1~~eta2")),
    aes(x, y, group = method, col = method), linewidth = 0.75
  ) +
  geom_vline(
    data = tibble(name = names(truval), truval = truval) |>
      # filter(name %in% c("y3~~y3", "eta1~~eta2")) |>
      mutate(name = factor(name, levels = names(coef(fit_lav)))),
    aes(xintercept = truval),
    linetype = "dashed"
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
