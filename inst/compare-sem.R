library(tidyverse)
library(lavaan)
library(blavaan)
library(INLAvaan)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)
nsamp <- 1e4

mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
"
dat <- PoliticalDemocracy

fit_lav  <- sem(mod, dat)
fit_blav <- bsem(mod, dat, bcontrol = list(cores = 3), burnin = nsamp / 2, sample = nsamp)
fit_inl1 <- inlavaan(mod, dat, method = "skewnorm")
fit_inl2 <- inlavaan(mod, dat, method = "asymgaus")
fit_inl3 <- inlavaan(mod, dat, method = "marggaus")
fit_inl4 <- inlavaan(mod, dat, method = "sampling")

# Comparison
draws <- do.call("rbind", blavInspect(fit_blav, "mcmc"))
plot_df_blav <-
  as.data.frame(draws) |>
  pivot_longer(everything()) |>
  mutate(name = factor(name, levels = names(coef(fit_blav))))

plot_df <-
  list(
    skewnorm = fit_inl1$pdf_data,
    asymgaus = fit_inl2$pdf_data
    # marggaus = fit_inl3$pdf_data,
    # sampling = fit_inl4$pdf_data
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
    data = plot_df_blav,
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
