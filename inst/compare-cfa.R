library(tidyverse)
library(lavaan)
library(blavaan)
library(INLAvaan)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)
nsamp <- 1e3

mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
dat <- HolzingerSwineford1939

DP <- dpriors(psi = "gamma(1,0.5)[sd]", theta = "gamma(1,0.5)[sd]")
STDLV <- FALSE
OPTIM <- "nlminb"

fit_lav  <- cfa(mod, dat, std.lv = STDLV)
fit_blav <- bcfa(mod, dat, bcontrol = list(cores = 3), burnin = nsamp / 2, sample = nsamp, std.lv = STDLV, dp = DP)
fit_inl1 <- inlavaan(mod, dat, lavfun = "cfa", method = "skewnorm", std.lv = STDLV, dp = DP, optim = OPTIM)
fit_inl2 <- inlavaan(mod, dat, lavfun = "cfa", method = "asymgaus", std.lv = STDLV)
fit_inl3 <- inlavaan(mod, dat, lavfun = "cfa", method = "marggaus", std.lv = STDLV)
fit_inl4 <- inlavaan(mod, dat, lavfun = "cfa", method = "sampling", std.lv = STDLV)

# Comparison
draws <- do.call("rbind", blavInspect(fit_blav, "mcmc"))
plot_df_blav <-
  as.data.frame(draws) |>
  pivot_longer(everything()) |>
  mutate(name = factor(name, levels = names(coef(fit_blav))))

plot_df <-
  list(
    skewnorm = fit_inl1$pdf_data,
    # asymgaus = fit_inl2$pdf_data,
    # marggaus = fit_inl3$pdf_data,
    sampling = fit_inl4$pdf_data
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
