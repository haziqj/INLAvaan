library(tidyverse)
library(lavaan)
library(blavaan)
library(INLAvaan)
library(lme4)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)
nsamp <- 1e3

# fit_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

dat <- pivot_wider(sleepstudy, names_from = Days, names_prefix = "Day",
                   values_from = Reaction)
# dat <- brlavaan::gen_data_growth(1000)
mod <- "
  # intercept with coefficients fixed to 1
  i =~  1*Day0 + 1*Day1 + 1*Day2 + 1*Day3 + 1*Day4 +
        1*Day5 + 1*Day6 + 1*Day7 + 1*Day8 + 1*Day9

  # slope with coefficients fixed to 0:9 (number of days)
  s =~  0*Day0 + 1*Day1 + 2*Day2 + 3*Day3 + 4*Day4 +
        5*Day5 + 6*Day6 + 7*Day7 + 8*Day8 + 9*Day9

  i ~~ i
  i ~ 1

  s ~~ s
  s ~ 1

  i ~~ s

  # fix intercepts
  Day0 ~ 0*1
  Day1 ~ 0*1
  Day2 ~ 0*1
  Day3 ~ 0*1
  Day4 ~ 0*1
  Day5 ~ 0*1
  Day6 ~ 0*1
  Day7 ~ 0*1
  Day8 ~ 0*1
  Day9 ~ 0*1

  # apply equality constraints
  Day0 ~~ v*Day0
  Day1 ~~ v*Day1
  Day2 ~~ v*Day2
  Day3 ~~ v*Day3
  Day4 ~~ v*Day4
  Day5 ~~ v*Day5
  Day6 ~~ v*Day6
  Day7 ~~ v*Day7
  Day8 ~~ v*Day8
  Day9 ~~ v*Day9
  "

fit_lav  <- growth(mod, dat, ceq.simple = TRUE, std.ov = !TRUE)
fit_blav <- bgrowth(mod, dat, bcontrol = list(cores = 3), burnin = nsamp / 2, sample = nsamp)
fit_inl1 <- inlavaan(mod, dat, lavfun = "growth", method = "skewnorm")
fit_inl2 <- inlavaan(mod, dat, lavfun = "growth", method = "asymgaus")
fit_inl3 <- inlavaan(mod, dat, lavfun = "growth", method = "marggaus")
fit_inl4 <- inlavaan(mod, dat, lavfun = "growth", method = "sampling")

# Comparison
draws <- do.call("rbind", blavInspect(fit_blav, "mcmc"))
plot_df_blav <-
  as.data.frame(draws) |>
  pivot_longer(everything()) |>
  mutate(name = factor(name, levels = names(coef(fit_inl1))))

plot_df <-
  list(
    skewnorm = fit_inl1$pdf_data,
    asymgaus = fit_inl2$pdf_data,
    marggaus = fit_inl3$pdf_data,
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
