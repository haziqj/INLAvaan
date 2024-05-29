library(tidyverse)
library(lavaan)
library(blavaan)
library(INLAvaan)
library(furrr)
plan("multisession", workers = parallel::detectCores() - 2)

gen_data <- function(n = 100) {
  true_model <- "
  eta1 =~ 1*y1 + 1.2*y2 + 1.5*y3
  eta2 =~ 1*y4 + 1.2*y5 + 1.5*y6
  eta2 ~ 0.3*eta1

  y1 ~~ 0.05*y4
  y2 ~~ 0.05*y5
  y3 ~~ 0.05*y6

  y1 ~~ 0.1*y1
  y2 ~~ 0.1*y2
  y3 ~~ 0.1*y3
  y4 ~~ 0.1*y4
  y5 ~~ 0.1*y5
  y6 ~~ 0.1*y6
"
  lavaan::simulateData(true_model, sample.nobs = n)
}

mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  eta2 ~ eta1
  y1 ~~ y4
  y2 ~~ y5
  y3 ~~ y6
"

run_blav_sim <- function(N, target = "stan") {
  fit_blav <-
    bsem(
      model = mod,
      data = gen_data(n = N),
      target = target,
      # meanstructure = TRUE,
      n.chains = 1,
      burnin = 1000,
      sample = 2000
      # bcontrol = list(cores = 3)
    )
  garb <- capture.output(tmp <- summary(fit_blav, neff = TRUE))
  neff <-
    tmp[, "    neff"] |>
    as.numeric() |>
    min(na.rm = TRUE) |>
    suppressWarnings()

  tibble(
    n = N,
    time = as.numeric(fit_blav@timing$total),
    neff = neff
  )
}

run_inlav_sim <- function(N) {
  require(lavaan)
  require(INLAvaan)
  fit_inla <- isem(model = mod, data = gen_data(n = N))

  tibble(
    n = N,
    time = as.numeric(fit_inla@timing$total),
    neff = NA
  )
}

# Run simulations --------------------------------------------------------------
res <-
  tibble(n = rep(c(100, 250, 500, 1000, 2500, 5000, 7500, 10000), each = 10)) |>
  mutate(
    res_blav = future_map(
      .x = n,
      .f = run_blav_sim,
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    ),
    res_blavvb = future_map(
      .x = n,
      .f = \(x) run_blav_sim(x, "vb"),
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    ),
    res_inla = map(
      .x = n,
      .f = run_inlav_sim,
      .progress = TRUE
    ),
  )
# save(res, file = "inst/timing.RData")

res |>
  unnest(c(res_blav, res_blavvb, res_inla), names_sep = "_") |>
  select(n, ends_with("time")) |>
  rename(
    blavaan = res_blav_time,
    blavaan_vb = res_blavvb_time,
    INLAvaan = res_inla_time
  ) |>
  pivot_longer(
    cols = -n,
    names_to = "method",
    values_to = "time"
  ) |>
  ggplot(aes(n, time, col = method)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Sample size", y = "Run time (s)",
       title = "Total run time to fit two factor SEM with varying sample sizes")

# Which solver to use? ---------------------------------------------------------
library(microbenchmark)
library(Matrix)
library(MASS)

safe_solve <- function(x) {
  try_chol <- try(chol(x), silent = TRUE)
  if (any(class(try_chol) %in% "try-error")) {
    xm <- forceSymmetric(Matrix(x))
    return(solve(xm))
  } else {
    return(chol2inv(try_chol))
  }
}

set.seed(42)
A <- matrix(rnorm(36), nrow = 6)
Sigma <- A %*% t(A)

microbenchmark(
  solve_base = solve(Sigma),
  chol2inv_chol = chol2inv(chol(Sigma)),
  MASS_ginv = ginv(Sigma),
  Matrix_solve = solve((Matrix(Sigma))),
  Matrix_chol2inv = solve(chol((Matrix(Sigma)))),
  safe_solve = safe_solve(Sigma),
  times = 1000
)
