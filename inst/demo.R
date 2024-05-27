library(tidyverse)
library(lavaan)
library(INLAvaan)

# 2-factor SEM model -----------------------------------------------------------
true_model <- "
  eta1 =~ 1*y1 + 1.2*y2 + 1.5*y3
  eta2 =~ 1*y4 + 1.2*y5 + 1.5*y6
  eta2 ~ 0.3*eta1

  y1 ~~ 0.05*y4

  y1 ~~ 0.1*y1
  y2 ~~ 0.1*y2
  y3 ~~ 0.1*y3
  y4 ~~ 0.1*y4
  y5 ~~ 0.1*y5
  y6 ~~ 0.1*y6
"
dat <- lavaan::simulateData(true_model, sample.nobs = 1000)

mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
"
fit <- isem(model = mod, data = dat, meanstructure = FALSE, verbose = TRUE)



tmp <- fit
do.call("inlav_model_test", tmp)

# Political democracy SEM example ----------------------------------------------
myModel <- '
  # latent variables
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  ind60 =~ x1 + x2 + x3

  # latent regressions
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
'

fit <- isem(
  model = myModel,
  data = PoliticalDemocracy,
  meanstructure = FALSE,
  verbose = TRUE
)

fit_lav <- sem(myModel, data = PoliticalDemocracy)

inla_coef <- coef(fit)
lav_coef <- coef(sem(myModel, data = PoliticalDemocracy, meanstructure = FALSE))

partable(fit) |>
  filter(free > 0) |>
  select(id, inla = est, pxnames, lhs, op, rhs) |>
  mutate(type = gsub("\\[[^]]*\\]", "", pxnames)) |>
  mutate(type = case_when(
    type == "theta" & lhs != rhs ~ "theta_cov",
    TRUE ~ type
  )) |>
  left_join(select(partable(fit_lav), id, lavaan = est), by = "id") |>
  ggplot(aes(lavaan, inla)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(col = type), size = 3) +
  theme_bw()



