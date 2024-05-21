library(tidyverse)
library(lavaan)

# 1-factor
true_model <- "
  # factor model
  eta =~ 1.0*y1 + 0.7*y2 + 0.6*y3 + 0.5*y4 + 0.4*y5

  # variance and covariance definitions for each observed variable
  y1 ~~ 1*y1
  y2 ~~ (1 - 0.7^2)*y2
  y3 ~~ (1 - 0.6^2)*y3
  y4 ~~ (1 - 0.5^2)*y4
  y5 ~~ (1 - 0.4^2)*y5

  # variance of the latent factor
  eta ~~ 1*eta
"
dat <- lavaan::simulateData(true_model, sample.nobs = 500) |>
  as_tibble() |>
  mutate(y1 = y1 + 1,
         y2 = y2 + 0.7,
         y3 = y3 + 0.6,
         y4 = y4 + 0.5,
         y5 = y5 + 0.4)

fit <- inlavaan(model = "eta =~ y1 + y2 + y3 + y4 + y5", data = dat)

# 3-factor model ---------------------------------------------------------------
true_model <- "
  # factor model
  eta1 =~ 1.0*y1 + 0.7*y2 + 0.6*y3 + 0.5*y4 + 0.4*y5
  eta2 =~ 1.0*y6 + 0.7*y7 + 0.6*y8 + 0.5*y9 + 0.4*y10
  eta3 =~ 1.0*y11 + 0.7*y12 + 0.6*y13 + 0.5*y14 + 0.4*y15

  # variance and covariance definitions for each observed variable
  y1 ~~ 1*y1
  y2 ~~ (1 - 0.7^2)*y2
  y3 ~~ (1 - 0.6^2)*y3
  y4 ~~ (1 - 0.5^2)*y4
  y5 ~~ (1 - 0.4^2)*y5
  y6 ~~ 1*y6
  y7 ~~ (1 - 0.7^2)*y7
  y8 ~~ (1 - 0.6^2)*y8
  y9 ~~ (1 - 0.5^2)*y9
  y10 ~~ (1 - 0.4^2)*y10
  y11 ~~ 1*y11
  y12 ~~ (1 - 0.7^2)*y12
  y13 ~~ (1 - 0.6^2)*y13
  y14 ~~ (1 - 0.5^2)*y14
  y15 ~~ (1 - 0.4^2)*y15

  # variance of the latent factor
  eta1 ~~ 0.8*eta1
  eta2 ~~ 0.7*eta2
  eta3 ~~ 0.6*eta3
  eta1 ~~ 0.5*eta2
  eta1 ~~ 0.4*eta3
  eta2 ~~ 0.3*eta3
"
dat <- lavaan::simulateData(true_model, sample.nobs = 100)


mod <- "
eta1 =~ y1 + y2 + y3 + y4 + y5
eta2 =~ y6 + y7 + y8 + y9 + y10
eta3 =~ y11 + y12 + y13 + y14 + y15
"
fit <- inlavaan(mod, dat)

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

fit <- inlavaan(
  model = myModel,
  data = PoliticalDemocracy,
  int.ov.free = TRUE,
  int.lv.free = FALSE,
  auto.fix.first = TRUE,
  auto.fix.single = TRUE,
  auto.var = TRUE,
  auto.cov.lv.x = TRUE,
  auto.efa = TRUE,
  auto.th = TRUE,
  auto.delta = TRUE,
  auto.cov.y = TRUE
)
