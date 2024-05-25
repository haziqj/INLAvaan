library(tidyverse)
library(lavaan)

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
  eta1 ~ eta2
  y1 ~~ y4
"
fit <- isem(model = mod, data = dat, meanstructure = FALSE)



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
  data = PoliticalDemocracy
)
inla_coef <- coef(fit)

lav_coef <- coef(sem(myModel, data = PoliticalDemocracy, meanstructure = TRUE))
