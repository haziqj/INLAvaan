library(tidyverse)
library(lavaan)

# 2-factor SEM model -----------------------------------------------------------
true_model <- "
  eta1 =~ 1*y1 + 1.2*y2 + 1.5*y3
  eta2 =~ 1*y4 + 1.2*y5 + 1.5*y6
  eta2 ~ 0.3*eta1

  y1 ~~ y1
  y2 ~~ y2
  y3 ~~ y3
  y4 ~~ y4
  y5 ~~ y5
  y6 ~~ y6
"
dat <- lavaan::simulateData(true_model, sample.nobs = 1000)

mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  eta1 ~ eta2
"
fit <- isem(model = mod, data = dat)

# tmp <- fit
# do.call("coeffun_inla", tmp)

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


coeffun_inla(
  fit$lavpartable,
  fit$pxpartable,
  fit$res
)
