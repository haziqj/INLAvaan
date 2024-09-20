library(tidyverse)
library(lavaan)

# partable needs to...
# 1. identify correlation parameter


# --- Data ---------------------------------------------------------------------
true_model <- "
  eta1 =~ 1*y1 + 1.2*y2 + 1.5*y3
  eta2 =~ 1*y4 + 1.2*y5 + 1.5*y6
  eta3 =~ 1*y7 + 1.2*y8 + 1.5*y9
  eta2 ~~ 0.3*eta1

  y1 ~~ 0.5*y4
  y1 ~~ 0.5*y7
  y4 ~~ 0.5*y7
"
dat <- lavaan::simulateData(true_model, sample.nobs = 1000)


# --- Models -------------------------------------------------------------------

# All parameters estimated
mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6
  eta2 ~ eta1

  y1 ~~ y4
  y1 ~~ y7
  y4 ~~ y7
"

LAV <- sem(mod, dat, do.fit = FALSE)
partable(LAV)
