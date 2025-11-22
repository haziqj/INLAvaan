library(tidyverse)
library(lavaan)
library(INLAvaan)

true_model <- "
  eta1 =~ 1*y1 + 1.2*y2 + 1.5*y3
  eta2 =~ 1*y4 + 1.2*y5 + 1.5*y6
  eta2 ~~ 0.3*eta1
"
dat <- lavaan::simulateData(true_model, sample.nobs = 1000)

mod <- "
  eta1 =~ y1 + y2 + y3
  eta2 =~ y4 + y5 + y6

  y1 ~~ y4
"
fit <- sem(mod, dat)
pt <- as_tibble(inlavaanify_partable(fit@ParTable, lavdata = fit@Data, lavoptions = fit@Options))


THETA <- c(1.2, 1.5, 1.2, 1.5, rep(0, 7), rep(0, 2), atanh(0.3 * (1 + 1e-6)))

pars_to_x(THETA, pt)


fit <- inlavaan(mod, dat)


myModel <- '
 # latent variables
   ind60 =~ x1 + x2 + x3
   dem60 =~ y1 + y2 + y3 + y4
   dem65 =~ y5 + y6 + y7 + y8
 # regressions
   dem60 ~ ind60
   dem65 ~ ind60 + dem60
 # residual covariances
   y1 ~~ y5
   y2 ~~ y4 + y6
   y3 ~~ y7
   y4 ~~ y8
   y6 ~~ y8
'
fit <- inlavaan(model = myModel,
           data = PoliticalDemocracy)
summary(fit)


coef(sem(myModel, PoliticalDemocracy))
# ind60=~x2    ind60=~x3    dem60=~y2    dem60=~y3    dem60=~y4    dem65=~y6
# 2.180        1.819        1.257        1.058        1.265        1.186
# dem65=~y7    dem65=~y8  dem60~ind60  dem65~ind60  dem65~dem60       y1~~y5
# 1.280        1.266        1.483        0.572        0.837        0.624
# y2~~y4       y2~~y6       y3~~y7       y4~~y8       y6~~y8       x1~~x1
# 1.313        2.153        0.795        0.348        1.356        0.082
# x2~~x2       x3~~x3       y1~~y1       y2~~y2       y3~~y3       y4~~y4
# 0.120        0.467        1.891        7.373        5.067        3.148
# y5~~y5       y6~~y6       y7~~y7       y8~~y8 ind60~~ind60 dem60~~dem60
# 2.351        4.954        3.431        3.254        0.448        3.956
# dem65~~dem65
# 0.172
