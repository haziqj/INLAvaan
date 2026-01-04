testthat::skip_on_ci()
testthat::skip_on_cran()
testthat::skip_if_not(interactive())


data("PoliticalDemocracy", package = "lavaan")

model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
     dem65 =~ y5 + y6 + y7 + y8

  # Latent regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

  # Custom priors on latent variances
    # ind60 ~~ prior('gamma(1, 1)')*ind60
    # dem60 ~~ prior('gamma(1,.9)')*dem60
    # dem65 ~~ prior('gamma(1,.5)')*dem65
"

fit <- asem(model, PoliticalDemocracy)

summary(fit)

## to get vcov, it seems we need to set @Options$se?
library(lavaan)
vcov(fit)

fit@Options$se <- "standard"
vcov(fit)

## would be nice, even if it just returns dic and ppp:
fitMeasures(fit)


## missing data: any possibility of a "full information" approach where you skip over the NAs?
set.seed(9619)
mis <- matrix(
  rbinom(prod(dim(PoliticalDemocracy)), 1, .9),
  nrow(PoliticalDemocracy),
  ncol(PoliticalDemocracy)
)
pd <- PoliticalDemocracy * mis
pd[pd == 0] <- NA

fitm <- asem(
  model,
  data = pd,
  # dp = blavaan::dpriors(lambda = "normal(1,.3)"),
  # missing = "ml",
)

summary(fitm)


## CFA
data("HolzingerSwineford1939", package = "lavaan")

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit2 <- acfa(HS.model, data = HolzingerSwineford1939)

fit2b <- acfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE)

summary(fit2b)


## Multi-group cfa with std.lv and group.equal
HS.model <- ' visual  =~ x1 + x2 + x3 '

fit2 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings",
  dp = blavaan::dpriors(lambda = "normal(1,.3)")
)

summary(fit2)


## Approximate constraints could be interesting
fit3 <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings"),
  wiggle = "intercepts",
  wiggle.sd = .1
)


## 4-group model with a bunch of constraints
data(StereotypeThreat, package = "psychotools")
StereotypeThreat <- transform(
  StereotypeThreat,
  group = interaction(ethnicity, condition)
)
StereotypeThreat <- StereotypeThreat[order(StereotypeThreat$group), ]

model <- ' f1 =~ abstract + verbal + c(l1,l1,l1,l4)*numerical
           f1 ~  c(maj,min1,maj,min2)*1 + c(NA,0,NA,0)*1
           abstract ~ c(ar1,ar2,ar3,ar3)*1
           numerical  ~ c(na1,na1,na1,na4)*1
           numerical ~~ c(e1,e1,e1,e4)*numerical
           f1 ~~ c(v1.maj,v1.min,v1.maj,v1.min)*f1
         '

fit <- acfa(
  model,
  data = StereotypeThreat,
  group = "group",
  group.equal = c("loadings", "residuals", "intercepts")
)

summary(fit)

lavPredict(fit) # I believe these are coming from lavaan


## constraint on covariances (or on correlations, depending on parameterization)
## (this is a place where I think blavaan will constrain correlations instead of covariances.
##  that might not be the right thing to do, but equality constraints on covariances are
##  already weird.)
HS.modelc <- ' visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9
               x1 ~~ c(a,b)*x4
               x2 ~~ c(a,b)*x5 '

fit2c <- acfa(
  HS.modelc,
  data = HolzingerSwineford1939,
  group = "school",
  dp = blavaan::dpriors(lambda = 'normal(1,.5)')
)

summary(fit2c)


## std.lv=TRUE with equality constraints, where mcmc can choke on sign indeterminacy
mod2 <- ' visual  =~ c("l1","l1")*x1 + c("l2","l2")*x2 + c("l3","l3")*x3
          textual =~ c("l4","l4")*x4 + c("l5","l5")*x5 + c("l6","l6")*x6
          visual ~~ c(1, NA)*visual
          textual ~~ c(1, NA)*textual '

fit2 <- acfa(
  mod2,
  data = HolzingerSwineford1939,
  group = "school",
  std.lv = TRUE,
  meanstructure = TRUE
)

summary(fit2)


## growth model, also see the examples at https://osf.io/4bpmq
data("Demo.growth")
colnames(Demo.growth)[1:4] = c("X1", "X2", "X3", "X4")

#LCS specification (constant change)
constantX.syntax <- '

#X

#latent variables
lX1 =~ 1*X1; lX2 =~ 1*X2; lX3 =~ 1*X3; lX4 =~ 1*X4;

#autoregressions
lX2 ~ 1*lX1; lX3 ~ 1*lX2; lX4 ~ 1*lX3;

#change - delta; d
dX1 =~ 1*lX2; dX2 =~ 1*lX3; dX3 =~ 1*lX4;

#intercept and slope
intX =~ 1*lX1;
slopeX =~ 1*dX1 + 1*dX2 + 1*dX3;

#residuals equal
X1 ~~ residX*X1; X2 ~~ residX*X2; X3 ~~ residX*X3; X4 ~~ residX*X4;


#manifest means @0
X1 ~ 0*1; X2 ~0*1; X3 ~ 0*1; X4 ~ 0*1;

#auto-proportions
dX1 ~ 0*lX1; dX2 ~ 0*lX2; dX3 ~ 0*lX3;

#slope and intercept means
slopeX ~ 1;
intX ~ 1;

#Latent variances and covariance
slopeX ~~ slopeX;
intX ~~ intX;
slopeX ~~ intX;

#means and vars @0
lX1 ~ 0*1; lX2 ~0*1; lX3 ~ 0*1; lX4 ~ 0*1;
dX1 ~ 0*1; dX2 ~0*1; dX3 ~ 0*1;

lX1 ~~ 0*lX1; lX2 ~~ 0*lX2; lX3 ~~ 0*lX3; lX4 ~~ 0*lX4;
dX1 ~~ 0*dX1; dX2 ~~ 0*dX2; dX3 ~~ 0*dX3;

'

fit <- inlavaan(constantX.syntax, data = Demo.growth)


## non-recursive (cyclic) model
set.seed(1234)
pop.model <- ' dv1 ~ 0.3*dv3
               dv2 ~ 0.5*dv1
               dv3 ~ 0.7*dv2 '
Data <- lavaan::simulateData(pop.model, sample.nobs = 500)

model <- ' dv1 ~ dv3
           dv2 ~ dv1
           dv3 ~ dv2 '

fit <- asem(model, data = Data, dp = blavaan::dpriors(beta = "normal(.5,.5)"))
