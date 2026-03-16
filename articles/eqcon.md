# Equality constraints

``` r
library(INLAvaan)

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
dat <- reshape(
  lme4::sleepstudy,
  timevar = "Days",
  idvar = "Subject",
  direction = "wide"
)
names(dat) <- sub("^Reaction\\.(.*)$", "Day\\1", names(dat))
fit <- agrowth(mod, dat)
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [77ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [96ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.297σ. [295ms]
#> 
#> ⠙ Fitting 0/6 skew-normal marginals.
#> ✔ Fitting 6/6 skew-normal marginals. [128ms]
#> 
#> ℹ Adjusting copula correlations (NORTA).
#> ✔ Adjusting copula correlations (NORTA). [36ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Posterior sampling and summarising. [320ms]
#> 
coef(fit)
#>      i~~i       i~1      s~~s       s~1      i~~s         v         v         v 
#> 12062.688    32.723    39.729     9.638     0.123   622.613   622.613   622.613 
#>         v         v         v         v         v         v         v 
#>   622.613   622.613   622.613   622.613   622.613   622.613   622.613
```
