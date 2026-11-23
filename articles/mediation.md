# Mediation Analysis

Mediation analysis ([Yuan and MacKinnon 2009](#ref-yuan2009bayesian))
allows researchers to investigate the mechanism by which an independent
variable (\\X\\) influences a dependent variable (\\Y\\). Rather than
just asking “Does X affect Y?”, mediation asks “Does X affect Y through
an intermediate variable M?”

Common examples include:

- **Psychology:** Does a therapy (\\X\\) reduce anxiety (\\M\\), which
  in turn improves sleep quality (\\Y\\)?
- **Medicine:** Does a new drug (\\X\\) lower blood pressure (\\M\\),
  thereby decreasing the risk of heart attack (\\Y\\)?

In this vignette, we demonstrate how to estimate a simple mediation
model using [INLAvaan](https://inlavaan.haziqj.ml/). We will fit a
standard three-variable mediation model:

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMzIwIDE4MCIgd2lkdGg9IjMyMCIgc3R5bGU9Im1heC13aWR0aDogMTAwJTsgaGVpZ2h0OiBhdXRvOyIgcm9sZT0iaW1nIiBhcmlhLWxhYmVsPSJNZWRpYXRpb24gZGlhZ3JhbTogWCBhZmZlY3RzIE0gKHBhdGggYSksIE0gYWZmZWN0cyBZIChwYXRoIGIpLCBhbmQgWCBhZmZlY3RzIFkgZGlyZWN0bHkgKHBhdGggYykuIiBmb250LWZhbWlseT0ic2VyaWYiIGZvbnQtc3R5bGU9Iml0YWxpYyIgZm9udC1zaXplPSIxOCIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMS41Ij48Y2lyY2xlIGN4PSI1MCIgY3k9IjEzNSIgcj0iMjIiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjE2MCIgY3k9IjQwIiByPSIyMiI+PC9jaXJjbGU+PGNpcmNsZSBjeD0iMjcwIiBjeT0iMTM1IiByPSIyMiI+PC9jaXJjbGU+PGxpbmUgeDE9IjY2LjciIHkxPSIxMjAuNiIgeDI9IjEzNS43IiB5Mj0iNjAuOSI+PC9saW5lPjxsaW5lIHgxPSIxNzYuNyIgeTE9IjU0LjQiIHgyPSIyNDUuNyIgeTI9IjExNC4xIj48L2xpbmU+PGxpbmUgeDE9IjcyIiB5MT0iMTM1IiB4Mj0iMjM4IiB5Mj0iMTM1Ij48L2xpbmU+PGcgZmlsbD0iY3VycmVudENvbG9yIiBzdHJva2U9Im5vbmUiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj48cG9seWdvbiBwb2ludHM9IjE0My4zLDU0LjQgMTM5LjAsNjQuNyAxMzIuNSw1Ny4yIj48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIyNTMuMywxMjAuNiAyNDIuNSwxMTcuOCAyNDkuMCwxMTAuMyI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMjQ4LDEzNSAyMzgsMTQwIDIzOCwxMzAiPjwvcG9seWdvbj48dGV4dCB4PSI1MCIgeT0iMTM1Ij5YPC90ZXh0Pjx0ZXh0IHg9IjE2MCIgeT0iNDAiPk08L3RleHQ+PHRleHQgeD0iMjcwIiB5PSIxMzUiPlk8L3RleHQ+PHRleHQgeD0iOTIiIHk9Ijc2Ij5hPC90ZXh0Pjx0ZXh0IHg9IjIyOCIgeT0iNzYiPmI8L3RleHQ+PHRleHQgeD0iMTYwIiB5PSIxNTUiPmM8L3RleHQ+PC9nPjwvc3ZnPg==)

- \\a\\: The effect of \\X\\ on \\M\\.
- \\b\\: The effect of \\M\\ on \\Y\\.
- \\c\\: The direct effect of \\X\\ on \\Y\\.
- \\a \times b\\: The indirect effect (the mediation effect).

In a mediation model, the *Total Effect* represents the overall impact
of \\X\\ on \\Y\\, ignoring the specific pathway. It answers the
question: “If I change \\X\\, how much does \\Y\\ change in *total*,
regardless of whether it goes through \\M\\ or not?”.

## Data Simulation

To verify that [INLAvaan](https://inlavaan.haziqj.ml/) recovers the
correct parameters, we simulate data where the “truth” is known. The
logic is as follows: Generate…

1.  \\X\\ normally;
2.  \\M\\ dependent on \\X\\ with a coefficient of 0.5; and
3.  \\Y\\ dependent only on \\M\\ with a coefficient of 0.7.

Critically, we do not add \\X\\ to the generation of \\Y\\. This means
the true direct effect (\\c\\) is 0, and the relationship is fully
mediated. We expect our model to estimate \\a \approx 0.5\\, \\b \approx
0.7\\, and the indirect effect \\ab \approx 0.35\\. The direct effect
\\c\\ should be close to zero.

``` r

set.seed(11)
n <- 100  # sample size

# 1. Predictor
X <- rnorm(n)

# 2. Mediator (Path a = 0.5)
M <- 0.5 * X + rnorm(n)

# 3. Outcome (Path b = 0.7, Path c = 0)
Y <- 0.7 * M + rnorm(n) 

dat <- data.frame(X = X, Y = Y, M = M)
```

## Model Specification and Fit

The standard `lavaan` syntax for a mediation model is straightforward
(note the use of the `:=` operator to define the indirect effect as a
new parameter.):

``` r

mod <- "
  # Direct effect (path c)
  Y ~ c*X

  # Mediator paths (path a and b)
  M ~ a*X
  Y ~ b*M

  # Define Indirect effect (a*b)
  ab := a*b

  # Define Total effect
  total := c + (a*b)
"
```

The model is fit using
[`asem()`](https://inlavaan.haziqj.ml/reference/asem.md). The
`meanstructure = TRUE` argument is supplied to estimate intercepts for
the variables.

``` r

library(INLAvaan)
fit <- asem(mod, dat, meanstructure = TRUE)
#> ℹ Mode finding and Hessian computation.
#> ✔ Posterior mode and Hessian. [75ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.073σ. [214ms]
#> 
#> ⠙ Fitting 0/7 skew-normal marginals.
#> ✔ Fit 7/7 skew-normal marginals. [124ms]
#> 
#> ⠙ Posterior sampling and summarising.
#> ✔ Summarise 1000 posterior draws. [518ms]
#> 
#> ℹ Fit measures: PPP, DIC.
```

The user may wish to specify different prior distributions for the
parameters. See the relevant section in the [Get
started](https://inlavaan.haziqj.ml/articles/INLAvaan.html#setting-priors)
vignetted for further details.

## Results

The summary output provides the posterior mean, standard deviation, and
95% credible intervals for all paths.

``` r

summary(fit)
#> INLAvaan 0.3.4.9007 ended normally after 5 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                         7
#> 
#>   Number of observations                           100
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                    -308.482 
#>    PPP (Chi-square)                              0.519 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                              568.329 
#>    Effective parameters (pD)                     6.719 
#> 
#> Parameter Estimates:
#> 
#>    Marginalisation method                     SKEWNORM
#>    VB correction                                  TRUE
#> 
#> Regressions:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>   Y ~                                                                          
#>     X          (c)   -0.060    0.118   -0.291    0.171    0.000    normal(0,10)
#>   M ~                                                                          
#>     X          (a)    0.525    0.108    0.314    0.736    0.000    normal(0,10)
#>   Y ~                                                                          
#>     M          (b)    0.771    0.099    0.577    0.964    0.000    normal(0,10)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .Y                -0.071    0.098   -0.263    0.121    0.000    normal(0,32)
#>    .M                 0.126    0.099   -0.068    0.319    0.000    normal(0,32)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .Y                 0.977    0.143    0.735    1.297    0.004 gamma(1,.5)[sd]
#>    .M                 0.998    0.146    0.752    1.324    0.004 gamma(1,.5)[sd]
#> 
#> Defined Parameters:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>     ab                0.406    0.097    0.227    0.609                         
#>     total             0.349    0.136    0.083    0.615
```

Looking at the Regressions and Defined Parameters sections of the
output:

- Both intercepts are non-significant, since we simulated data with true
  means of zero.
- Path \\a\\ (`M ~ X`) estimated at 0.525 (true value 0.5).
- Path \\b\\ (`Y ~ M`) estimated at 0.771 (true value 0.7).
- Path \\c\\ (`Y ~ X`) estimated at -0.060. The 95% Credible Interval
  \[-0.291, 0.171\] includes zero, correctly identifying that there is
  no direct effect.
- Indirect Effect \\ab\\ estimated at 0.406 (true value 0.35). The
  interval \[0.227, 0.609\] does not cross zero, indicating significant
  mediation.
- Total Effect estimated at 0.349.
  - This is the sum of the direct and indirect effects (\\c + ab\\).
  - It tells us that a 1-unit increase in \\X\\ leads to a total
    increase of roughly 0.349 in \\Y\\.
  - **Note:** In this simulation, even though the *direct* effect is
    non-significant (close to zero), the *total* effect is significant
    because the mechanism via \\M\\ is strong. This illustrates a “full
    mediation” scenario: \\X\\ affects \\Y\\, but *only* because of
    \\M\\.

## References

Yuan, Ying, and David P. MacKinnon. 2009. “Bayesian Mediation Analysis.”
*Psychological Methods* 14 (4): 301–22.
<https://doi.org/10.1037/a0016972>.
