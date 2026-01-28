# Mediation Analysis

Mediation analysis ([Yuan and MacKinnon 2009](#ref-yuan2009bayesian))
allows researchers to investigate the mechanism by which an independent
variable ($X$) influences a dependent variable ($Y$). Rather than just
asking “Does X affect Y?”, mediation asks “Does X affect Y through an
intermediate variable M?”

Common examples include:

- **Psychology:** Does a therapy ($X$) reduce anxiety ($M$), which in
  turn improves sleep quality ($Y$)?
- **Medicine:** Does a new drug ($X$) lower blood pressure ($M$),
  thereby decreasing the risk of heart attack ($Y$)?

In this vignette, we demonstrate how to estimate a simple mediation
model using [INLAvaan](https://inlavaan.haziqj.ml/). We will fit a
standard three-variable mediation model:

``` mermaid
graph LR
    X((X)) -->|a| M((M))
    M -->|b| Y((Y))
    X -->|c| Y
```

- $a$: The effect of $X$ on $M$.
- $b$: The effect of $M$ on $Y$.
- $c$: The direct effect of $X$ on $Y$.
- $a \times b$: The indirect effect (the mediation effect).

In a mediation model, the *Total Effect* represents the overall impact
of $X$ on $Y$, ignoring the specific pathway. It answers the question:
“If I change $X$, how much does $Y$ change in *total*, regardless of
whether it goes through $M$ or not?”.

## Data Simulation

To verify that [INLAvaan](https://inlavaan.haziqj.ml/) recovers the
correct parameters, we simulate data where the “truth” is known. The
logic is as follows: Generate…

1.  $X$ normally;
2.  $M$ dependent on $X$ with a coefficient of 0.5; and
3.  $Y$ dependent only on $M$ with a coefficient of 0.7.

Critically, we do not add $X$ to the generation of $Y$. This means the
true direct effect ($c$) is 0, and the relationship is fully mediated.
We expect our model to estimate $a \approx 0.5$, $b \approx 0.7$, and
the indirect effect $ab \approx 0.35$. The direct effect $c$ should be
close to zero.

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
#> ℹ Finding posterior mode.
#> ✔ Finding posterior mode. [28ms]
#> 
#> ℹ Computing the Hessian.
#> ✔ Computing the Hessian. [62ms]
#> 
#> ℹ Performing VB correction.
#> ✔ VB correction; mean |δ| = 0.012σ. [168ms]
#> 
#> ⠙ Fitting skew normal to 0/7 marginals.
#> ✔ Fitting skew normal to 7/7 marginals. [192ms]
#> 
#> ⠙ Computing ppp and DIC.
#> ✔ Computing ppp and DIC. [357ms]
#> 
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
#> INLAvaan 0.2.3 ended normally after 5 iterations
#> 
#>   Estimator                                      BAYES
#>   Optimization method                           NLMINB
#>   Number of model parameters                         7
#> 
#>   Number of observations                           100
#> 
#> Model Test (User Model):
#> 
#>    Marginal log-likelihood                    -311.858 
#>    PPP (Chi-square)                              0.612 
#> 
#> Information Criteria:
#> 
#>    Deviance (DIC)                              568.307 
#>    Effective parameters (pD)                     6.697 
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
#>     X          (a)    0.525    0.108    0.315    0.736    0.000    normal(0,10)
#>   Y ~                                                                          
#>     M          (b)    0.771    0.099    0.577    0.964    0.000    normal(0,10)
#> 
#> Intercepts:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .Y                -0.071    0.098   -0.263    0.122    0.000    normal(0,32)
#>    .M                 0.126    0.099   -0.068    0.319    0.000    normal(0,32)
#> 
#> Variances:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>    .Y                 0.977    0.145    0.733    1.303    0.006 gamma(1,.5)[sd]
#>    .M                 0.998    0.147    0.749    1.325    0.006 gamma(1,.5)[sd]
#> 
#> Defined Parameters:
#>                    Estimate       SD     2.5%    97.5%     NMAD    Prior       
#>     ab                0.403    0.095    0.249    0.593                         
#>     total             0.347    0.126    0.105    0.587
```

Looking at the Regressions and Defined Parameters sections of the
output:

- Both intercepts are non-significant, since we simulated data with true
  means of zero.
- Path $a$ (`M ~ X`) estimated at 0.525 (true value 0.5).
- Path $b$ (`Y ~ M`) estimated at 0.771 (true value 0.7).
- Path $c$ (`Y ~ X`) estimated at -0.060. The 95% Credible Interval
  \[-0.291, 0.171\] includes zero, correctly identifying that there is
  no direct effect.
- Indirect Effect $ab$ estimated at 0.403 (true value 0.35). The
  interval \[0.249, 0.593\] does not cross zero, indicating significant
  mediation.
- Total Effect estimated at 0.347.
  - This is the sum of the direct and indirect effects ($c + ab$).
  - It tells us that a 1-unit increase in $X$ leads to a total increase
    of roughly 0.347 in $Y$.
  - **Note:** In this simulation, even though the *direct* effect is
    non-significant (close to zero), the *total* effect is significant
    because the mechanism via $M$ is strong. This illustrates a “full
    mediation” scenario: $X$ affects $Y$, but *only* because of $M$.

## References

Yuan, Ying, and David P. MacKinnon. 2009. “Bayesian Mediation Analysis.”
*Psychological Methods* 14 (4): 301–22.
<https://doi.org/10.1037/a0016972>.
