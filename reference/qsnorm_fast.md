# Fast Approximation of Skew-Normal Quantile Function

A fast approximation of skew-normal quantiles using the high-performance
approximation algorithm from the INLA GMRFLib C source, and originally
by Thomas Luu (see details for reference).

## Usage

``` r
qsnorm_fast(p, xi = 0, omega = 1, alpha = 0)
```

## Arguments

- p:

  Vector of probabilities.

- xi:

  Location parameter (numeric vector).

- omega:

  Scale parameter (numeric vector).

- alpha:

  Shape parameter (numeric vector).

## Value

Vector of quantiles.

## Details

This function implements a high-performance approximation for the
skew-normal quantile function based on the algorithm described by Luu
(2016). The method uses a domain decomposition strategy to achieve high
accuracy (\\\< 10^{-7}\\ relative error) without iterative numerical
inversion.

The domain is split into two regions:

- **Tail Regions:** For extreme probabilities where \\\vert u \vert\\ is
  large, the quantile is approximated using the Lambert W-function,
  \\W(z)\\, solving \\z = \Phi(q)\\ via asymptotic expansion: \$\$q
  \approx \sqrt{2 W\left(\frac{1}{2\pi (1-p)^2}\right)}\$\$

- **Central Region:** For the main body of the distribution, the
  function uses a high-order Taylor expansion of the inverse error
  function around a carefully selected expansion point \$x_0\$:
  \$\$\Phi^{-1}(p) \approx \sum\_{k=0}^5 c_k (z - x_0)^k\$\$

This approach is significantly faster than standard numerical inversion
(e.g., `uniroot`) while maintaining sufficient precision for most
statistical applications.

## References

Luu, T. (2016). *Fast and accurate parallel computation of quantile
functions for random number generation* \#' (Doctoral thesis). UCL
(University College London). <https://discovery.ucl.ac.uk/1482128/>
