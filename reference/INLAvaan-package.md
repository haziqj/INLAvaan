# INLAvaan: Approximate Bayesian Latent Variable Analysis

Implements approximate Bayesian inference for Structural Equation Models
(SEM) using a custom adaptation of the Integrated Nested Laplace
Approximation (Rue et al., 2009)
[doi:10.1111/j.1467-9868.2008.00700.x](https://doi.org/10.1111/j.1467-9868.2008.00700.x)
as described in Jamil and Rue (2026a)
[doi:10.48550/arXiv.2603.25690](https://doi.org/10.48550/arXiv.2603.25690)
. Provides a computationally efficient alternative to Markov Chain Monte
Carlo (MCMC) for Bayesian estimation, allowing users to fit latent
variable models using the 'lavaan' syntax. See also the companion paper
on implementation and workflows, Jamil and Rue (2026b)
[doi:10.48550/arXiv.2604.00671](https://doi.org/10.48550/arXiv.2604.00671)
.

## Main features

- [`acfa()`](https://inlavaan.haziqj.ml/reference/acfa.md): Approximate
  Confirmatory Factor Analysis.

- [`asem()`](https://inlavaan.haziqj.ml/reference/asem.md): Approximate
  Structural Equation Modelling.

- [`agrowth()`](https://inlavaan.haziqj.ml/reference/agrowth.md):
  Approximate Latent Growth Curve models.

## Model specifications

Supports advanced 'lavaan' syntax features, including:

- Equality constraints

- Defined parameters (e.g., `:=` operator for indirect effects)

- Flexible prior specifications

## Methods for `INLAvaan` objects

After fitting a model an INLAvaan object is returned. The following S4
methods are available. See
[INLAvaan](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md) for
the class definition.

- Summaries and parameter estimates:
  [`summary()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
  [`coef()`](https://inlavaan.haziqj.ml/reference/INLAvaan-class.md),
  [`vcov()`](https://inlavaan.haziqj.ml/reference/vcov.md),
  [`standardisedsolution()`](https://inlavaan.haziqj.ml/reference/standardisedsolution.md)

- Fit assessment and model comparison:
  [`fitmeasures()`](https://inlavaan.haziqj.ml/reference/fitMeasures.md),
  [`bfit_indices()`](https://inlavaan.haziqj.ml/reference/bfit_indices.md),
  [`compare()`](https://inlavaan.haziqj.ml/reference/compare.md),
  [`diagnostics()`](https://inlavaan.haziqj.ml/reference/diagnostics.md),
  [`timing()`](https://inlavaan.haziqj.ml/reference/timing.md)

- Posterior inference and simulation:
  [`predict()`](https://inlavaan.haziqj.ml/reference/predict.md),
  [`sampling()`](https://inlavaan.haziqj.ml/reference/sampling.md),
  [`simulate()`](https://inlavaan.haziqj.ml/reference/simulate.md)

- Visualisation:
  [`plot()`](https://inlavaan.haziqj.ml/reference/plot.md)

## Online vignettes

The [package website](https://inlavaan.haziqj.ml/) contains
comprehensive examples covering:

- Confirmatory Factor Analysis (CFA)

- Structural Equation Models (SEM)

- Latent Growth Curve Models

- Multigroup and Invariance Testing

- Mediation Analysis

## See also

Useful links:

- <https://inlavaan.haziqj.ml/>

- <https://github.com/haziqj/INLAvaan>

- Report bugs at <https://github.com/haziqj/INLAvaan/issues>

## Author

**Maintainer**: Haziq Jamil <haziq.jamil@gmail.com>
([ORCID](https://orcid.org/0000-0003-3298-1010)) \[copyright holder\]

Other contributors:

- Håvard Rue ([ORCID](https://orcid.org/0000-0002-0222-1881))
  (Statistical and computational methodology) \[contributor\]

- Alvin Bong (Initial site build) \[contributor\]
