# INLAvaan: Approximate Bayesian Latent Variable Analysis

Implements approximate Bayesian inference for Structural Equation Models
(SEM) using a custom adaptation of the Integrated Nested Laplace
Approximation (INLA). Provides a computationally efficient alternative
to Markov Chain Monte Carlo (MCMC) for Bayesian estimation, allowing
users to fit latent variable models using the 'lavaan' syntax.

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
