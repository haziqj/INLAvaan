# Changelog

## INLAvaan 0.2.1

- Support for lavaan 0.6-21.
- Implemented variational Bayes mean correction for posterior marginals.

## INLAvaan 0.2-0

- INLAvaan has been rewritten from the ground up specifically for SEM
  models. The new version does not call R-INLA directly, but instead
  uses the core approximation ideas to fit SEM models more efficiently.
- Features are restricted to **normal likelihoods only** and continuous
  observations for now.
- Support for most models that lavaan/blavaan can fit, including CFA,
  SEM, and growth curve models.
- Support for multigroup analysis.
- Added PPP and DIC model fit indices.
- Added prior specification for all model parameters.
- Added support for fixed values and parameter constraints.
- Initial CRAN submission.

## INLAvaan 0.1-0

- Used `rgeneric` functionality of R-INLA to implement a basic SEM
  framework.
