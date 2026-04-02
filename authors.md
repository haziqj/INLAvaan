# Authors and Citation

## Authors

- **[Haziq Jamil](https://haziqj.ml/)**. Author, maintainer, copyright
  holder. [](https://orcid.org/0000-0003-3298-1010)

- **[Håvard
  Rue](https://www.kaust.edu.sa/en/study/faculty/haavard-rue)**.
  Contributor. [](https://orcid.org/0000-0002-0222-1881)  
  Statistical and computational methodology

- **[Alvin Bong](https://alvinbjl.github.io)**. Contributor.  
  Initial site build

## Citation

Source:
[`inst/CITATION`](https://github.com/haziqj/INLAvaan/blob/main/inst/CITATION)

Jamil H, Rue H (2026). “Approximate Bayesian inference for structural
equation models using integrated nested Laplace approximations.”
[doi:10.48550/arXiv.2603.25690](https://doi.org/10.48550/arXiv.2603.25690),
2603.25690.

    @Misc{jamil2026approximate,
      title = {Approximate Bayesian inference for structural equation models using integrated nested Laplace approximations},
      author = {Haziq Jamil and Håvard Rue},
      year = {2026},
      number = {2603.25690 [stat.ME]},
      eprint = {2603.25690},
      primaryclass = {stat.ME},
      publisher = {arXiv},
      doi = {10.48550/arXiv.2603.25690},
      abstract = {Markov chain Monte Carlo (MCMC) methods remain the mainstay of Bayesian estimation of structural equation models (SEM); however they often incur a high computational cost. We present a bespoke approximate Bayesian approach to SEM, drawing on ideas from the integrated nested Laplace approximation (INLA; Rue et al., 2009, J. R. Stat. Soc. Series B Stat. Methodol.) framework. We implement a simplified Laplace approximation that efficiently profiles the posterior density in each parameter direction while correcting for asymmetry, allowing for parametric skew-normal estimation of the marginals. Furthermore, we apply a variational Bayes correction to shift the marginal locations, thereby better capturing the posterior mass. Essential quantities, including factor scores and model-fit indices, are obtained via an adjusted Gaussian copula sampling scheme. For normal-theory SEM, this approach offers a highly accurate alternative to sampling-based inference, achieving near-'maximum likelihood' speeds while retaining the precision of full Bayesian inference.},
      archiveprefix = {arXiv},
      copyright = {Creative Commons Attribution Non Commercial Share Alike 4.0 International},
    }

Jamil H, Rue H (2026). “Implementation and workflows for INLA-based
approximate Bayesian structural equation modelling.”
[doi:10.48550/arXiv.2604.00671](https://doi.org/10.48550/arXiv.2604.00671),
2604.00671.

    @Misc{jamil2026implementation,
      title = {Implementation and workflows for INLA-based approximate Bayesian structural equation modelling},
      author = {Haziq Jamil and Håvard Rue},
      year = {2026},
      number = {2604.00671 [stat.CO]},
      eprint = {2604.00671},
      primaryclass = {stat.CO},
      publisher = {arXiv},
      doi = {10.48550/arXiv.2604.00671},
      abstract = {Bayesian structural equation modelling (BSEM) offers many advantages such as principled uncertainty quantification, small-sample regularisation, and flexible model specification. However, the Markov chain Monte Carlo (MCMC) methods on which it relies are computationally prohibitive for the iterative cycle of specification, criticism, and refinement that careful psychometric practice demands. We present INLAvaan, an R package for fast, approximate Bayesian SEM built around the Integrated Nested Laplace Approximation (INLA) framework for structural equation models developed by Jamil & Rue (2026, arXiv:2603.25690 [stat.ME]). This paper serves as a companion manuscript that describes the architectural decisions and computational strategies underlying the package. Two substantive applications -- a 256-parameter bifactor circumplex model and a multilevel mediation model with full-information missing-data handling -- demonstrate the approach on specifications where MCMC would require hours of run time and careful convergence work. In constrast, INLAvaan delivers calibrated posterior summaries in seconds.},
      archiveprefix = {arXiv},
      copyright = {Creative Commons Attribution Non Commercial Share Alike 4.0 International},
      langid = {english},
    }
