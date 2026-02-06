#' @keywords internal
#' @section Main features:
#' * [acfa()]: Approximate Confirmatory Factor Analysis.
#' * [asem()]: Approximate Structural Equation Modelling.
#' * [agrowth()]: Approximate Latent Growth Curve models.
#'
#' @section Model specifications:
#' Supports advanced 'lavaan' syntax features, including:
#' * Equality constraints
#' * Defined parameters (e.g., `:=` operator for indirect effects)
#' * Flexible prior specifications
#'
#' @section Online vignettes:
#' The [package website](https://inlavaan.haziqj.ml/) contains comprehensive examples covering:
#' * Confirmatory Factor Analysis (CFA)
#' * Structural Equation Models (SEM)
#' * Latent Growth Curve Models
#' * Multigroup and Invariance Testing
#' * Mediation Analysis
#'
#' @aliases INLAvaan
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @import methods
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert_info
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom modeest mfv1
#' @importFrom qrng sobol
#' @importFrom statmod gauss.quad
#' @importFrom stats approx
#' @importFrom stats cov2cor
#' @importFrom stats dbeta
#' @importFrom stats density
#' @importFrom stats dgamma
#' @importFrom stats dnorm
#' @importFrom stats dnorm dgamma
#' @importFrom stats median
#' @importFrom stats nlminb
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats reshape
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom stats splinefun
#' @importFrom stats uniroot
#' @importFrom tidyr %>% pivot_longer
#' @importFrom utils capture.output
#' @importFrom utils getFromNamespace
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils tail
## usethis namespace: end
NULL
