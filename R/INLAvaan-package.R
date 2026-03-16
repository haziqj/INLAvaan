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
#' @import methods
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_inform
#' @importFrom cli col_grey
#' @importFrom cli symbol
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_done
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_update
#' @importFrom cli cli_warn
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices recordPlot
#' @importFrom graphics abline
#' @importFrom graphics barplot
#' @importFrom graphics grid
#' @importFrom graphics layout
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics polygon
#' @importFrom stats aggregate
#' @importFrom stats approx
#' @importFrom stats cov2cor
#' @importFrom stats dbeta
#' @importFrom stats density
#' @importFrom stats dgamma
#' @importFrom stats dnorm
#' @importFrom stats median
#' @importFrom stats nlminb
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats reshape
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom stats splinefun
#' @importFrom stats uniroot
#' @importFrom utils capture.output
#' @importFrom utils getFromNamespace
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils tail
## usethis namespace: end
NULL
