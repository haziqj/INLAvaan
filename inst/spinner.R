# Spinner in INLAvaan ----------------------------------------------------------
# devtools::install_github("haziqj/INLAvaan")
library(INLAvaan)
library(lavaan)

myModel <- '
  # latent variables
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  ind60 =~ x1 + x2 + x3

  # latent regressions
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
'
# About 10 seconds
fit <- isem(model = myModel, data = PoliticalDemocracy)

# Example using cli progressbar ------------------------------------------------
fun <- function() {
  # Setup
  options(cli.spinner = "boxBounce")
  options(cli.progress_show_after = 0)
  cli_progress_bar(
    name = "Laplace-ing through p dimensions",
    format = "{cli::pb_spin} {cli::pb_name} [{cli::pb_elapsed}]",
    clear = FALSE
  )

  # Simulating an inla call
  while (TRUE) {
    if (runif(1) < 0.01) break
    Sys.sleep(0.05)
    cli_progress_update()
  }
  cli_progress_update(force = TRUE)

  # Defaults
  options(cli.spinner = "dots")
  options(cli.progress_show_after = 2)
}
fun()
