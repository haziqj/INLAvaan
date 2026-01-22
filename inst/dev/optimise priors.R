library(INLAvaan)
library(blavaan)
utils::data("PoliticalDemocracy", package = "lavaan")

optimise_prior_gamma <- function(x) {
  a1 <- x[1]
  b1 <- x[2]
  a2 <- x[3]
  b2 <- x[4]
  a3 <- x[5]
  b3 <- x[6]

  model <- "
  # Latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
     dem65 =~ y5 + y6 + y7 + y8

  # Latent regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # Residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

  # Custom priors on latent variances
    ind60 ~~ prior('gamma(a1,b1)')*ind60
    dem60 ~~ prior('gamma(a2,b2)')*dem60
    dem65 ~~ prior('gamma(a3,b3)')*dem65
  "
  model <- gsub(
    "a1",
    as.character(a1),
    gsub(
      "b1",
      as.character(b1),
      gsub(
        "a2",
        as.character(a2),
        gsub(
          "b2",
          as.character(b2),
          gsub("a3", as.character(a3), gsub("b3", as.character(b3), model))
        )
      )
    )
  )

  fit <- asem(model, PoliticalDemocracy, verbose = FALSE)
  suppressWarnings(suppressMessages(
    fit_blav <- bsem(
      model,
      PoliticalDemocracy,
      sample = 1000,
      n.chains = 1,
      test = "none",
      seed = 1234
      # bcontrol = list(
      #   refresh = 0, # no iteration progress
      #   verbose = FALSE, # less chatter
      #   open_progress = FALSE
      # )
    )
  ))
  res <- INLAvaan:::compare_mcmc(fit_blav, INLAvaan = fit)
  sum(res$metrics_df$JS_percent)
}

start <- rep(1, 6)
optimise_prior_gamma(start)

opt <- optim(
  start,
  optimise_prior_gamma,
  method = "L-BFGS-B",
  lower = rep(0.01, 6),
  upper = rep(10, 6),
  hessian = FALSE
)
