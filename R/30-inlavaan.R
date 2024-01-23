model <- '
   # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
     dem65 =~ y5 + y6 + y7 + y8
   # regressions
     dem60 ~ ind60
     dem65 ~ ind60 + dem60
   # residual covariances
     y1 ~~ y5
     y2 ~~ y4 + y6
     y3 ~~ y7
     y4 ~~ y8
     y6 ~~ y8
'

inlavaan <- function(model = NULL,
                     data = NULL) {

  PT <- lavaanify(model, auto = TRUE)
  PT <- cbind(PT, as.data.frame(lavaan:::lav_lisrel(PT)))

  # dat_inla <-
  #   dat |>
  #   mutate(id = row_number()) |>
  #   pivot_longer(-id, names_to = "item", values_to = "y") |>
  #   mutate(k = row_number())

  # Formula for INLA
  # form = y ~ -1 + f(k, model = model)

  # inla(form, data = dat_inla,
  #      control.family = list(hyper = list(prec = list(initial = 15,
  #                                                     fixed = TRUE))),
  #      verbose = FALSE)

}; inlavaan(model)
