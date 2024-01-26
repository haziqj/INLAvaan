#' @export
inlavaan <- function(model = NULL, data = NULL) {

  my_message <- sample(cli_messages, size = 1)
  cli::cli_progress_bar(my_message, clear = FALSE)

  PT <- lavaan::lavaanify(model, auto = TRUE, meanstructure = TRUE)
  # PT <- lavaan::lavaanify(
  #   model = model,
  #   int.ov.free = TRUE,
  #   int.lv.free = FALSE,
  #   auto.fix.first = TRUE,
  #   auto.fix.single = TRUE,
  #   auto.var = TRUE,
  #   auto.cov.lv.x = TRUE,
  #   auto.cov.y = TRUE,
  #   auto.th = TRUE,
  #   auto.delta = TRUE,
  #   auto.efa = TRUE
  # )
  PT <- cbind(PT, as.data.frame(lavaan:::lav_lisrel(PT)))
  # PT$est <- PT$ustart
  PT <- as_tibble(PT)

  # Priors (default?)
  PT <- PT |>
    mutate(prior = case_when(
      mat == "nu" ~ "normal(0,32)",
      mat == "alpha" ~ "normal(0,10)",
      mat == "lambda" ~ "normal(0,10)",
      mat == "beta" ~ "normal(0,10)",
      mat == "theta" ~ "gamma(1,.5)[sd])",
      mat == "psi" ~ "gamma(1,.5)[sd]",
      mat == "rho" ~ "beta(1,1)",
      mat == "ibpsi" ~ "wishart(3,iden)",
      mat == "tau" ~ "normal(0,1.5)"
    ))

  cli::cli_progress_update(force = TRUE)

  # Figure out names of variables ----------------------------------------------
  ov_names <-
    PT |>
    filter(op == "=~") |>
    pull(rhs)

  lv_names <-
    PT |>
    filter(op == "=~") |>
    pull(lhs) |>
    unique()

  cv_names <-
    PT |>
    filter(op == "~~", lhs != rhs) |>
    pivot_longer(c(lhs, rhs)) |>
    pull(value)

  cli::cli_progress_update(force = TRUE)

  # Formula for the measurement model ------------------------------------------
  form_measure <- function(y) {
    res <- rep(NA, length(y))
    res[1] <- paste0("f(", y[1], ")")
    for (i in 2:length(res)) {
      res[i] <- paste0("f(", y[i], ", copy = '", y[1],
                       "', hyper = list(beta = list(fixed = FALSE)))")
    }
    res
  }

  measurement_model <-
    PT |>
    filter(op == "=~") |>
    group_by(lhs) |>
    mutate(copy = "",
           idx_name = rhs,
           formula = form_measure(rhs)) |>
    ungroup()

  # Formula for the structural model -------------------------------------------
  copy_info <-
    PT |>
    filter(op == "=~") |>
    slice(1, .by = lhs) |>
    select(lhs, copy = rhs)

  structural_model <-
    PT |>
    filter(op == "~") |>
    left_join(copy_info, by = join_by("rhs" == "lhs")) |>
    mutate(idx_name = paste0("b", row_number()),
           formula = paste0("f(", idx_name, ", copy = '", copy,
                            "', hyper = list(beta = list(fixed = FALSE)))"))

  # Handle correlations --------------------------------------------------------
  if (sum(PT$op == "~~" & PT$lhs != PT$rhs) > 0) {
    correlation_model <-
      PT |>
      filter(op == "~~", lhs != rhs) |>
      mutate(idx_name = map(row_number(), \(x) list(paste0("d", 2*x-1),
                                                    paste0("d", 2*x)))) |>
      unnest_longer(idx_name) |>
      group_by(id) |>
      mutate(formula = form_measure(idx_name),
             mat = case_when(
               mat == "theta" ~ "lambdaD",
               mat == "psi" ~ "betaE",
               TRUE ~ mat
               # mat == "beta" ~ "psiD"
             )) |>
      ungroup() |>
      mutate(copy = "")
    tmp <- correlation_model[correlation_model$op == "~~", ]$free
    tmp[seq(1, length(tmp), 2)] <- 0
    correlation_model[correlation_model$op == "~~", ]$free <- tmp
  } else {
    correlation_model <- data.frame()
  }

  # The INLA model table -------------------------------------------------------
  IT <-
    bind_rows(
      measurement_model,
      structural_model,
      correlation_model
    )
  # formula
  form <- as.formula(paste0("y ~ -1 + nu + ",
                            paste0(IT$formula, collapse = " + ")))

  cli::cli_progress_update(force = TRUE)

  # Prepare the data -----------------------------------------------------------
  n <- nrow(data)
  p <- length(ov_names)

  # prep the observed variables by stretching the data
  dat_inla <-
    data |>
    select(all_of(ov_names)) |>
    mutate(i = row_number()) |>
    pivot_longer(-i, names_to = "item", values_to = "y") |>
    mutate(k = row_number()) |>
    pivot_wider(names_from = item, values_from = y)

  # get indices for observed items
  idxi_list <-
    dat_inla |>
    mutate(across(all_of(ov_names), \(x) ifelse(!is.na(x), i, NA))) |>
    select(all_of(ov_names)) |>
    as.list()

  # get indices for latent variables
  idxl_list <-
    data |>
    select(all_of(ov_names)) |>
    mutate(i = row_number()) |>
    pivot_longer(-i, names_to = "item", values_to = "y") |>
    left_join(IT |> filter(op == "=~") |> select(latent = lhs, item = rhs),
              by = "item") |>
    mutate(k = row_number()) |>
    pivot_wider(names_from = latent, values_from = i) |>
    select(all_of(lv_names)) |>
    as.list()

  # build list of indices
  idx_all_list <- idxi_list

  tmp <- idxl_list[structural_model$lhs]
  names(tmp) <- structural_model$idx_name
  idx_all_list <- c(idx_all_list, tmp)
  rm(tmp)

  tmp <- c(idxi_list, idxl_list)[cv_names]
  names(tmp) <- correlation_model$idx_name
  idx_all_list <- c(idx_all_list, tmp)
  rm(tmp)

  # dummy index for intercepts
  nu <-
    data |>
    select(all_of(ov_names)) |>
    mutate(i = row_number()) |>
    pivot_longer(-i, names_to = "item", values_to = "y") |>
    pull(item) |>
    factor()

  # data list
  the_dat <- select(dat_inla, all_of(ov_names))
  # the_dat <- scale(the_dat, center = TRUE, scale = FALSE)

  datt <- c(
    list(y = the_dat,
         nu = nu),
    idx_all_list
  )

  cli::cli_progress_update(force = TRUE)

  # Fit INLA model -------------------------------------------------------------
  fit <- INLA::inla(form, data = datt, family = rep("gaussian", p))
  cli::cli_progress_done()

  out <-
    list(
      fit = fit,
      PT = PT,
      IT = IT,
      ov_names = ov_names,
      lv_names = lv_names,
      n = n,
      p = p
    )
  class(out) <- "inlavaan"
  out
}





# inlavaanxxx <- function(model = NULL,
#                      data = NULL) {
#
#   PT <- lavaan::lavaanify(model, auto = TRUE)
#   PT <- cbind(PT, as.data.frame(lavaan:::lav_lisrel(PT)))
#   PT$est <- PT$ustart
#
#   # Priors (default?)
#   PT <- PT |>
#     mutate(prior = case_when(
#       mat == "nu" ~ "normal(0,32)",
#       mat == "alpha" ~ "normal(0,10)",
#       mat == "lambda" ~ "normal(0,10)",
#       mat == "beta" ~ "normal(0,10)",
#       mat == "theta" ~ "gamma(1,.5)[sd])",
#       mat == "psi" ~ "gamma(1,.5)[sd]",
#       mat == "rho" ~ "beta(1,1)",
#       mat == "ibpsi" ~ "wishart(3,iden)",
#       mat == "tau" ~ "normal(0,1.5)"
#     ))
#
#   n <- nrow(data)
#   # Stretch the data
#   dat_inla <-
#     data |>
#     mutate(iii = row_number()) |>  # FIXME: making sure there isn't a column named iii!
#     pivot_longer(-iii, names_to = "item", values_to = "y") |>
#     mutate(kkk = row_number())  # FIXME: Same thing here with kkk.
#
#
#   # Create the INLA model
#   inla.rgeneric.sem.model <- function(
#     cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior",
#             "quit"),
#     theta = NULL
#   ) {
#
#     get_p <- function() {
#       nrow(PT_to_matrices(PT)$Theta)
#     }
#
#     theta_to_PT <- function() {
#
#       idx_free <- which(PT$free > 0)
#
#       par_vals <- theta
#       names(par_vals) <- PT$mat[idx_free]
#       par_vals[names(par_vals) %in% c("theta", "psi")] <- exp(
#         par_vals[names(par_vals) %in% c("theta", "psi")]
#       )
#       PT$est[idx_free] <- par_vals
#
#       PT
#     }
#
#     PT_to_matrices <- function(.PT) {
#       # Lambda
#       lambdas <- .PT[grep("lambda", .PT$mat), c("est", "row", "col")]
#       Lambda <- Matrix::sparseMatrix(i = lambdas$row,
#                                      j = lambdas$col,
#                                      x = lambdas$est)
#
#       # Theta
#       thetas <- .PT[grep("theta", .PT$mat), c("est", "row", "col")]
#       Theta <- Matrix::sparseMatrix(i = thetas$row,
#                                     j = thetas$col,
#                                     x = thetas$est)
#       Theta <- (Theta + Matrix::t(Theta)) / 2
#
#       # Psi
#       psis <- .PT[grep("psi", .PT$mat), c("est", "row", "col")]
#       Psi <- Matrix::sparseMatrix(i = psis$row,
#                                   j = psis$col,
#                                   x = psis$est)
#
#       list(
#         Lambda = Lambda,
#         Theta = Theta,
#         Psi = Psi
#       )
#     }
#
#     # Don't forget to write test functions!
#     parse_dist <- function(x, debug = FALSE) {
#       if (grepl("normal", x)) {
#         res <- strsplit(x, ",|\\(|\\)")[[1]]
#         mean <- as.numeric(res[2])
#         sd <- sqrt(as.numeric(res[3]))
#         if (isTRUE(debug)) {
#           return(list(dist = "normal", mean = mean, sd = sd))
#         } else {
#           return(function(theta) dnorm(theta, mean = mean, sd = sd, log = TRUE))
#         }
#       }
#
#       if (grepl("gamma", x)) {
#         res <- strsplit(x, ",|\\(|\\)")[[1]]
#         shape <- as.numeric(res[2])
#         rate <- as.numeric(res[3])
#         # FIXME: something to do with [sd] or not?
#         if (isTRUE(debug)) {
#           return(list(dist = "gamma", shape = shape, rate = rate))
#         } else {
#           return(function(theta) dgamma(theta, shape = shape, rate = rate,
#                                         log = TRUE))
#         }
#
#         # FIXME: Also, since we parameterise as var = log(theta), need to add Jacobian
#       }
#
#       if (grepl("beta", x)) {
#         res <- strsplit(x, ",|\\(|\\)")[[1]]
#         shape1 <- as.numeric(res[2])
#         shape2 <- as.numeric(res[3])
#         # FIXME: something to do with [sd] or not?
#         if (isTRUE(debug)) {
#           return(list(dist = "beta", shape1 = shape1, shape2 = shape2))
#         } else {
#           return(function(theta) dgamma(theta, shape1 = shape1, shape2 = shape2,
#                                         log = TRUE))
#         }
#       }
#     }
#
#     Q <- function() {
#       new_PT <- theta_to_PT()
#       mat <- PT_to_matrices(new_PT)
#       Lambda <- mat$Lambda
#       Theta <- mat$Theta
#       Psi <- mat$Psi
#       Sigma <- Lambda %*% Psi %*% Matrix::t(Lambda) + Theta
#       Q <- solve(Sigma)
#       p <- get_p()
#
#       Qlist <- replicate(n, Q, simplify = FALSE)
#       Q <- bdiag(Qlist)
#       inla.as.sparse(Q)
#       # rows <- rep(which(Q != 0, arr.ind = TRUE)[,1], n)
#       # cols <- rep(which(Q != 0, arr.ind = TRUE)[,2], n)
#       # vals <- rep(Q[Q != 0], n)
#       #
#       # print(Sigma)
#       #
#       #
#       # # Adjust indices for block diagonal structure
#       # for(i in 1:(n-1)) {
#       #   rows[((i*p*p)+1):((i+1)*p*p)] <- rows[((i*p*p)+1):((i+1)*p*p)] + i*p
#       #   cols[((i*p*p)+1):((i+1)*p*p)] <- cols[((i*p*p)+1):((i+1)*p*p)] + i*p
#       # }
#       #
#       # # Create the block diagonal sparse matrix
#       # sparseMatrix(i = rows, j = cols, x = vals, dims = c(n*p, n*p))
#     }
#
#     mu <- function() { numeric(0) }  # FIXME: When meanstructure = TRUE then this should be a vector of length p
#
#     log.norm.const <- function() { numeric(0) }
#
#     log.prior <- function() {
#       require("tidyverse")
#       theta_to_PT() |>
#         as_tibble() |>
#         select(est, prior) |>
#         mutate(prior = map(prior, parse_dist)) |>
#         mutate(vals = purrr::map2_dbl(prior, est, ~ .x(.y))) |>
#         pull(est) |>
#         sum()
#     }
#
#     initial <- function() {
#       nparam <- sum(PT$free > 0)
#       rnorm(nparam)
#     }
#
#     graph <- function() {
#       p <- get_p()
#       Qlist <- replicate(n, matrix(1, nrow = p, ncol = p), simplify = FALSE)
#       Q <- bdiag(Qlist)
#       inla.as.sparse(Q)
#     }
#
#     quit <- function() { return(invisible()) }
#
#     if (!length(theta)) theta = initial()
#     val = do.call(match.arg(cmd), args = list())
#     return (val)
#   }
#
#   inlavaan_model <- inla.rgeneric.define(inla.rgeneric.sem.model,
#                                          PT = PT,
#                                          n = nrow(data))
#
#
#
#
#   inla(y ~ -1 + f(kkk, model = inlavaan_model),
#        data = dat_inla,
#        control.family = list(hyper = list(prec = list(initial = 15,
#                                                       fixed = TRUE))),
#        verbose = FALSE)
#
# }
#
