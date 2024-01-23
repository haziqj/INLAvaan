
inlavaan <- function(model = NULL,
                     data = NULL) {

  PT <- lavaan::lavaanify(model, auto = TRUE)
  PT <- cbind(PT, as.data.frame(lavaan:::lav_lisrel(PT)))
  PT$est <- PT$ustart

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

  print(PT)

  n <- nrow(data)
  # Stretch the data
  dat_inla <-
    data |>
    mutate(iii = row_number()) |>  # FIXME: making sure there isn't a column named iii!
    pivot_longer(-iii, names_to = "item", values_to = "y") |>
    mutate(kkk = row_number())  # FIXME: Same thing here with kkk.


  # Create the INLA model
  inla.rgeneric.sem.model <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior",
            "quit"),
    theta = NULL
  ) {

    get_p <- function() {
      nrow(PT_to_matrices(PT)$Theta)
    }

    theta_to_PT <- function() {

      idx_free <- which(PT$free > 0)

      par_vals <- theta
      names(par_vals) <- PT$mat[idx_free]
      par_vals[names(par_vals) %in% c("theta", "psi")] <- exp(
        par_vals[names(par_vals) %in% c("theta", "psi")]
      )
      PT$est[idx_free] <- par_vals

      PT
    }

    PT_to_matrices <- function(.PT) {
      # Lambda
      lambdas <- .PT[grep("lambda", .PT$mat), c("est", "row", "col")]
      Lambda <- Matrix::sparseMatrix(i = lambdas$row,
                                     j = lambdas$col,
                                     x = lambdas$est)

      # Theta
      thetas <- .PT[grep("theta", .PT$mat), c("est", "row", "col")]
      Theta <- Matrix::sparseMatrix(i = thetas$row,
                                    j = thetas$col,
                                    x = thetas$est)
      Theta <- (Theta + Matrix::t(Theta)) / 2

      # Psi
      psis <- .PT[grep("psi", .PT$mat), c("est", "row", "col")]
      Psi <- Matrix::sparseMatrix(i = psis$row,
                                  j = psis$col,
                                  x = psis$est)

      list(
        Lambda = Lambda,
        Theta = Theta,
        Psi = Psi
      )
    }

    # Don't forget to write test functions!
    parse_dist <- function(x, debug = FALSE) {
      if (grepl("normal", x)) {
        res <- strsplit(x, ",|\\(|\\)")[[1]]
        mean <- as.numeric(res[2])
        sd <- sqrt(as.numeric(res[3]))
        if (isTRUE(debug)) {
          return(list(dist = "normal", mean = mean, sd = sd))
        } else {
          return(function(theta) dnorm(theta, mean = mean, sd = sd, log = TRUE))
        }
      }

      if (grepl("gamma", x)) {
        res <- strsplit(x, ",|\\(|\\)")[[1]]
        shape <- as.numeric(res[2])
        rate <- as.numeric(res[3])
        # FIXME: something to do with [sd] or not?
        if (isTRUE(debug)) {
          return(list(dist = "gamma", shape = shape, rate = rate))
        } else {
          return(function(theta) dgamma(theta, shape = shape, rate = rate,
                                        log = TRUE))
        }

        # FIXME: Also, since we parameterise as var = log(theta), need to add Jacobian
      }

      if (grepl("beta", x)) {
        res <- strsplit(x, ",|\\(|\\)")[[1]]
        shape1 <- as.numeric(res[2])
        shape2 <- as.numeric(res[3])
        # FIXME: something to do with [sd] or not?
        if (isTRUE(debug)) {
          return(list(dist = "beta", shape1 = shape1, shape2 = shape2))
        } else {
          return(function(theta) dgamma(theta, shape1 = shape1, shape2 = shape2,
                                        log = TRUE))
        }
      }
    }

    Q <- function() {
      new_PT <- theta_to_PT()
      mat <- PT_to_matrices(new_PT)
      Lambda <- mat$Lambda
      Theta <- mat$Theta
      Psi <- mat$Psi
      Sigma <- Lambda %*% Psi %*% Matrix::t(Lambda) + Theta
      Q <- solve(Sigma)
      p <- get_p()

      Qlist <- replicate(n, Q, simplify = FALSE)
      Q <- bdiag(Qlist)
      inla.as.sparse(Q)
      # rows <- rep(which(Q != 0, arr.ind = TRUE)[,1], n)
      # cols <- rep(which(Q != 0, arr.ind = TRUE)[,2], n)
      # vals <- rep(Q[Q != 0], n)
      #
      # print(Sigma)
      #
      #
      # # Adjust indices for block diagonal structure
      # for(i in 1:(n-1)) {
      #   rows[((i*p*p)+1):((i+1)*p*p)] <- rows[((i*p*p)+1):((i+1)*p*p)] + i*p
      #   cols[((i*p*p)+1):((i+1)*p*p)] <- cols[((i*p*p)+1):((i+1)*p*p)] + i*p
      # }
      #
      # # Create the block diagonal sparse matrix
      # sparseMatrix(i = rows, j = cols, x = vals, dims = c(n*p, n*p))
    }

    mu <- function() { numeric(0) }  # FIXME: When meanstructure = TRUE then this should be a vector of length p

    log.norm.const <- function() { numeric(0) }

    log.prior <- function() {
      require("tidyverse")
      theta_to_PT() |>
        as_tibble() |>
        select(est, prior) |>
        mutate(prior = map(prior, parse_dist)) |>
        mutate(vals = purrr::map2_dbl(prior, est, ~ .x(.y))) |>
        pull(est) |>
        sum()
    }

    initial <- function() {
      nparam <- sum(PT$free > 0)
      rnorm(nparam)
    }

    graph <- function() {
      p <- get_p()
      Qlist <- replicate(n, matrix(1, nrow = p, ncol = p), simplify = FALSE)
      Q <- bdiag(Qlist)
      inla.as.sparse(Q)
    }

    quit <- function() { return(invisible()) }

    if (!length(theta)) theta = initial()
    val = do.call(match.arg(cmd), args = list())
    return (val)
  }

  inlavaan_model <- inla.rgeneric.define(inla.rgeneric.sem.model,
                                         PT = PT,
                                         n = nrow(data))




  inla(y ~ -1 + f(kkk, model = inlavaan_model),
       data = dat_inla,
       control.family = list(hyper = list(prec = list(initial = 15,
                                                      fixed = TRUE))),
       verbose = FALSE)

}
