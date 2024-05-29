cli_messages <- c(
  "Laplace-ing through p dimensions",
  "Summoning Bayesian spirits",
  "Casting statistical spells",
  "Conjuring INLA magic",
  "Channeling Laplace's wizardry",
  "Harnessing the power of priors",
  "Diving into the probability pool",
  "Navigating the seas of stochasticity"
)

theta_to_rho <- function(x) {
  u <- 1 / (1 + exp(-x))
  rho <- 2 * u - 1
  rho
}

rho_to_theta <- function(x) {
  x[x > 1] <- 1
  x[x < -1] <- -1
  u <- (x + 1) / 2
  theta <- log(u / (1 - u))
  theta
}

safe_solve <- function(x) {
  try_chol <- try(chol(x), silent = TRUE)
  if (any(class(try_chol) %in% "try-error")) {
    xm <- forceSymmetric(Matrix(x))
    return(solve(xm))
  } else {
    return(chol2inv(try_chol))
  }
}

# 1. From theta to PT
# 2. From PT to Sigma and hence Q
# 3. From PT to log priors

# get_p <- function(.PT) {
#   nrow(PT_to_matrices(.PT = .PT)$Theta)
# }
#
# theta_to_PT <- function(.PT) {
#
#   PT <- .PT
#   idx_free <- which(PT$free > 0)
# print(theta)
#   par_vals <- theta
#   names(par_vals) <- PT$mat[idx_free]
#   par_vals[names(par_vals) %in% c("theta", "psi")] <- exp(
#     par_vals[names(par_vals) %in% c("theta", "psi")]
#   )
#   PT$est[idx_free] <- par_vals
#
#   PT
# }
#
# PT_to_matrices <- function(.PT) {
#   # Lambda
#   lambdas <- .PT[grep("lambda", .PT$mat), c("est", "row", "col")]
#   Lambda <- Matrix::sparseMatrix(i = lambdas$row,
#                                  j = lambdas$col,
#                                  x = lambdas$est)
#
#   # Theta
#   thetas <- .PT[grep("theta", .PT$mat), c("est", "row", "col")]
#   Theta <- Matrix::sparseMatrix(i = thetas$row,
#                                 j = thetas$col,
#                                 x = thetas$est)
#   Thetat <- Matrix::sparseMatrix(j = thetas$row,
#                                  i = thetas$col,
#                                  x = thetas$est)
#   Theta <- (Theta + Thetat) / 2
#
#   # Psi
#   psis <- .PT[grep("psi", .PT$mat), c("est", "row", "col")]
#   Psi <- Matrix::sparseMatrix(i = psis$row,
#                               j = psis$col,
#                               x = psis$est)
#
#   list(
#     Lambda = Lambda,
#     Theta = Theta,
#     Psi = Psi
#   )
# }
#
# # Don't forget to write test functions!
# parse_dist <- function(x, debug = FALSE) {
#   if (grepl("normal", x)) {
#     res <- strsplit(x, ",|\\(|\\)")[[1]]
#     mean <- as.numeric(res[2])
#     sd <- sqrt(as.numeric(res[3]))
#     if (isTRUE(debug)) {
#       return(list(dist = "normal", mean = mean, sd = sd))
#     } else {
#       return(function(theta) dnorm(theta, mean = mean, sd = sd, log = TRUE))
#     }
#   }
#
#   if (grepl("gamma", x)) {
#     res <- strsplit(x, ",|\\(|\\)")[[1]]
#     shape <- as.numeric(res[2])
#     rate <- as.numeric(res[3])
#     # FIXME: something to do with [sd] or not?
#     if (isTRUE(debug)) {
#       return(list(dist = "gamma", shape = shape, rate = rate))
#     } else {
#       return(function(theta) dgamma(theta, shape = shape, rate = rate,
#                                     log = TRUE))
#     }
#
#     # FIXME: Also, since we parameterise as var = log(theta), need to add Jacobian
#   }
#
#   if (grepl("beta", x)) {
#     res <- strsplit(x, ",|\\(|\\)")[[1]]
#     shape1 <- as.numeric(res[2])
#     shape2 <- as.numeric(res[3])
#     # FIXME: something to do with [sd] or not?
#     if (isTRUE(debug)) {
#       return(list(dist = "beta", shape1 = shape1, shape2 = shape2))
#     } else {
#       return(function(theta) dgamma(theta, shape1 = shape1, shape2 = shape2,
#                                     log = TRUE))
#     }
#   }
# }

# parse_dist("normal(0,10)", TRUE)
# parse_dist("gamma(1,.5)[sd]", TRUE)
# parse_dist("beta(1,1)", TRUE)
