# 1. From theta to PT
# 2. From PT to Sigma and hence Q
# 3. From PT to log priors

theta_to_PT <- function() {
  idx_free <- which(PT$free > 0)
  PT$est <- PT$ustart
  PT$est[idx_free] <- theta
  PT
}

PT_to_matrices <- function() {
  .PT <- theta_to_PT()

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
  Theta <- (Theta + t(Theta)) / 2

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
parse_dist <- function(x) {
  if (grepl("normal", x)) {
    res <- strsplit(x, ",|\\(|\\)")[[1]]
    mean <- as.numeric(res[2])
    sd <- sqrt(as.numeric(res[3]))
    return(function(theta) dnorm(theta, mean = mean, sd = sd, log = TRUE))
  }
}

