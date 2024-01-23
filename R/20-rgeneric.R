inla.rgeneric.cfa.model <- function(
    cmd = c("graph", "Q", "mu", "initial", "log.norm.const", "log.prior",
            "quit"),
    theta = NULL
) {

  interpret.theta <- function() {

  }

  Q <- function() {
    params <- interpret.theta()
    Lambda <- matrix(params$lambda, ncol = 1)
    Theta <- diag(params$theta)
    psi <- params$psi
    Sigma <- psi * Lambda %*% t(Lambda) + Theta
    Q <- solve(Sigma)

    # Qlist <- replicate(n, Q, simplify = FALSE)
    # Q <- bdiag(Qlist)
    # inla.as.sparse(Q)
    rows <- rep(which(Q != 0, arr.ind = TRUE)[,1], n)
    cols <- rep(which(Q != 0, arr.ind = TRUE)[,2], n)
    vals <- rep(Q[Q != 0], n)

    # Adjust indices for block diagonal structure
    for(i in 1:(n-1)) {
      rows[((i*p*p)+1):((i+1)*p*p)] <- rows[((i*p*p)+1):((i+1)*p*p)] + i*p
      cols[((i*p*p)+1):((i+1)*p*p)] <- cols[((i*p*p)+1):((i+1)*p*p)] + i*p
    }

    # Create the block diagonal sparse matrix
    sparseMatrix(i = rows, j = cols, x = vals, dims = c(n*p, n*p))
  }



  # mu <- function() { numeric(0) }
  #
  # log.norm.const = function() { numeric(0) }
  #
  # log.prior = function() {
  #   # params <- interpret.theta()
  #   sum(dnorm(theta[1:2], mean = 0, sd = 1, log = TRUE))
  # }
  #
  # initial = function() { c(0.5, 1.5) }
  #
  # graph <- function() {
  #   Qlist <- replicate(n, matrix(1, nrow = p, ncol = p), simplify = FALSE)
  #   Q <- bdiag(Qlist)
  #   inla.as.sparse(Q)
  # }
  #
  # quit = function() { return(invisible()) }
  #
  # # if (!length(theta)) theta = initial()
  # val = do.call(match.arg(cmd), args = list())
  # return (val)
}

# inlavaan_model <- inla.rgeneric.define(inla.rgeneric.cfa.model, model = model)





