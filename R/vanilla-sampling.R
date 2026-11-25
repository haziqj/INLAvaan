# Draws from the approximate joint posterior by a Gaussian copula: the marginals
# in `sn` (a matrix with columns `xi`, `omega` and `alpha` and a row per
# parameter) coupled through the correlation matrix `corr`, by default that of
# the Laplace covariance `cov`. Each draw takes z ~ N(0, corr), and maps each
# normal score to the marginal's quantile, theta_j = Q_j(Phi(z_j)). A marginal
# with alpha = 0 is Gaussian and the map is linear, so with every alpha at 0
# the draws are those of the joint Gaussian. Otherwise the map is
# sn_score_map(), a spline of the exact skew-normal quantile. Returns a matrix
# with a row per draw and a column per parameter.
sn_draws <- function(sn, cov, nsamp, corr = stats::cov2cor(cov)) {
  m <- nrow(sn)
  U <- chol(corr)
  z <- matrix(stats::rnorm(nsamp * m), nsamp, m) %*% U
  theta <- matrix(0, nsamp, m, dimnames = list(NULL, rownames(sn)))
  for (j in seq_len(m)) {
    if (sn[j, "alpha"] == 0) {
      theta[, j] <- sn[j, "xi"] + sn[j, "omega"] * z[, j]
    } else {
      map <- sn_score_map(sn[j, "xi"], sn[j, "omega"], sn[j, "alpha"])
      theta[, j] <- map(z[, j])
    }
  }
  theta
}
