sobol_owen <- function(n, d) {
  N <- nrow(SobolOwen)
  D <- ncol(SobolOwen)

  # If within stored table dimensions, use it directly
  if (n <= N && d <= D) {
    return(SobolOwen[seq_len(n), seq_len(d), drop = FALSE])
  }

  # Otherwise, fall back to qrng if available
  # nocov start
  if (requireNamespace("qrng", quietly = TRUE)) {
    return(qrng::sobol(n = n, d = d, randomize = "Owen"))
  }

  # No fallback available — error with guidance
  cli_abort(c(
    "Requested Sobol sequence ({n} x {d}) exceeds stored table ({N} x {D}).",
    "i" = "Install the {.pkg qrng} package to generate larger sequences on the fly."
  ))
  # nocov end
}

# Node set for the VB mean correction: `n` scrambled Sobol points mapped
# through the Gaussian quantile function and rotated by `L`, with the mode
# itself prepended to lock the rule at the expansion point.
#
# The nodes are centred before the rotation. The expectation being approximated
# is over a mean-zero Gaussian, so the rule must match that first moment
# exactly. Scrambled Sobol points have a mean near zero but not equal to it,
# and for a quadratic log-posterior the variational optimum solves
# `L %*% delta = -colMeans(zs)` -- so any leftover node mean enters the reported
# shift directly, whatever the shape of the posterior. Centring makes
# `colMeans(zs)` exactly zero: the centred block has zero column sums and the
# prepended row is zero.
vb_nodes <- function(n, L) {
  z_std <- qnorm(sobol_owen(n = n, d = ncol(L)))
  z_std <- sweep(z_std, 2, colMeans(z_std))
  rbind(0, z_std %*% t(L))
}
