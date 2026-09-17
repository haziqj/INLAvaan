# Node set for the VB mean correction by a deterministic Gauss-Hermite rule,
# the alternative to the scrambled Sobol cloud of vb_nodes(). Returns the
# node offsets (one row per node, the centre first) and their weights.
#
# In whitened coordinates z ~ N(0, I) every odd moment and every cross moment
# E[z_r z_s] vanishes, so the Gaussian expectation of a polynomial of degree
# three is a sum of one-dimensional expectations, one per axis. Each axis is
# integrated by the three-point Gauss-Hermite rule, with nodes at 0 and
# +/- sqrt(3) and weights 2/3 and 1/6. The axes share the centre, which leaves
# 2m + 1 nodes: the centre, then +sqrt(3) and -sqrt(3) times each axis. The
# weights sum to one, and the centre weight 1 - m/3 is negative once m > 3.
# This is the unscented transform with m + kappa = 3.
#
# For the VB score, the expected gradient, the rule is exact whenever the
# log-posterior is a quartic polynomial in whitened coordinates. It misses
# mixed terms such as z_r^2 z_s^2, which enter at higher order in n. Any
# whitener gives the same exactness, but the axes set the size of the mixed
# terms, and in tests the principal axes of Sigma kept them smaller than a
# Cholesky factor. The rule is symmetric, so eigenvector signs do not matter.
vb_nodes_gauss_hermite <- function(Sigma) {
  m <- ncol(Sigma)
  e <- eigen(Sigma, symmetric = TRUE)
  axes <- sweep(e$vectors, 2, sqrt(pmax(e$values, 0)), "*")
  list(
    nodes = rbind(0, sqrt(3) * t(axes), -sqrt(3) * t(axes)),
    weights = c(1 - m / 3, rep(1 / 6, 2 * m))
  )
}
