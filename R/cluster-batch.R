# Small matrices, one per cluster. The multilevel kernels (R/random-slope-
# kernels.R, R/twolevel-fiml-kernels.R) hold one small k x k matrix per
# cluster, or per cell of a cluster, as one row of a matrix stored by column,
# and work every row at once with the vector operations below, looping only
# over the k indices. rs_cluster_posterior() integrates out the cluster
# effects of all clusters, given A_j, p_j and the covariance of the effects.

## ----- Helpers ---------------------------------------------------------------

rs_at <- function(i, j, k) (j - 1L) * k + i

# Log-determinants and quadratic forms q' B^-1 q from the Cholesky factors.
# NULL when a matrix is not positive definite.
rs_batch_chol_quad <- function(b, q, k) {
  l <- matrix(0, nrow(b), k * k)
  z <- matrix(0, nrow(b), k)
  logdet <- numeric(nrow(b))
  for (j in seq_len(k)) {
    d <- b[, rs_at(j, j, k)]
    zj <- q[, j]
    for (m in seq_len(j - 1L)) {
      d <- d - l[, rs_at(j, m, k)]^2
      zj <- zj - l[, rs_at(j, m, k)] * z[, m]
    }
    if (any(!is.finite(d)) || any(d <= 0)) {
      return(NULL)
    }
    ljj <- sqrt(d)
    l[, rs_at(j, j, k)] <- ljj
    z[, j] <- zj / ljj
    logdet <- logdet + 2 * log(ljj)
    for (i in seq_len(k - j) + j) {
      s <- b[, rs_at(i, j, k)]
      for (m in seq_len(j - 1L)) {
        s <- s - l[, rs_at(i, m, k)] * l[, rs_at(j, m, k)]
      }
      l[, rs_at(i, j, k)] <- s / ljj
    }
  }
  list(logdet = logdet, quad = rowSums(z^2))
}

# Lower factors L with L L' = b of positive semidefinite matrices: Cholesky
# factors, with a zero column where a pivot vanishes, so that a singular
# matrix is factored too. NULL when a pivot is clearly negative.
rs_batch_chol_psd <- function(b, k, tol = 1e-10) {
  l <- matrix(0, nrow(b), k * k)
  scale <- pmax(
    apply(b[, rs_at(seq_len(k), seq_len(k), k), drop = FALSE], 1L, max),
    1
  )
  for (j in seq_len(k)) {
    d <- b[, rs_at(j, j, k)]
    for (m in seq_len(j - 1L)) {
      d <- d - l[, rs_at(j, m, k)]^2
    }
    if (any(!is.finite(d)) || any(d < -tol * scale)) {
      return(NULL)
    }
    zero <- d <= tol * scale
    ljj <- ifelse(zero, 0, sqrt(pmax(d, 0)))
    l[, rs_at(j, j, k)] <- ljj
    for (i in seq_len(k - j) + j) {
      s <- b[, rs_at(i, j, k)]
      for (m in seq_len(j - 1L)) {
        s <- s - l[, rs_at(i, m, k)] * l[, rs_at(j, m, k)]
      }
      l[, rs_at(i, j, k)] <- ifelse(zero, 0, s / ifelse(zero, 1, ljj))
    }
  }
  l
}

rs_batch_t <- function(x, k) {
  idx <- as.numeric(t(matrix(seq_len(k * k), k, k)))
  x[, idx, drop = FALSE]
}

# The same k x k matrix `cmat` times every row's matrix
rs_const_mm <- function(cmat, x, k) {
  idx <- matrix(seq_len(k * k), k, k)
  out <- matrix(0, nrow(x), k * k)
  for (j in seq_len(k)) {
    out[, idx[, j]] <- x[, idx[, j], drop = FALSE] %*% t(cmat)
  }
  out
}

# Inverses by Gauss-Jordan elimination without pivoting, which is stable for
# the symmetric positive definite B_j. Each step works a whole row of every
# matrix at once. NULL when a pivot is not positive.
rs_batch_inverse <- function(b, k) {
  idx <- matrix(seq_len(k * k), k, k)
  a <- b
  inv <- matrix(0, nrow(b), k * k)
  inv[, diag(idx)] <- 1
  for (j in seq_len(k)) {
    row_j <- idx[j, ]
    piv <- a[, idx[j, j]]
    if (any(!is.finite(piv)) || any(piv <= 0)) {
      return(NULL)
    }
    a[, row_j] <- a[, row_j, drop = FALSE] / piv
    inv[, row_j] <- inv[, row_j, drop = FALSE] / piv
    for (i in seq_len(k)[-j]) {
      row_i <- idx[i, ]
      f <- a[, idx[i, j]]
      a[, row_i] <- a[, row_i, drop = FALSE] - f * a[, row_j, drop = FALSE]
      inv[, row_i] <- inv[, row_i, drop = FALSE] -
        f * inv[, row_j, drop = FALSE]
    }
  }
  inv
}

rs_batch_mm <- function(x, y, k) {
  idx <- matrix(seq_len(k * k), k, k)
  out <- matrix(0, nrow(x), k * k)
  for (i in seq_len(k)) {
    xi <- x[, idx[i, ], drop = FALSE]
    for (j in seq_len(k)) {
      out[, idx[i, j]] <- rowSums(xi * y[, idx[, j], drop = FALSE])
    }
  }
  out
}

rs_batch_mv <- function(x, v, k) {
  idx <- matrix(seq_len(k * k), k, k)
  out <- matrix(0, nrow(x), k)
  for (i in seq_len(k)) {
    out[, i] <- rowSums(x[, idx[i, ], drop = FALSE] * v)
  }
  out
}

## ----- Cluster effects, integrated out ---------------------------------------

# The symmetric square root of Sigma_v, or NULL when Sigma_v is not positive
# semidefinite (lavaan's tolerance)
rs_sigma_v_half <- function(sigma_v) {
  ev <- eigen(sigma_v, symmetric = TRUE)
  if (ev$values[length(ev$values)] < -1e-12) {
    return(NULL)
  }
  ev$vectors %*%
    diag(sqrt(pmax(ev$values, 0)), length(ev$values)) %*%
    t(ev$vectors)
}

# The cluster effects of every cluster, integrated out: log|Z_j|, the quadratic
# form p~_j' S_j p~_j and, with `s0 = TRUE`, S_j = Sigma_v Z_j^-1 itself (one
# cluster per row). `status` is "ok", "undefined" when some Z_j has an
# eigenvalue at or below zero (lavaan's +Inf), or "fallback" when Sigma_v is
# not positive semidefinite and some A_j has no factor (it is a sum of
# squares, so this only happens through rounding). With A_j = L_j L_j', the
# matrices Z_j = I + L_j (L_j' Sigma_v) and B_j = I + L_j' Sigma_v L_j have
# the same eigenvalues, also when L_j has zero columns.
rs_cluster_posterior <- function(a, p_tilde, sigma_v, pv, s0 = FALSE) {
  if (anyNA(sigma_v)) {
    return(list(status = "undefined"))
  }
  diag_idx <- rs_at(seq_len(pv), seq_len(pv), pv)
  half <- rs_sigma_v_half(sigma_v)
  if (!is.null(half)) {
    hh <- kronecker(half, half)
    b <- a %*% hh
    b[, diag_idx] <- b[, diag_idx] + 1
    ch <- rs_batch_chol_quad(b, p_tilde %*% half, pv)
    if (is.null(ch)) {
      return(list(status = "undefined"))
    }
    out <- list(status = "ok", logdet = ch$logdet, quad = ch$quad)
    if (s0) {
      b_inv <- rs_batch_inverse(b, pv)
      if (is.null(b_inv)) {
        return(list(status = "undefined"))
      }
      out$s0 <- b_inv %*% hh
    }
    return(out)
  }
  l <- rs_batch_chol_psd(a, pv)
  if (is.null(l)) {
    return(list(status = "fallback"))
  }
  lt <- rs_batch_t(l, pv)
  tl <- rs_const_mm(sigma_v, l, pv)
  b <- rs_batch_mm(lt, tl, pv)
  b[, diag_idx] <- b[, diag_idx] + 1
  sp <- p_tilde %*% sigma_v
  ch <- rs_batch_chol_quad(b, rs_batch_mv(lt, sp, pv), pv)
  if (is.null(ch)) {
    return(list(status = "undefined"))
  }
  out <- list(
    status = "ok",
    logdet = ch$logdet,
    quad = rowSums(p_tilde * sp) - ch$quad
  )
  if (s0) {
    b_inv <- rs_batch_inverse(b, pv)
    if (is.null(b_inv)) {
      return(list(status = "undefined"))
    }
    corr <- rs_batch_mm(rs_batch_mm(tl, b_inv, pv), rs_batch_t(tl, pv), pv)
    out$s0 <- matrix(as.numeric(sigma_v), nrow(a), pv * pv, byrow = TRUE) -
      corr
  }
  out
}
