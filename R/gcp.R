# GCP: Graphical Correlation Parametrisation for correlation matrices
#
# Implements the graphpcor GCP approach (Freni-Sterrantino et al., 2025)
# standalone within INLAvaan — no external dependencies on graphpcor,
# INLAtools, or INLA.
#
# The GCP guarantees a positive definite correlation matrix for any
# unconstrained parameter vector theta, by working through the Cholesky
# factor of a precision matrix.

# --- Core GCP functions -------------------------------------------------------

#' Fill in Cholesky elements determined by the sparsity structure.
#'
#' Given a lower-triangular matrix L with free parameters at graph-edge
#' positions and zeros elsewhere (below diagonal), compute the "fill-in"
#' entries that arise from the Cholesky recursion.
#'
#' @param L Lower-triangular matrix with diagonal d0 and free params placed.
#' @return L with fill-in elements computed.
#' @keywords internal
gcp_fill_chol <- function(L) {
  p <- nrow(L)
  if (p <= 2L) return(L)

  # Identify fill-in positions: zero in L but non-zero in chol(LL')
  # Use the Cholesky of the graph Laplacian + I to find the pattern
  graph_nz <- (L != 0)
  # Compute actual Cholesky to discover fill-in pattern
  Q_test <- L %*% t(L)
  L_full <- t(chol(Q_test))
  fill_pos <- which(L == 0 & L_full != 0 & lower.tri(L))

  if (length(fill_pos) == 0L) return(L)

  # Sort by column then row (required for forward substitution)
  rc <- arrayInd(fill_pos, .dim = c(p, p))
  ord <- order(rc[, 2], rc[, 1])
  fill_pos <- fill_pos[ord]
  rc <- rc[ord, , drop = FALSE]

  for (v in seq_len(nrow(rc))) {
    i <- rc[v, 1]
    j <- rc[v, 2]
    if (j == 1L) {
      L[i, j] <- 0
    } else {
      k <- seq_len(j - 1L)
      L[i, j] <- -sum(L[i, k] * L[j, k]) / L[j, j]
    }
  }
  L
}

#' Map unconstrained parameters theta to a correlation matrix via GCP.
#'
#' @param theta Numeric vector of free parameters (length = number of edges).
#' @param p Integer, dimension of the correlation matrix.
#' @param iLtheta Integer vector, positions in the lower triangle of L where
#'   theta values are placed (vectorised column-major indices).
#' @param d0 Numeric vector of length p, diagonal of L^(0). Default: p:1.
#' @return A p x p correlation matrix C.
#' @keywords internal
gcp_to_corr <- function(theta, p, iLtheta, d0 = p:1) {
  L <- diag(x = d0, nrow = p, ncol = p)
  L[iLtheta] <- theta
  L <- gcp_fill_chol(L)
  Q <- L %*% t(L)
  V <- solve(Q)
  s <- sqrt(diag(V))
  S_inv <- diag(1 / s, nrow = p, ncol = p)
  C <- S_inv %*% V %*% S_inv
  # Ensure exact symmetry and unit diagonal
  C <- (C + t(C)) / 2
  diag(C) <- 1
  C
}

#' Compute Jacobian d vec(C_free) / d theta via central differences.
#'
#' @inheritParams gcp_to_corr
#' @param h Step size for central differences. Default: 1e-5.
#' @return A matrix with nrow = length(theta) (free correlations) and
#'   ncol = length(theta).
#' @keywords internal
gcp_jac_corr <- function(theta, p, iLtheta, d0 = p:1, h = 1e-5) {
  m <- length(theta)
  J <- matrix(0, nrow = m, ncol = m)
  for (k in seq_len(m)) {
    th_plus <- th_minus <- theta
    th_plus[k] <- theta[k] + h
    th_minus[k] <- theta[k] - h
    C_plus <- gcp_to_corr(th_plus, p, iLtheta, d0)
    C_minus <- gcp_to_corr(th_minus, p, iLtheta, d0)
    J[, k] <- (C_plus[iLtheta] - C_minus[iLtheta]) / (2 * h)
  }
  J
}

#' Compute the correlation matrix and its analytical Jacobian for an GCP block.
#'
#' Works for both dense blocks (all lower-triangular elements free) and sparse
#' blocks (only positions in iLtheta are free).
#'
#' @param theta Numeric vector of free parameters.
#' @param p Integer, dimension of the correlation matrix.
#' @param d0 Numeric vector of length p, diagonal of L^(0). Default: p:1.
#' @param iLtheta Integer vector of lower-triangular positions (column-major)
#'   that are free. NULL means all lower-triangular positions (dense).
#' @return A list with components:
#'   - C: The p x p correlation matrix.
#'   - J: The m x m Jacobian matrix d rho / d theta.
#' @keywords internal
gcp_with_jac_dense <- function(theta, p, d0 = p:1, iLtheta = NULL) {
  L <- diag(d0)
  if (is.null(iLtheta)) {
    lt_idx <- which(lower.tri(diag(p)))
  } else {
    lt_idx <- iLtheta
  }
  L[lt_idx] <- theta

  Q <- tcrossprod(L)       # L %*% t(L)
  V <- solve(Q)
  C <- cov2cor(V)
  s <- sqrt(diag(V))

  m <- length(theta)
  lt_rows <- row(diag(p))[lt_idx]
  lt_cols <- col(diag(p))[lt_idx]

  # Precompute constants used for all columns of J
  C_vals <- C[lt_idx]
  V_diag <- diag(V)
  term_a <- C_vals / (2 * V_diag[lt_rows])
  term_b <- C_vals / (2 * V_diag[lt_cols])
  inv_s_prod <- 1 / (s[lt_rows] * s[lt_cols])

  # Precompute V %*% L once (p x p BLAS call)
  VL <- V %*% L

  J <- matrix(0, nrow = m, ncol = m)
  for (k in seq_len(m)) {
    i <- lt_rows[k]   # row of L_{ij}
    j <- lt_cols[k]   # col of L_{ij}

    # dV/dL_{ij} = -V (e_i L_j^T + L_j e_i^T) V
    #            = -(V_{:,i})(L_j^T V) - (V L_j)(V_{i,:})
    vi  <- V[, i]      # V e_i
    wj  <- VL[, j]     # V L_{:,j}

    # Vectorized over all (a,b) in lower triangle
    dV_ab <- -(vi[lt_rows] * wj[lt_cols] + wj[lt_rows] * vi[lt_cols])
    dV_aa <- -2 * vi[lt_rows] * wj[lt_rows]
    dV_bb <- -2 * vi[lt_cols] * wj[lt_cols]

    J[, k] <- dV_ab * inv_s_prod - term_a * dV_aa - term_b * dV_bb
  }

  list(C = C, J = J)
}

#' Extract GCP sparsity pattern from an INLAvaan parameter table.
#'
#' Given a parameter table and a group, identifies the free correlation
#' parameters and returns the GCP metadata needed for block processing.
#'
#' @param pt Parameter table (list).
#' @param g Integer, group number.
#' @param block Character, one of "theta" or "psi".
#' @return A list with components:
#'   - p: dimension of the block
#'   - var_names: ordered variable names
#'   - iLtheta: integer vector of lower-triangle positions for free params
#'   - d0: diagonal vector (default p:1)
#'   - pt_idx: indices into the parameter table for the correlation rows
#'   - pt_var_idx: indices for the corresponding variance rows
#'   - is_dense: logical, TRUE if all p(p-1)/2 positions are free
#' Returns NULL if no correlation parameters exist for this block/group.
#' @keywords internal
gcp_graph_from_pt <- function(pt, g, block = c("theta", "psi")) {
  block <- match.arg(block)

  # What matrix types are we looking for?
  cor_pattern <- paste0(block, "_co")  # matches theta_cor, theta_cov, psi_cor, psi_cov
  var_pattern <- paste0(block, "_var")

  # Find free correlation rows in this group
  cor_idx <- which(
    grepl(cor_pattern, pt$mat) &
    pt$group == g &
    pt$free > 0
  )
  if (length(cor_idx) == 0L) return(NULL)

  # Collect all variable names involved in correlations
  cor_vars <- unique(c(pt$lhs[cor_idx], pt$rhs[cor_idx]))

  # Also include variables that have variances but no correlations
  # (they contribute to the block dimension)
  var_idx <- which(
    pt$mat == var_pattern &
    pt$group == g &
    pt$lhs %in% cor_vars
  )
  var_names <- sort(unique(pt$lhs[var_idx]))
  p_block <- length(var_names)

  if (p_block < 2L) return(NULL)

  # Build the lower-triangle index mapping
  # For each free correlation (lhs ~~ rhs), find position in L
  ref_mat <- matrix(0L, p_block, p_block,
                    dimnames = list(var_names, var_names))
  lt_all <- which(lower.tri(ref_mat))

  iLtheta <- integer(length(cor_idx))
  for (k in seq_along(cor_idx)) {
    ci <- cor_idx[k]
    i <- match(pt$lhs[ci], var_names)
    j <- match(pt$rhs[ci], var_names)
    # Ensure i > j (lower triangle)
    if (i < j) { tmp <- i; i <- j; j <- tmp }
    iLtheta[k] <- (j - 1L) * p_block + i  # column-major vectorised index
  }
  # Sort to match column-major order
  sort_ord <- order(iLtheta)
  iLtheta <- iLtheta[sort_ord]
  cor_idx <- cor_idx[sort_ord]

  d0 <- p_block:1

  list(
    p = p_block,
    var_names = var_names,
    iLtheta = iLtheta,
    d0 = d0,
    pt_cor_idx = cor_idx,
    pt_var_idx = var_idx,
    is_dense = length(iLtheta) == p_block * (p_block - 1L) / 2L
  )
}

#' Build a list of GCP block metadata from a parameter table.
#'
#' Scans the parameter table for all groups and blocks (theta, psi)
#' and returns GCP metadata for each block that has free correlations.
#'
#' @param pt Parameter table (list).
#' @return A list of GCP block metadata, each element from gcp_graph_from_pt().
#'   Returns an empty list if no GCP blocks are found.
#' @keywords internal
gcp_blocks_from_pt <- function(pt) {
  is_multilvl <- "level" %in% names(pt)
  if (is_multilvl) {
    nG <- max(pt$level)
  } else {
    nG <- max(pt$group)
  }

  blocks <- list()
  for (g in seq_len(nG)) {
    for (block in c("theta", "psi")) {
      info <- gcp_graph_from_pt(pt, g, block)
      if (!is.null(info)) {
        info$group <- g
        info$block <- block
        blocks[[length(blocks) + 1L]] <- info
      }
    }
  }
  blocks
}
