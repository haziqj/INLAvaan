pars_to_x <- function(theta, pt) {
  # Convert unrestricted theta-side parameters to lavaan-side parameters x.
  # Always receive UNPACKED theta and returns PACKED theta.
  if (is.null(pt) | missing(pt)) { # nocov
    cli_abort("Parameter table 'pt' must be provided.")
  }

  is_multilvl <- "level" %in% names(pt)
  if (is_multilvl) {
    pt$group <- pt$level
  }
  nG <- max(pt$group)
  idxfree <- pt$free > 0
  pars <- pt$parstart
  pars[idxfree] <- theta
  npt <- length(pars)
  xx <- x <- mapply(function(f, z) f(z), pt$ginv, pars)
  sd1sd2 <- rep(1, npt)
  jcb_mat <- NULL
  thidx <- integer(npt)
  thidx[pt$free > 0] <- seq_len(sum(pt$free > 0))

  # --- ITP block processing ---------------------------------------------------
  # If ITP blocks exist, compute correlation matrices from block theta vectors
  # and overwrite the per-parameter correlation values.
  itp_blocks <- pt$itp_blocks
  has_itp <- length(itp_blocks) > 0
  itp_cor_indices <- integer(0)  # track which pt indices are handled by ITP

  if (has_itp) {
    for (blk in itp_blocks) {
      # Extract the theta values for this block's correlation parameters
      blk_theta <- pars[blk$pt_cor_idx]

      # Compute the full correlation matrix via ITP
      C_blk <- itp_to_corr(blk_theta, blk$p, blk$iLtheta, blk$d0)

      # Overwrite the per-parameter x values with the ITP-derived correlations
      for (k in seq_along(blk$pt_cor_idx)) {
        ci <- blk$pt_cor_idx[k]
        i_name <- pt$lhs[ci]
        j_name <- pt$rhs[ci]
        i_pos <- match(i_name, blk$var_names)
        j_pos <- match(j_name, blk$var_names)
        x[ci] <- C_blk[i_pos, j_pos]
        xx[ci] <- C_blk[i_pos, j_pos]  # xcor also gets the ITP value
      }
      itp_cor_indices <- c(itp_cor_indices, blk$pt_cor_idx)
    }
  }

  # Now deal with covariances
  for (g in seq_len(nG)) {
    idxcov <- which(grepl("cov", pt$mat) & pt$group == g)
    for (j in idxcov) {
      X1 <- pt$lhs[j]
      X2 <- pt$rhs[j]
      where_varX1 <- which(
        pt$lhs == X1 & pt$op == "~~" & pt$rhs == X1 & pt$group == g
      )
      where_varX2 <- which(
        pt$lhs == X2 & pt$op == "~~" & pt$rhs == X2 & pt$group == g
      )

      sd1 <- sqrt(x[where_varX1])
      sd2 <- sqrt(x[where_varX2])
      rho <- x[j]
      x[j] <- rho * sd1 * sd2

      thidx1 <- thidx[where_varX1]
      thidx2 <- thidx[where_varX2]
      thidx3 <- thidx[j]
      jcb_mat <- rbind(jcb_mat, c(thidx1, thidx3, 0.5 * rho * sd1 * sd2))
      jcb_mat <- rbind(jcb_mat, c(thidx2, thidx3, 0.5 * rho * sd1 * sd2))
      sd1sd2[j] <- sd1 * sd2
    }
  }
  jcb_mat <- jcb_mat[jcb_mat[, 1] != 0 & jcb_mat[, 2] != 0, ]

  out <- x[pt$free > 0L & !duplicated(pt$free)]
  attr(out, "xcor") <- xx[pt$free > 0L & !duplicated(pt$free)]
  attr(out, "sd1sd2") <- sd1sd2[idxfree]
  attr(out, "jcb_mat") <- jcb_mat
  attr(out, "itp_blocks") <- itp_blocks
  out
}
