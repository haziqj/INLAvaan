pars_to_x <- function(theta, pt) {
  # Convert unrestricted theta-side parameters to lavaan-side parameters x.
  # Always receive UNPACKED theta and returns PACKED theta.
  if (is.null(pt) | missing(pt)) { # nocov
    cli_abort("Parameter table 'pt' must be provided.")
  }

  is_multilvl <- "level" %in% names(pt)
  if (is_multilvl) {
    pt$group <- partable_level_index(pt)
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

  # Covariances held equal share a free index, and the packed x keeps the first
  # such row (the owner). So every copy is scaled by the owner's variances.
  owner <- seq_len(npt)
  is_copy <- pt$free > 0L & duplicated(pt$free)
  owner[is_copy] <- match(pt$free[is_copy], pt$free)

  # A composite's ~~ row is not a parameter: its variance is w'Tw at the current
  # weights and indicator covariances T. For an endogenous composite that total
  # variance bounds the residual variance lavaan derives, so a covariance scaled
  # by it can still reach every admissible value. T may be free, so covariances
  # without a composite (T among them) come first and those with one after.
  comp <- attr(pt, "composites")
  if (is.null(comp) && any(pt$op == "<~")) {
    comp <- composite_blocks(pt)
  }
  comp_keys <- vapply(comp, function(cb) paste(cb$name, cb$group), "")
  idxcov <- which(grepl("cov", pt$mat))
  with_comp <- paste(pt$lhs[owner[idxcov]], pt$group[owner[idxcov]]) %in%
    comp_keys |
    paste(pt$rhs[owner[idxcov]], pt$group[owner[idxcov]]) %in% comp_keys
  comp_cov <- idxcov[with_comp]
  idxcov <- c(idxcov[!with_comp], comp_cov)
  var_rows <- matrix(0L, npt, 2L)
  comp_tw <- list()
  comp_done <- length(comp) == 0L

  for (j in idxcov) {
    if (!comp_done && j %in% comp_cov) {
      comp_tw <- composite_variances(comp, x, pars, pt, thidx, sd1sd2, var_rows)
      for (ct in comp_tw) {
        x[ct$vrow] <- ct$var
      }
      comp_done <- TRUE
    }
    k <- owner[j]
    X1 <- pt$lhs[k]
    X2 <- pt$rhs[k]
    where_varX1 <- which(
      pt$lhs == X1 & pt$op == "~~" & pt$rhs == X1 & pt$group == pt$group[k]
    )
    where_varX2 <- which(
      pt$lhs == X2 & pt$op == "~~" & pt$rhs == X2 & pt$group == pt$group[k]
    )
    var_rows[j, ] <- c(where_varX1[1L], where_varX2[1L])

    sd1 <- sqrt(x[where_varX1])
    sd2 <- sqrt(x[where_varX2])
    rho <- x[j]
    x[j] <- rho * sd1 * sd2

    thidx1 <- thidx[where_varX1]
    thidx2 <- thidx[where_varX2]
    thidx3 <- thidx[j]
    jcb_mat <- rbind(jcb_mat, c(thidx1, thidx3, 0.5 * rho * sd1 * sd2))
    jcb_mat <- rbind(jcb_mat, c(thidx2, thidx3, 0.5 * rho * sd1 * sd2))
    # A composite's sd moves with its weights and with T:
    # d sd(C) = (2 w'T dw + w' dT w) / (2 sd(C))
    if (length(comp_tw) > 0L) {
      for (side in 1:2) {
        ct <- comp_tw[[paste(c(X1, X2)[side], pt$group[k])]]
        if (is.null(ct)) {
          next
        }
        sd_this <- c(sd1, sd2)[side]
        sd_other <- c(sd2, sd1)[side]
        jcb_mat <- rbind(
          jcb_mat,
          cbind(thidx[ct$wrows], thidx3, rho * sd_other * ct$tw / sd_this)
        )
        if (length(ct$t_th) > 0L) {
          jcb_mat <- rbind(
            jcb_mat,
            cbind(ct$t_th, thidx3, rho * sd_other * ct$t_d / (2 * sd_this))
          )
        }
      }
    }
    sd1sd2[j] <- sd1 * sd2
  }
  if (!comp_done) {
    for (ct in composite_variances(
      comp,
      x,
      pars,
      pt,
      thidx,
      sd1sd2,
      var_rows
    )) {
      x[ct$vrow] <- ct$var
    }
  }
  if (!is.null(jcb_mat)) {
    jcb_mat <- jcb_mat[jcb_mat[, 1] != 0 & jcb_mat[, 2] != 0, , drop = FALSE]
  }

  out <- x[pt$free > 0L & !duplicated(pt$free)]
  attr(out, "xcor") <- xx[pt$free > 0L & !duplicated(pt$free)]
  attr(out, "sd1sd2") <- sd1sd2[idxfree]
  attr(out, "jcb_mat") <- jcb_mat
  out
}
