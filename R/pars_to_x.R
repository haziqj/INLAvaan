# The parameter map of a model: everything that pars_to_x() and the chain rule
# of the gradient need from the parameter table, found once per model. One call
# of pars_to_x() is then vector work on these index vectors, with no search of
# the table. The entries are plain integer vectors and small tables.
#
# The transform of a row is read from its `ginv`: the identity, exp (variances)
# or tanh (correlations). The derivative of these three is known in closed form.
# A row with any other function is an "other" row, and its `ginv` and
# `ginv_prime` are called one row at a time.
pars_to_x_map <- function(pt) {
  npt <- length(pt$free)
  group <- if ("level" %in% names(pt)) partable_level_index(pt) else pt$group
  is_free <- pt$free > 0L
  free_rows <- which(is_free)
  packed_rows <- which(is_free & !duplicated(pt$free))
  thidx <- integer(npt)
  thidx[free_rows] <- seq_along(free_rows)

  kind <- vapply(
    pt$ginv,
    function(f) {
      if (identical(f, identity)) {
        0L
      } else if (identical(f, exp)) {
        1L
      } else if (identical(f, tanh)) {
        2L
      } else {
        3L
      }
    },
    integer(1)
  )
  kind_free <- kind[free_rows]

  # Covariances held equal share a free index, and the packed x keeps the first
  # such row (the owner). So every copy is scaled by the owner's variances.
  owner <- seq_len(npt)
  is_copy <- is_free & duplicated(pt$free)
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
  k <- owner[idxcov]
  with_comp <- paste(pt$lhs[k], group[k]) %in%
    comp_keys |
    paste(pt$rhs[k], group[k]) %in% comp_keys

  # The variance rows of the two ends of each covariance, in its own group or
  # level
  var_at <- which(pt$op == "~~" & pt$lhs == pt$rhs)
  var_key <- paste(pt$lhs[var_at], group[var_at], sep = "\r")
  var1 <- var_at[match(paste(pt$lhs[k], group[k], sep = "\r"), var_key)]
  var2 <- var_at[match(paste(pt$rhs[k], group[k], sep = "\r"), var_key)]
  if (anyNA(var1) || anyNA(var2)) {
    # nocov start
    cli_abort(
      "A covariance has no variance row for one of its two variables.",
      .internal = TRUE
    )
  } # nocov end
  var_rows <- matrix(0L, npt, 2L)
  var_rows[idxcov, ] <- cbind(var1, var2)

  # The off-diagonal terms of the Jacobian: each covariance moves with the log
  # variances of its two ends. Two rows per covariance, in the order of the
  # covariances, without the rows whose variance or covariance is fixed. `src`
  # is the covariance that gives the value of a row.
  cov_rows <- idxcov[!with_comp]
  jcb_i <- c(rbind(thidx[var1[!with_comp]], thidx[var2[!with_comp]]))
  jcb_j <- rep(thidx[cov_rows], each = 2L)
  jcb_src <- rep(seq_along(cov_rows), each = 2L)
  keep <- jcb_i != 0L & jcb_j != 0L
  jcb_i <- jcb_i[keep]
  jcb_j <- jcb_j[keep]

  # The chain rule adds the off-diagonal terms to the gradient one row at a
  # time. A round holds at most one row per target, and the rows of a target
  # keep their order over the rounds, so that adding a round at a time gives
  # the same sums as adding a row at a time. The pattern of a model with
  # composites is not known here, and its terms are added row by row.
  jcb_rounds <- NULL
  if (length(comp) == 0L && length(jcb_i) > 0L) {
    turn <- stats::ave(seq_along(jcb_i), jcb_i, FUN = seq_along)
    jcb_rounds <- unname(lapply(
      split(seq_along(jcb_i), turn),
      function(rows) list(rows = rows, i = jcb_i[rows], j = jcb_j[rows])
    ))
  }

  list(
    group = group,
    free_rows = free_rows,
    packed_rows = packed_rows,
    thidx = thidx,
    owner = owner,
    exp_rows = which(kind == 1L),
    tanh_rows = which(kind == 2L),
    other_rows = which(kind == 3L),
    exp_free = which(kind_free == 1L),
    tanh_free = which(kind_free == 2L),
    other_free = which(kind_free == 3L),
    has_cov = length(idxcov) > 0L,
    cov_rows = cov_rows,
    cov_var1 = var1[!with_comp],
    cov_var2 = var2[!with_comp],
    var_rows = var_rows,
    jcb_i = jcb_i,
    jcb_j = jcb_j,
    jcb_src = jcb_src[keep],
    jcb_rounds = jcb_rounds,
    comp = comp,
    comp_cov_rows = idxcov[with_comp]
  )
}

pars_to_x <- function(theta, pt, map = NULL) {
  # Convert unrestricted theta-side parameters to lavaan-side parameters x.
  # Always receive UNPACKED theta and returns PACKED theta. `map` is the
  # parameter map of pars_to_x_map(pt), built here when it is not given. A
  # caller with many theta builds it once.
  if (is.null(pt) | missing(pt)) {
    # nocov
    cli_abort("Parameter table 'pt' must be provided.")
  }
  if (is.null(map)) {
    map <- pars_to_x_map(pt)
  }

  pars <- pt$parstart
  pars[map$free_rows] <- theta
  x <- pars
  x[map$exp_rows] <- exp(pars[map$exp_rows])
  x[map$tanh_rows] <- tanh(pars[map$tanh_rows])
  for (i in map$other_rows) {
    x[i] <- pt$ginv[[i]](pars[i])
  }
  xx <- x
  sd1sd2 <- rep(1, length(x))
  jcb_mat <- NULL

  # A covariance is held as a correlation, and is scaled here by the standard
  # deviations of its two ends
  if (map$has_cov) {
    cov_rows <- map$cov_rows
    sd1 <- sqrt(x[map$cov_var1])
    sd2 <- sqrt(x[map$cov_var2])
    rho <- x[cov_rows]
    x[cov_rows] <- rho * sd1 * sd2
    sd1sd2[cov_rows] <- sd1 * sd2
    half_cov <- 0.5 * rho * sd1 * sd2
    jcb_mat <- matrix(
      c(map$jcb_i, map$jcb_j, half_cov[map$jcb_src]),
      ncol = 3L
    )
  }

  if (length(map$comp) > 0L) {
    with_comp <- pars_to_x_composites(x, pars, pt, map, sd1sd2, jcb_mat)
    x <- with_comp$x
    sd1sd2 <- with_comp$sd1sd2
    jcb_mat <- with_comp$jcb_mat
  }

  out <- x[map$packed_rows]
  attr(out, "xcor") <- xx[map$packed_rows]
  attr(out, "sd1sd2") <- sd1sd2[map$free_rows]
  attr(out, "jcb_mat") <- jcb_mat
  out
}

# The composite part of pars_to_x(): the variance of each composite at the
# current weights and T, then the covariances with a composite, one row at a
# time. These come after every other covariance, because T is among those.
pars_to_x_composites <- function(x, pars, pt, map, sd1sd2, jcb_mat) {
  thidx <- map$thidx
  comp_tw <- composite_variances(
    map$comp,
    x,
    pars,
    pt,
    thidx,
    sd1sd2,
    map$var_rows
  )
  for (ct in comp_tw) {
    x[ct$vrow] <- ct$var
  }

  for (j in map$comp_cov_rows) {
    k <- map$owner[j]
    ends <- c(pt$lhs[k], pt$rhs[k])
    where_var <- map$var_rows[j, ]
    sd1 <- sqrt(x[where_var[1L]])
    sd2 <- sqrt(x[where_var[2L]])
    rho <- x[j]
    x[j] <- rho * sd1 * sd2

    thidx1 <- thidx[where_var[1L]]
    thidx2 <- thidx[where_var[2L]]
    thidx3 <- thidx[j]
    jcb_mat <- rbind(jcb_mat, c(thidx1, thidx3, 0.5 * rho * sd1 * sd2))
    jcb_mat <- rbind(jcb_mat, c(thidx2, thidx3, 0.5 * rho * sd1 * sd2))
    # A composite's sd moves with its weights and with T:
    # d sd(C) = (2 w'T dw + w' dT w) / (2 sd(C))
    for (side in 1:2) {
      ct <- comp_tw[[paste(ends[side], map$group[k])]]
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
    sd1sd2[j] <- sd1 * sd2
  }
  if (!is.null(jcb_mat)) {
    jcb_mat <- jcb_mat[jcb_mat[, 1] != 0 & jcb_mat[, 2] != 0, , drop = FALSE]
  }
  list(x = x, sd1sd2 = sd1sd2, jcb_mat = jcb_mat)
}

# Derivative of the transform of each free row at `theta` (unpacked): the
# diagonal of the Jacobian of pars_to_x(), before the scaling of the
# covariances.
pars_to_x_jcb <- function(theta, pt, map) {
  jcb <- rep(1, length(theta))
  jcb[map$exp_free] <- exp(theta[map$exp_free])
  jcb[map$tanh_free] <- 1 - tanh(theta[map$tanh_free])^2
  for (k in map$other_free) {
    jcb[k] <- pt$ginv_prime[[map$free_rows[k]]](theta[k])
  }
  jcb
}

# Chain rule from a gradient in lavaan's x (`gll`, one entry per free row) to
# the gradient in the unpacked `theta`, where `x` is pars_to_x(theta, pt, map).
# The Jacobian is a diagonal (the transform of each row, times sd1sd2 for a
# covariance) plus the off-diagonal terms in jcb_mat, from the variances into
# the covariances. So the product is vector work, and the dense matrix is
# never formed. This sits inside every gradient call.
pars_chain_rule <- function(gll, theta, x, pt, map) {
  out <- pars_to_x_jcb(theta, pt, map) * attr(x, "sd1sd2") * gll
  jcb_mat <- attr(x, "jcb_mat")
  if (is.null(jcb_mat)) {
    return(out)
  }
  if (is.null(map$jcb_rounds)) {
    for (k in seq_len(nrow(jcb_mat))) {
      i <- jcb_mat[k, 1]
      j <- jcb_mat[k, 2]
      out[i] <- out[i] + jcb_mat[k, 3] * gll[j]
    }
  } else {
    value <- jcb_mat[, 3L]
    for (round in map$jcb_rounds) {
      out[round$i] <- out[round$i] + value[round$rows] * gll[round$j]
    }
  }
  out
}
