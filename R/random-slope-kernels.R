# Random-slope kernels: the log-likelihood of a two-level model with random
# slopes (lavaan's rv() modifier) and its gradient, for lavaan's closed-form
# route with complete data and no between-only responses (rs_kernel_ok()).
#
# Given the covariates, the data of cluster j are
#   y_ij = mu_y + P x_ij + Z_ij v_j + e_ij,  v_j ~ N(d_j, Sigma_v),
#   e_ij ~ N(0, Sigma_w),
# and the cluster effects v_j integrate out in closed form. lavaan scores the
# clusters one at a time, in an R loop, from the cluster sums of its
# random-slope cache. Its gradient loops over the clusters again and does the
# p1 x p1 algebra of every cluster inside the loop.
#
# Here every cluster quantity is small (p_v x p_v, with p_v the number of
# cluster effects), so it is stored one cluster per row and worked as vectors
# over all clusters, with loops only over the p_v indices. lavaan's
# Z_j = I + A_j Sigma_v is not symmetric, so it is replaced by a symmetric
# matrix with the same determinant: B_j = I + H A_j H with H = Sigma_v^(1/2)
# when Sigma_v is positive semidefinite, and B_j = I + L_j' Sigma_v L_j with
# A_j = L_j L_j' when it is not (lavaan scores such points too, and the
# optimiser visits them on unrestricted level-2 covariances). Either form gives
# S_j = Sigma_v Z_j^-1, the posterior covariance of v_j, from which the
# log-likelihood and the gradient follow. Because Sigma_w is the same for every
# cluster, each
# cluster term of the gradient is linear in the posterior moments of v_j, with
# coefficients from the data. So the gradient sums over the clusters first and
# does the p1 x p1 algebra once. Everything that depends only on the data is in
# the plan of rs_kernel_plan().
#
# The derivative with respect to lavaan's free parameters takes lavaan's
# central differences of the implied pieces (lav_mvn_cl_rs_implied()), so it
# is lavaan's gradient up to rounding.

# The models these kernels take. Everything else keeps lavaan's code.
rs_kernel_ok <- function(lavmodel, lavdata, lavcache) {
  if (
    lavmodel@estimator != "ML" ||
      !has_random_slopes(lavmodel) ||
      lavdata@nlevels != 2L ||
      lavdata@ngroups != 1L ||
      length(lavcache) == 0L
  ) {
    return(FALSE)
  }
  rs <- lavcache[[1L]]$rs
  !is.null(rs) &&
    !isTRUE(rs$info$nl.flag) &&
    is.null(rs$stats$mp) &&
    identical(as.integer(rs$info$kz), 0L)
}

## ----- Plan ------------------------------------------------------------------

# The cluster sums as matrices and arrays with one row per cluster, and their
# totals over the clusters
rs_kernel_plan <- function(rs) {
  st <- rs$stats
  info <- rs$info
  n_cl <- st$nclusters
  nx <- info$nx
  p1 <- info$p1
  sxx <- array(0, dim = c(n_cl, nx, nx))
  sxy <- array(0, dim = c(n_cl, nx, p1))
  syy_total <- matrix(0, p1, p1)
  for (j in seq_len(n_cl)) {
    sxx[j, , ] <- st$sxx[[j]]
    sxy[j, , ] <- st$sxy[[j]]
    syy_total <- syy_total + st$syy[[j]]
  }
  list(
    info = info,
    n_cl = n_cl,
    nj = st$cluster.size,
    sy = matrix(st$sy, n_cl, p1),
    sx = matrix(st$sx, n_cl, nx),
    sxx = sxx,
    sxy = sxy,
    exo_b = st$exo.b,
    n_total = sum(st$cluster.size),
    sy_total = colSums(matrix(st$sy, n_cl, p1)),
    sx_total = colSums(matrix(st$sx, n_cl, nx)),
    sxx_total = matrix(apply(sxx, c(2L, 3L), sum), nx, nx),
    sxy_total = matrix(apply(sxy, c(2L, 3L), sum), nx, p1),
    syy_total = syy_total,
    path_x = info$path.tab$x.idx,
    path_z = info$path.tab$z.idx,
    nx = nx,
    p1 = p1,
    nexo = info$nexo.b
  )
}

## ----- Small matrices, one per cluster ---------------------------------------

# Every helper below holds one k x k matrix per row, stored by column, and
# works with vector operations over the rows.
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

## ----- Cluster terms ---------------------------------------------------------

# A_j, p_j and the residual sums of every cluster, as lavaan's loops build
# them. `w_mat` is the inverse of Sigma_w.
rs_cluster_terms <- function(plan, imp, w_mat) {
  pv <- imp$pv
  p1 <- plan$p1
  nx <- plan$nx
  n_cl <- plan$n_cl
  path_x <- plan$path_x
  path_zcol <- imp$z.v.idx[plan$path_z]
  lmat <- imp$lmat
  q0 <- imp$q0
  pmat <- imp$P
  mu_y <- imp$mu_y
  wl <- w_mat %*% lmat
  ltwl <- crossprod(lmat, wl)
  wq0 <- w_mat %*% q0
  q0twl <- crossprod(q0, wl)
  a <- tcrossprod(plan$nj, as.numeric(crossprod(q0, wq0)))
  se <- plan$sy - tcrossprod(plan$nj, mu_y) - plan$sx %*% t(pmat)
  p <- se %*% wq0
  sxe <- vector("list", nx)
  for (xi in seq_len(nx)) {
    sxe[[xi]] <- matrix(plan$sxy[, xi, ], n_cl, p1) -
      tcrossprod(plan$sx[, xi], mu_y) -
      matrix(plan$sxx[, xi, ], n_cl, nx) %*% t(pmat)
  }
  for (i in seq_along(path_x)) {
    zi <- path_zcol[i]
    xi <- path_x[i]
    for (r in seq_len(pv)) {
      term <- q0twl[r, i] * plan$sx[, xi]
      a[, rs_at(r, zi, pv)] <- a[, rs_at(r, zi, pv)] + term
      a[, rs_at(zi, r, pv)] <- a[, rs_at(zi, r, pv)] + term
    }
    for (k in seq_along(path_x)) {
      a[, rs_at(zi, path_zcol[k], pv)] <- a[, rs_at(zi, path_zcol[k], pv)] +
        ltwl[i, k] * plan$sxx[, xi, path_x[k]]
    }
    p[, zi] <- p[, zi] + as.numeric(sxe[[xi]] %*% wl[, i])
  }
  d <- matrix(imp$mu.v, n_cl, pv, byrow = TRUE)
  exo_c <- NULL
  if (plan$nexo > 0L) {
    exo_c <- sweep(plan$exo_b, 2L, imp$mu.exo)
    d <- d + exo_c %*% t(imp$cc)
  }
  list(a = a, p = p, se = se, sxe = sxe, d = d, exo_c = exo_c)
}

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

rs_w_inverse <- function(sigma_w) {
  ch <- tryCatch(chol(sigma_w), error = function(e) NULL)
  if (is.null(ch)) {
    return(NULL)
  }
  list(inv = chol2inv(ch), logdet = 2 * sum(log(diag(ch))))
}

## ----- Log-likelihood --------------------------------------------------------

# The log-likelihood with the 2 pi terms, as lavaan's lav_mvn_cl_rs_m2ll()
# returns it with `log2pi = TRUE, minus_two = FALSE`, or -Inf when it is not
# defined. NULL sends the call back to lavaan (see rs_cluster_posterior()).
rs_kernel_loglik <- function(plan, imp) {
  if (is.null(imp)) {
    return(-Inf)
  }
  pv <- imp$pv
  w <- rs_w_inverse(imp$sigma_w)
  if (is.null(w)) {
    return(-Inf)
  }
  w_mat <- w$inv
  ct <- rs_cluster_terms(plan, imp, w_mat)
  pmat <- imp$P
  mu_y <- imp$mu_y
  wp <- w_mat %*% pmat
  mu_y_w <- as.numeric(w_mat %*% mu_y)

  # The part that does not depend on the cluster effects, from the totals
  quad0 <- sum(w_mat * plan$syy_total) -
    2 * sum(mu_y_w * plan$sy_total) +
    plan$n_total * sum(mu_y_w * mu_y) -
    2 * sum(t(wp) * plan$sxy_total) +
    2 * sum(as.numeric(mu_y_w %*% pmat) * plan$sx_total) +
    sum(crossprod(pmat, wp) * plan$sxx_total)

  # The cluster effects, integrated out
  ad <- rs_batch_mv(ct$a, ct$d, pv)
  p_tilde <- ct$p - ad
  quad_d <- -2 * rowSums(ct$d * ct$p) + rowSums(ct$d * ad)
  post <- rs_cluster_posterior(ct$a, p_tilde, imp$sigma.v, pv)
  if (post$status == "fallback") {
    return(NULL)
  }
  if (post$status == "undefined") {
    return(-Inf)
  }
  m2ll <- sum(plan$nj) *
    w$logdet +
    sum(post$logdet) +
    sum(quad_d) -
    sum(post$quad) +
    quad0 +
    plan$n_total * plan$p1 * log(2 * pi)
  -m2ll / 2
}

## ----- Gradient --------------------------------------------------------------

# The derivative of minus twice the log-likelihood with respect to lavaan's
# implied pieces, in the order of lav_mvn_cl_rs_phi_vec(). It equals colSums()
# of lavaan's lav_mvn_cl_rs_dfphi(). NULL when it is not defined.
rs_kernel_dphi <- function(plan, imp) {
  pv <- imp$pv
  p1 <- plan$p1
  nx <- plan$nx
  n_cl <- plan$n_cl
  nj <- plan$nj
  path_x <- plan$path_x
  path_zcol <- imp$z.v.idx[plan$path_z]
  npaths <- length(path_x)
  lmat <- imp$lmat
  q0 <- imp$q0
  pmat <- imp$P
  mu_y <- imp$mu_y
  w <- rs_w_inverse(imp$sigma_w)
  if (is.null(w)) {
    return(NULL)
  }
  w_mat <- w$inv
  ct <- rs_cluster_terms(plan, imp, w_mat)
  a <- ct$a
  se <- ct$se
  sxe <- ct$sxe
  at <- function(i, j) rs_at(i, j, pv)

  ## ----- Posterior moments of v_j --------------------------------------------
  p_tilde <- ct$p - rs_batch_mv(a, ct$d, pv)
  post <- rs_cluster_posterior(a, p_tilde, imp$sigma.v, pv, s0 = TRUE)
  if (post$status != "ok") {
    return(NULL)
  }
  # The posterior covariance S_j = Sigma_v Z_j^-1, one cluster per row
  s0 <- post$s0
  u <- rs_batch_mv(s0, p_tilde, pv)
  zp <- p_tilde - rs_batch_mv(a, u, pv)
  mu0 <- ct$d + u
  g_a <- s0
  g_sv <- a - rs_batch_mm(rs_batch_mm(a, s0, pv), a, pv)
  for (r in seq_len(pv)) {
    for (s in seq_len(pv)) {
      g_a[, at(r, s)] <- g_a[, at(r, s)] + mu0[, r] * mu0[, s]
      g_sv[, at(r, s)] <- g_sv[, at(r, s)] - zp[, r] * zp[, s]
    }
  }
  g_p <- -2 * mu0
  g_d <- -2 * zp

  ## ----- Sums over the clusters ----------------------------------------------
  ga_n <- matrix(colSums(nj * g_a), pv, pv)
  sx_ga <- lapply(seq_len(npaths), function(r) {
    colSums(plan$sx[, path_x[r]] * g_a[, at(seq_len(pv), path_zcol[r])])
  })
  sxx_ga <- matrix(0, npaths, npaths)
  for (r in seq_len(npaths)) {
    for (s in seq_len(npaths)) {
      sxx_ga[r, s] <- sum(
        plan$sxx[, path_x[r], path_x[s]] * g_a[, at(path_zcol[r], path_zcol[s])]
      )
    }
  }
  gp_se <- crossprod(g_p, se)
  gp_sxe <- lapply(seq_len(npaths), function(r) {
    as.numeric(crossprod(g_p[, path_zcol[r]], sxe[[path_x[r]]]))
  })
  sx_gp <- vapply(
    seq_len(npaths),
    function(r) sum(plan$sx[, path_x[r]] * g_p[, path_zcol[r]]),
    numeric(1)
  )
  psx <- pmat %*% plan$sx_total
  see <- plan$syy_total -
    outer(plan$sy_total, mu_y) -
    outer(mu_y, plan$sy_total) +
    plan$n_total * tcrossprod(mu_y) -
    t(plan$sxy_total) %*% t(pmat) -
    pmat %*% plan$sxy_total +
    mu_y %*% t(psx) +
    psx %*% t(mu_y) +
    pmat %*% plan$sxx_total %*% t(pmat)

  ## ----- The p1 x p1 algebra, once -------------------------------------------
  gw <- q0 %*% ga_n %*% t(q0) + q0 %*% gp_se + see
  sq_ga <- q0 %*% ga_n
  for (r in seq_len(npaths)) {
    arv <- as.numeric(q0 %*% sx_ga[[r]])
    gw <- gw + outer(lmat[, r], arv) + outer(arv, lmat[, r])
    gw <- gw + outer(lmat[, r], gp_sxe[[r]])
    sq_ga <- sq_ga + outer(lmat[, r], sx_ga[[r]])
    for (s in seq_len(npaths)) {
      gw <- gw + sxx_ga[r, s] * outer(lmat[, r], lmat[, s])
    }
  }
  gw <- (gw + t(gw)) / 2
  gsw <- plan$n_total * w_mat - w_mat %*% gw %*% w_mat
  gq0 <- 2 * (w_mat %*% sq_ga) + w_mat %*% crossprod(se, g_p)
  glm <- matrix(0, p1, npaths)
  for (r in seq_len(npaths)) {
    xq_ga <- q0 %*% sx_ga[[r]]
    for (s in seq_len(npaths)) {
      xq_ga <- xq_ga + sxx_ga[r, s] * lmat[, s]
    }
    glm[, r] <- 2 *
      as.numeric(w_mat %*% xq_ga) +
      as.numeric(w_mat %*% gp_sxe[[r]])
  }
  sq_gp <- q0 %*% colSums(nj * g_p)
  for (r in seq_len(npaths)) {
    sq_gp <- sq_gp + sx_gp[r] * lmat[, r]
  }
  gmy <- -as.numeric(w_mat %*% (sq_gp + 2 * colSums(se)))
  g_pmat <- numeric(0)
  if (nx > 0L) {
    mm <- q0 %*% crossprod(g_p, plan$sx)
    for (r in seq_len(npaths)) {
      sxx_r <- matrix(plan$sxx[, path_x[r], ], n_cl, nx)
      mm <- mm +
        outer(lmat[, r], as.numeric(crossprod(g_p[, path_zcol[r]], sxx_r)))
    }
    # One row per covariate, one column per outcome
    sxe_sum <- matrix(vapply(sxe, colSums, numeric(p1)), p1, nx)
    g_pmat <- as.numeric(-(w_mat %*% (mm + 2 * sxe_sum)))
  }
  g_cc <- numeric(0)
  g_me <- numeric(0)
  if (plan$nexo > 0L) {
    g_cc <- as.numeric(crossprod(g_d, ct$exo_c))
    g_me <- -as.numeric(t(imp$cc) %*% colSums(g_d))
  }
  c(
    as.numeric(gsw),
    gmy,
    g_pmat,
    as.numeric(glm),
    as.numeric(gq0),
    colSums(g_sv),
    colSums(g_d),
    g_cc,
    g_me
  )
}

# The implied pieces as one vector, in the order of lavaan's
# lav_mvn_cl_rs_phi_vec(). Between-only responses are outside these kernels,
# so their pieces are empty.
rs_phi_vec <- function(imp) {
  c(
    as.numeric(imp$sigma_w),
    imp$mu_y,
    as.numeric(imp$P),
    as.numeric(imp$lmat),
    as.numeric(imp$q0),
    as.numeric(imp$sigma.v),
    imp$mu.v,
    as.numeric(imp$cc),
    imp$mu.exo
  )
}

# The gradient of lavaan's fit function with respect to the packed free
# parameters, as lav_model_grad() returns it for a random-slope model, or NULL
# when it is not defined (the caller then falls back to lavaan). The
# derivative of the implied pieces takes lavaan's central differences.
rs_kernel_grad <- function(lavmodel_x, plan, h = 1e-6) {
  info <- plan$info
  imp0 <- lavaan___lav_mvn_cl_rs_implied(lavmodel = lavmodel_x, rs_info = info)
  if (is.null(imp0)) {
    return(NULL)
  }
  gphi <- rs_kernel_dphi(plan, imp0)
  if (is.null(gphi)) {
    return(NULL)
  }
  x0 <- lavaan::lav_model_get_parameters(lavmodel_x)
  jac <- matrix(0, length(gphi), length(x0))
  for (k in seq_along(x0)) {
    h_k <- h * max(1, abs(x0[k]))
    x_right <- x_left <- x0
    x_right[k] <- x0[k] + h_k
    x_left[k] <- x0[k] - h_k
    imp_r <- lavaan___lav_mvn_cl_rs_implied(
      lavmodel = lavaan::lav_model_set_parameters(lavmodel_x, x = x_right),
      rs_info = info
    )
    imp_l <- lavaan___lav_mvn_cl_rs_implied(
      lavmodel = lavaan::lav_model_set_parameters(lavmodel_x, x = x_left),
      rs_info = info
    )
    if (is.null(imp_r) || is.null(imp_l)) {
      return(NULL)
    }
    jac[, k] <- (rs_phi_vec(imp_r) - rs_phi_vec(imp_l)) / (2 * h_k)
  }
  as.numeric(gphi %*% jac) / (2 * plan$n_total)
}
