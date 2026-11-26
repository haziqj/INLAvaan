# Random-slope kernels: the log-likelihood of a two-level model with random
# slopes (lavaan's rv() modifier) and its gradient, for lavaan's closed-form
# route with no between-only responses (rs_kernel_ok()), with complete data
# or with missing values at level 1.
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
# With missing values at level 1, lavaan splits the data of each cluster into
# cells, one per missing-data pattern, each with the inverse of the block of
# Sigma_w that its pattern observes. A_j and p_j are then sums over the cells
# of cluster j. Here the cells of one pattern are worked together as the
# clusters are with complete data, pattern by pattern, and the gradient adds
# the within-level terms of every pattern.
#
# The implied pieces come from rs_implied() (R/random-slope-implied.R), and the
# derivative with respect to lavaan's free parameters goes through them by
# the exact chain rule. lavaan differences them numerically instead, so the
# two gradients agree to about 1e-10.

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
    identical(as.integer(rs$info$kz), 0L)
}

## ----- Plan ------------------------------------------------------------------

# The sums of a set of cells (the clusters with complete data, or the cells of
# one missing-data pattern), one cell per row, with their totals
rs_cell_set <- function(n, sy, sx, sxx, sxy, syy, nx, p1, obs, j1 = NULL) {
  n_c <- length(n)
  sxx_a <- array(0, dim = c(n_c, nx, nx))
  sxy_a <- array(0, dim = c(n_c, nx, p1))
  syy_total <- matrix(0, p1, p1)
  for (j in seq_len(n_c)) {
    sxx_a[j, , ] <- sxx[[j]]
    sxy_a[j, , ] <- sxy[[j]]
    syy_total <- syy_total + syy[[j]]
  }
  sy <- matrix(sy, n_c, p1)
  sx <- matrix(sx, n_c, nx)
  list(
    obs = obs,
    j1 = j1,
    nj = n,
    sy = sy,
    sx = sx,
    sxx = sxx_a,
    sxy = sxy_a,
    n_total = sum(n),
    sy_total = colSums(sy),
    sx_total = colSums(sx),
    sxx_total = matrix(apply(sxx_a, c(2L, 3L), sum), nx, nx),
    sxy_total = matrix(apply(sxy_a, c(2L, 3L), sum), nx, p1),
    syy_total = syy_total
  )
}

# The cell sets of the data and what the clusters share, built once per model.
# `ntotal` is lavaan's sample size, which scales its gradient. With missing
# data it also counts the cases whose outcomes are all missing.
rs_kernel_plan <- function(rs, ntotal = sum(rs$stats$cluster.size)) {
  st <- rs$stats
  info <- rs$info
  nx <- info$nx
  p1 <- info$p1
  sets <- if (is.null(st$mp)) {
    list(rs_cell_set(
      st$cluster.size,
      st$sy,
      st$sx,
      st$sxx,
      st$sxy,
      st$syy,
      nx,
      p1,
      obs = seq_len(p1)
    ))
  } else {
    lapply(st$mp$pattern, function(cell) {
      rs_cell_set(
        cell$n,
        cell$sy,
        cell$sx,
        cell$sxx,
        cell$sxy,
        cell$syy,
        nx,
        p1,
        obs = cell$o,
        j1 = cell$j1
      )
    })
  }
  list(
    info = info,
    n_cl = st$nclusters,
    sets = sets,
    ntotal = ntotal,
    exo_b = st$exo.b,
    path_x = info$path.tab$x.idx,
    path_z = info$path.tab$z.idx,
    nx = nx,
    p1 = p1,
    nexo = info$nexo.b
  )
}

## ----- Cell terms ------------------------------------------------------------

# The contributions to A_j and p_j of every cell of a set, and its residual
# sums, as lavaan's loops build them. `w_mat` is the inverse of the block of
# Sigma_w that the set observes, padded with zeros.
rs_cell_terms <- function(plan, set, imp, w_mat) {
  pv <- imp$pv
  p1 <- plan$p1
  nx <- plan$nx
  n_c <- length(set$nj)
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
  a <- tcrossprod(set$nj, as.numeric(crossprod(q0, wq0)))
  se <- set$sy - tcrossprod(set$nj, mu_y) - set$sx %*% t(pmat)
  p <- se %*% wq0
  sxe <- vector("list", nx)
  for (xi in seq_len(nx)) {
    sxe[[xi]] <- matrix(set$sxy[, xi, ], n_c, p1) -
      tcrossprod(set$sx[, xi], mu_y) -
      matrix(set$sxx[, xi, ], n_c, nx) %*% t(pmat)
  }
  for (i in seq_along(path_x)) {
    zi <- path_zcol[i]
    xi <- path_x[i]
    for (r in seq_len(pv)) {
      term <- q0twl[r, i] * set$sx[, xi]
      a[, rs_at(r, zi, pv)] <- a[, rs_at(r, zi, pv)] + term
      a[, rs_at(zi, r, pv)] <- a[, rs_at(zi, r, pv)] + term
    }
    for (k in seq_along(path_x)) {
      a[, rs_at(zi, path_zcol[k], pv)] <- a[, rs_at(zi, path_zcol[k], pv)] +
        ltwl[i, k] * set$sxx[, xi, path_x[k]]
    }
    p[, zi] <- p[, zi] + as.numeric(sxe[[xi]] %*% wl[, i])
  }
  list(a = a, p = p, se = se, sxe = sxe)
}

# The part of -2 log L of a set that does not depend on the cluster effects,
# from the totals of the set
rs_set_quad0 <- function(set, imp, w_mat) {
  pmat <- imp$P
  mu_y <- imp$mu_y
  wp <- w_mat %*% pmat
  mu_y_w <- as.numeric(w_mat %*% mu_y)
  sum(w_mat * set$syy_total) -
    2 * sum(mu_y_w * set$sy_total) +
    set$n_total * sum(mu_y_w * mu_y) -
    2 * sum(t(wp) * set$sxy_total) +
    2 * sum(as.numeric(mu_y_w %*% pmat) * set$sx_total) +
    sum(crossprod(pmat, wp) * set$sxx_total)
}

# The inverse of the block of Sigma_w that `obs` observes, padded with zeros,
# and its log-determinant, or NULL
rs_w_block <- function(sigma_w, obs) {
  ch <- tryCatch(
    chol(sigma_w[obs, obs, drop = FALSE]),
    error = function(e) NULL
  )
  if (is.null(ch)) {
    return(NULL)
  }
  inv <- matrix(0, nrow(sigma_w), ncol(sigma_w))
  inv[obs, obs] <- chol2inv(ch)
  list(inv = inv, logdet = 2 * sum(log(diag(ch))))
}

# A_j and p_j of every cluster (one cluster per row), with the cell terms and
# the inverses of every set, the 2 pi and log-determinant terms of Sigma_w and
# the parts that do not depend on the cluster effects. NULL when a block of
# Sigma_w has no Cholesky factor.
rs_cluster_terms <- function(plan, imp) {
  pv <- imp$pv
  n_cl <- plan$n_cl
  a <- matrix(0, n_cl, pv * pv)
  p <- matrix(0, n_cl, pv)
  ld <- quad0 <- 0
  per_set <- vector("list", length(plan$sets))
  for (k in seq_along(plan$sets)) {
    set <- plan$sets[[k]]
    w <- rs_w_block(imp$sigma_w, set$obs)
    if (is.null(w)) {
      return(NULL)
    }
    ct <- rs_cell_terms(plan, set, imp, w$inv)
    if (is.null(set$j1)) {
      a <- a + ct$a
      p <- p + ct$p
    } else {
      a[set$j1, ] <- a[set$j1, , drop = FALSE] + ct$a
      p[set$j1, ] <- p[set$j1, , drop = FALSE] + ct$p
    }
    ld <- ld + set$n_total * (w$logdet + length(set$obs) * log(2 * pi))
    quad0 <- quad0 + rs_set_quad0(set, imp, w$inv)
    per_set[[k]] <- list(w_mat = w$inv, ct = ct)
  }
  d <- matrix(imp$mu.v, n_cl, pv, byrow = TRUE)
  exo_c <- NULL
  if (plan$nexo > 0L) {
    exo_c <- sweep(plan$exo_b, 2L, imp$mu.exo)
    d <- d + exo_c %*% t(imp$cc)
  }
  list(
    a = a,
    p = p,
    d = d,
    exo_c = exo_c,
    ld = ld,
    quad0 = quad0,
    per_set = per_set
  )
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
  ct <- rs_cluster_terms(plan, imp)
  if (is.null(ct)) {
    return(-Inf)
  }
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
  m2ll <- ct$ld +
    sum(post$logdet) +
    sum(quad_d) -
    sum(post$quad) +
    ct$quad0
  -m2ll / 2
}

## ----- Gradient --------------------------------------------------------------

# The derivative of minus twice the log-likelihood with respect to each of the
# implied pieces of rs_implied(), every element taken on its own, as a named
# list. In the order of lav_mvn_cl_rs_phi_vec() (rs_dphi_vec()) it equals
# colSums() of lavaan's lav_mvn_cl_rs_dfphi(). NULL when it is not defined.
rs_kernel_dphi <- function(plan, imp) {
  pv <- imp$pv
  ct <- rs_cluster_terms(plan, imp)
  if (is.null(ct)) {
    return(NULL)
  }
  a <- ct$a
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

  ## ----- Within-level terms, set by set --------------------------------------
  gw_total <- NULL
  for (k in seq_along(plan$sets)) {
    set <- plan$sets[[k]]
    rows <- if (is.null(set$j1)) seq_len(plan$n_cl) else set$j1
    part <- rs_dphi_within(
      plan,
      set,
      imp,
      ct$per_set[[k]]$w_mat,
      ct$per_set[[k]]$ct,
      g_a[rows, , drop = FALSE],
      g_p[rows, , drop = FALSE]
    )
    gw_total <- if (is.null(gw_total)) {
      part
    } else {
      Map(`+`, gw_total, part)
    }
  }
  g_cc <- numeric(0)
  g_me <- numeric(0)
  if (plan$nexo > 0L) {
    g_cc <- as.numeric(crossprod(g_d, ct$exo_c))
    g_me <- -as.numeric(t(imp$cc) %*% colSums(g_d))
  }
  list(
    sigma_w = gw_total$gsw,
    mu_y = gw_total$gmy,
    P = matrix(gw_total$g_pmat, plan$p1, plan$nx),
    lmat = gw_total$glm,
    q0 = gw_total$gq0,
    sigma.v = matrix(colSums(g_sv), pv, pv),
    mu.v = colSums(g_d),
    cc = matrix(g_cc, pv, plan$nexo),
    mu.exo = g_me
  )
}

# The derivatives with respect to Sigma_w, mu_y, P, L and Q_0 of one set of
# cells, from the posterior moments of the clusters of its cells (`g_a`,
# `g_p`, one cell per row). The sums over the cells come first, as vector
# operations, and the p1 x p1 algebra is done once.
rs_dphi_within <- function(plan, set, imp, w_mat, ct, g_a, g_p) {
  pv <- imp$pv
  p1 <- plan$p1
  nx <- plan$nx
  n_c <- length(set$nj)
  nj <- set$nj
  path_x <- plan$path_x
  path_zcol <- imp$z.v.idx[plan$path_z]
  npaths <- length(path_x)
  lmat <- imp$lmat
  q0 <- imp$q0
  pmat <- imp$P
  mu_y <- imp$mu_y
  se <- ct$se
  sxe <- ct$sxe
  at <- function(i, j) rs_at(i, j, pv)

  ## ----- Sums over the cells -------------------------------------------------
  ga_n <- matrix(colSums(nj * g_a), pv, pv)
  sx_ga <- lapply(seq_len(npaths), function(r) {
    colSums(
      set$sx[, path_x[r]] * g_a[, at(seq_len(pv), path_zcol[r]), drop = FALSE]
    )
  })
  sxx_ga <- matrix(0, npaths, npaths)
  for (r in seq_len(npaths)) {
    for (s in seq_len(npaths)) {
      sxx_ga[r, s] <- sum(
        set$sxx[, path_x[r], path_x[s]] * g_a[, at(path_zcol[r], path_zcol[s])]
      )
    }
  }
  gp_se <- crossprod(g_p, se)
  gp_sxe <- lapply(seq_len(npaths), function(r) {
    as.numeric(crossprod(g_p[, path_zcol[r]], sxe[[path_x[r]]]))
  })
  sx_gp <- vapply(
    seq_len(npaths),
    function(r) sum(set$sx[, path_x[r]] * g_p[, path_zcol[r]]),
    numeric(1)
  )
  psx <- pmat %*% set$sx_total
  see <- set$syy_total -
    outer(set$sy_total, mu_y) -
    outer(mu_y, set$sy_total) +
    set$n_total * tcrossprod(mu_y) -
    t(set$sxy_total) %*% t(pmat) -
    pmat %*% set$sxy_total +
    mu_y %*% t(psx) +
    psx %*% t(mu_y) +
    pmat %*% set$sxx_total %*% t(pmat)

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
  gsw <- set$n_total * w_mat - w_mat %*% gw %*% w_mat
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
    mm <- q0 %*% crossprod(g_p, set$sx)
    for (r in seq_len(npaths)) {
      sxx_r <- matrix(set$sxx[, path_x[r], ], n_c, nx)
      mm <- mm +
        outer(lmat[, r], as.numeric(crossprod(g_p[, path_zcol[r]], sxx_r)))
    }
    # One row per covariate, one column per outcome
    sxe_sum <- matrix(vapply(sxe, colSums, numeric(p1)), p1, nx)
    g_pmat <- as.numeric(-(w_mat %*% (mm + 2 * sxe_sum)))
  }
  list(gsw = gsw, gmy = gmy, g_pmat = g_pmat, glm = glm, gq0 = gq0)
}

# The derivative of rs_kernel_dphi() as one vector, in the order of lavaan's
# lav_mvn_cl_rs_phi_vec(). Between-only responses are outside these kernels,
# so their pieces are empty.
rs_dphi_vec <- function(g) {
  c(
    as.numeric(g$sigma_w),
    g$mu_y,
    as.numeric(g$P),
    as.numeric(g$lmat),
    as.numeric(g$q0),
    as.numeric(g$sigma.v),
    g$mu.v,
    as.numeric(g$cc),
    g$mu.exo
  )
}

# The gradient of lavaan's fit function with respect to the packed free
# parameters, as lav_model_grad() returns it for a random-slope model, or NULL
# when it is not defined (the caller then falls back to lavaan). The chain
# rule through the implied pieces is exact (rs_implied_back()).
rs_kernel_grad <- function(lavmodel_x, plan) {
  steps <- rs_implied_steps(lavmodel_x, plan$info)
  if (is.null(steps)) {
    return(NULL)
  }
  g <- rs_kernel_dphi(plan, steps$imp)
  if (is.null(g)) {
    return(NULL)
  }
  rs_implied_back(lavmodel_x, steps, g) / (2 * plan$ntotal)
}
