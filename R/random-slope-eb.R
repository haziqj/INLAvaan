# Empirical Bayes values of the latent variables of a random-slope model, as
# lavaan's lav_mvn_cl_rs_eb() gives them for predict(): the posterior means
# (and SDs) of the level-2 latent variables given each cluster's data, and the
# conditional means of the level-1 latent variables given each row and its
# cluster's posterior mean. At level 2 the posterior of each cluster comes
# from the random-slope kernels (R/random-slope-kernels.R), all clusters at
# once. Level 1 is lavaan's lav_mvn_cl_rs_eb_core() step. On the quadrature
# route each node gives these values and the nodes are mixed by their
# posterior weights, as in lav_mvn_cl_rs_eb_nl().

# `lavcache` is the fit's lavaan cache. Returns l1, l2, cluster.idx and with
# `se = TRUE` se2.
rs_eb <- function(lavmodel, lavdata, lavcache, se = FALSE) {
  rs <- lavcache[[1L]]$rs
  plan <- rs_any_plan(rs)
  if (!is.null(plan$quad)) {
    return(rs_eb_quad(plan, lavmodel, lavdata, rs, se))
  }
  imp <- rs_implied(lavmodel, rs$info)
  if (is.null(imp)) {
    cli_abort("The model-implied matrices could not be computed.")
  }
  core <- rs_eb_core(plan, lavmodel, lavmodel@GLIST, lavdata, rs$info, imp, se)
  meta_seq <- seq_len(imp$meta)
  l2 <- core$mu0[, meta_seq, drop = FALSE]
  colnames(l2) <- imp$eta.names
  out <- list(l1 = core$l1, l2 = l2, cluster.idx = core$cluster.idx)
  if (se) {
    se2 <- sqrt(pmax(core$var0[, meta_seq, drop = FALSE], 0))
    colnames(se2) <- imp$eta.names
    out$se2 <- se2
  }
  out
}

# The posterior mean and variance of the cluster effects of every cluster, and
# the level-1 values, for the route-A problem described by `info` and `plan`
rs_eb_core <- function(plan, lavmodel, glist, lavdata, info, imp, se) {
  pv <- imp$pv
  ct <- rs_cluster_terms(plan, imp)
  if (is.null(ct)) {
    cli_abort("The within covariance matrix is not positive definite.")
  }
  p_tilde <- ct$p - rs_batch_mv(ct$a, ct$d, pv)
  post <- rs_cluster_posterior(ct$a, p_tilde, imp$sigma.v, pv, s0 = TRUE)
  if (post$status != "ok") {
    cli_abort("The posterior of the cluster effects is not defined here.")
  }
  mu0 <- ct$d + rs_batch_mv(post$s0, p_tilde, pv)
  var0 <- if (se) post$s0[, rs_at(seq_len(pv), seq_len(pv), pv), drop = FALSE]

  ## ----- Level 1 ---------------------------------------------------------------
  nmat <- lavmodel@nmat
  mm_w <- seq_len(nmat[1L])
  mw <- glist[mm_w]
  dn_w <- lavmodel@dimNames[mm_w][[which(names(mw) == "lambda")]]
  ov_w <- dn_w[[1L]]
  lv_w <- dn_w[[2L]]
  y_idx <- match(info$y.names, ov_w)
  x_lv_idx <- match(info$x.names, lv_w)
  nlv_w <- ncol(mw$lambda)
  ib_w <- if (is.null(mw$beta)) diag(nlv_w) else solve(diag(nlv_w) - mw$beta)
  psi_w0 <- mw$psi
  alpha_w0 <- if (is.null(mw$alpha)) matrix(0, nlv_w, 1L) else mw$alpha
  if (length(x_lv_idx) > 0L) {
    psi_w0[x_lv_idx, ] <- 0
    psi_w0[, x_lv_idx] <- 0
    alpha_w0[x_lv_idx, 1L] <- 0
  }
  real_idx <- which(!lv_w %in% lavdata@ov.names[[1L]])
  y1 <- lavdata@X[[1L]]
  cluster_idx <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  if (length(real_idx) == 0L) {
    return(list(
      mu0 = mu0,
      var0 = var0,
      l1 = matrix(0, nrow(y1), 0L),
      cluster.idx = cluster_idx
    ))
  }
  path_tab <- info$path.tab
  npaths <- nrow(path_tab)
  path_zcol <- imp$z.v.idx[path_tab$z.idx]
  path_x <- path_tab$x.idx
  yy <- y1[, info$y.data.idx, drop = FALSE]
  xx <- y1[, info$x.data.idx, drop = FALSE]
  n1 <- nrow(y1)
  vhat <- mu0[cluster_idx, , drop = FALSE]
  yhat <- matrix(imp$mu_y, n1, info$p1, byrow = TRUE) +
    xx %*% t(imp$P) +
    vhat %*% t(imp$q0)
  zx <- matrix(0, n1, npaths)
  for (i in seq_len(npaths)) {
    zx[, i] <- vhat[, path_zcol[i]] * xx[, path_x[i]]
    yhat <- yhat + outer(zx[, i], imp$lmat[, i])
  }
  e_mat <- yy - yhat
  t_inp <- matrix(alpha_w0[, 1L], n1, nlv_w, byrow = TRUE)
  if (length(x_lv_idx) > 0L) {
    t_inp[, x_lv_idx] <- t_inp[, x_lv_idx, drop = FALSE] + xx
  }
  for (i in seq_len(npaths)) {
    lhs_col <- match(path_tab$lhs[i], lv_w)
    if (!is.na(lhs_col)) {
      t_inp[, lhs_col] <- t_inp[, lhs_col] + zx[, i]
    }
  }
  m_full <- t_inp %*% t(ib_w)
  veta0 <- ib_w %*% psi_w0 %*% t(ib_w)
  ceta_y <- (veta0 %*% t(mw$lambda[y_idx, , drop = FALSE]))[
    real_idx,
    ,
    drop = FALSE
  ]
  obs <- !is.na(yy)
  upd <- matrix(0, n1, length(real_idx))
  case_id <- apply(1L * obs, 1L, paste, collapse = "")
  for (pid in unique(case_id)) {
    ridx <- which(case_id == pid)
    o <- which(obs[ridx[1L], ])
    if (length(o) == 0L) {
      next
    }
    w_o <- chol2inv(chol(imp$sigma_w[o, o, drop = FALSE]))
    upd[ridx, ] <- e_mat[ridx, o, drop = FALSE] %*%
      (w_o %*% t(ceta_y[, o, drop = FALSE]))
  }
  l1 <- m_full[, real_idx, drop = FALSE] + upd
  colnames(l1) <- lv_w[real_idx]
  list(mu0 = mu0, var0 = var0, l1 = l1, cluster.idx = cluster_idx)
}

# The quadrature route: each node's values, mixed by the posterior weights of
# the nodes
rs_eb_quad <- function(plan, lavmodel, lavdata, rs, se) {
  info <- rs$info
  cond <- rs_cond(lavmodel, info, rs$stats$exo.b)
  if (is.null(cond)) {
    cli_abort("The model-implied matrices could not be computed.")
  }
  hmat <- rs_quad_hmat(plan, cond)
  if (any(!is.finite(apply(hmat, 1L, max)))) {
    cli_abort(
      "The quadrature log-likelihood could not be computed for one or more
       clusters."
    )
  }
  pw <- exp(hmat - rs_quad_lse(hmat))
  n_cl <- plan$n_cl
  beta_cells <- cbind(info$nl.beta.row, info$nl.beta.col)
  path_tab <- info$path.tab
  path_nl_z <- match(
    path_tab$z.idx[which(path_tab$type == "lv")],
    info$nl.z.idx
  )
  pv_c <- length(cond$lin.v)
  m_lin <- matrix(0, n_cl, pv_c)
  v_lin <- if (se) matrix(0, n_cl, pv_c)
  l1_mix <- NULL
  glist_i <- lavmodel@GLIST
  for (i in seq_len(cond$nnodes)) {
    if (is.null(cond$imp[[i]])) {
      next
    }
    glist_i[[info$nl.beta.mm]][beta_cells] <- cond$bmat[, i][path_nl_z]
    core <- rs_eb_core(
      plan,
      lavmodel,
      glist_i,
      lavdata,
      plan$info,
      cond$imp[[i]],
      se
    )
    m_lin <- m_lin + pw[, i] * core$mu0
    if (se) {
      v_lin <- v_lin + pw[, i] * (core$var0 + core$mu0^2)
    }
    if (is.null(l1_mix)) {
      l1_mix <- matrix(0, nrow(core$l1), ncol(core$l1))
      colnames(l1_mix) <- colnames(core$l1)
      cluster_idx <- core$cluster.idx
    }
    if (ncol(l1_mix) > 0L) {
      l1_mix <- l1_mix + pw[cluster_idx, i] * core$l1
    }
  }
  if (se) {
    v_lin <- v_lin - m_lin^2
  }
  bt <- t(cond$bmat)
  m_nl <- pw %*% bt
  v_nl <- if (se) pw %*% bt^2 - m_nl^2
  l2 <- matrix(0, n_cl, cond$meta)
  colnames(l2) <- cond$eta.names
  se2 <- if (se) l2
  for (m in seq_len(cond$meta)) {
    lin_pos <- match(m, cond$lin.v)
    if (!is.na(lin_pos)) {
      l2[, m] <- m_lin[, lin_pos]
      if (se) {
        se2[, m] <- sqrt(pmax(v_lin[, lin_pos], 0))
      }
    } else {
      nl_pos <- match(m, cond$nl.v)
      l2[, m] <- m_nl[, nl_pos]
      if (se) {
        se2[, m] <- sqrt(pmax(v_nl[, nl_pos], 0))
      }
    }
  }
  out <- list(l1 = l1_mix, l2 = l2, cluster.idx = cluster_idx)
  if (se) {
    out$se2 <- se2
  }
  out
}
