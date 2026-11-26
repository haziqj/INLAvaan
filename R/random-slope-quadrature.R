# Random slopes on a latent or split covariate (lavaan's route B). The slope
# multiplies a latent variable, so the model is not linear in the cluster
# effects and has no closed form. lavaan integrates the slopes that act on
# latent variables by Gauss-Hermite quadrature: at each node the slopes are
# fixed, the other cluster effects integrate out in closed form (route A), and
# the likelihood of a cluster is the weighted sum over the nodes
# (lav_mvn_cl_rs_loglik_nl()). Here each node is scored with INLAvaan's own
# random-slope kernels, which work all clusters at once, and the gradient
# weights each cluster's route-A gradient at a node by the posterior weight of
# that node. The conditional pieces at the nodes come from rs_cond(), a port
# of lavaan's lav_mvn_cl_rs_cond(), and their derivative with respect to
# lavaan's parameters is taken by central differences, as lavaan does.

## ----- Nodes and conditional pieces --------------------------------------------

# Gauss-Hermite nodes and weights for a standard normal density, as lavaan's
# lav_integration_gauss_hermite(n, dnorm = TRUE) gives them
rs_gh_nodes <- function(n) {
  n <- as.integer(n)
  if (n == 1L) {
    x <- 0
    w <- sqrt(pi)
  } else {
    jn <- matrix(0, n, n)
    jn[cbind(seq_len(n - 1L) + 1L, seq_len(n - 1L))] <- sqrt(
      seq_len(n - 1L) / 2
    )
    ev <- eigen(jn, symmetric = TRUE)
    x <- ev$values
    w <- sqrt(pi) * ev$vectors[1L, ]^2
  }
  list(x = -x * sqrt(2), w = w / sqrt(pi))
}

# The conditional route-A pieces at every node, the log-weights of the nodes
# and, with between-level covariates, the per-cluster correction of the
# weights. NULL where lavaan's map is not defined.
rs_cond <- function(lavmodel, info, exo_b, glist = lavmodel@GLIST) {
  imp0 <- rs_implied(lavmodel, info, glist)
  if (is.null(imp0)) {
    return(NULL)
  }
  path_tab <- info$path.tab
  nl_z <- info$nl.z.idx
  nl_v <- imp0$z.v.idx[nl_z]
  lin_v <- setdiff(seq_len(imp0$pv), nl_v)
  s_nl <- imp0$sigma.v[nl_v, nl_v, drop = FALSE]
  mu_nl <- imp0$mu.v[nl_v]
  if (anyNA(s_nl)) {
    return(NULL)
  }
  ch <- tryCatch(chol(s_nl), error = function(e) NULL)
  if (is.null(ch)) {
    return(NULL)
  }
  s_nl_inv <- chol2inv(ch)
  q_nl <- length(nl_v)
  case_b <- info$nexo.b > 0L
  delta <- NULL
  if (case_b) {
    wc <- exo_b - rep(imp0$mu.exo, each = nrow(exo_b))
    delta <- wc %*% t(imp0$cc[nl_v, , drop = FALSE])
  }
  r_mat <- imp0$sigma.v[lin_v, nl_v, drop = FALSE] %*% s_nl_inv
  sigma_v_c <- imp0$sigma.v[lin_v, lin_v, drop = FALSE] -
    r_mat %*% imp0$sigma.v[nl_v, lin_v, drop = FALSE]
  sigma_v_c <- (sigma_v_c + t(sigma_v_c)) / 2
  cc_c <- imp0$cc[lin_v, , drop = FALSE]
  if (case_b) {
    cc_c <- cc_c - r_mat %*% imp0$cc[nl_v, , drop = FALSE]
  }
  ov_rows <- which(path_tab$type == "ov")
  z_keep <- which(info$z.type == "ov")
  z_v_idx_c <- match(imp0$z.v.idx[z_keep], lin_v)
  gh <- rs_gh_nodes(if (is.null(info$ngh)) 21L else info$ngh)
  if (q_nl == 1L) {
    tmat <- matrix(gh$x, nrow = 1L)
    logw <- log(gh$w)
  } else {
    tmat <- t(as.matrix(expand.grid(rep(list(gh$x), q_nl))))
    logw <- rowSums(as.matrix(expand.grid(rep(list(log(gh$w)), q_nl))))
  }
  nnodes <- ncol(tmat)
  if (case_b) {
    grid_c <- mu_nl + colMeans(delta)
    dc <- delta - rep(colMeans(delta), each = nrow(delta))
    s_grid <- s_nl + crossprod(dc) / nrow(delta)
    a_up <- tryCatch(chol(s_grid), error = function(e) NULL)
    if (is.null(a_up)) {
      return(NULL)
    }
  } else {
    grid_c <- mu_nl
    a_up <- ch
  }
  bmat <- grid_c + t(a_up) %*% tmat
  logr <- NULL
  if (case_b) {
    mu_mat <- rep(mu_nl, each = nrow(delta)) + delta
    mb <- s_nl_inv %*% bmat
    bmb <- colSums(bmat * mb)
    mm <- rowSums((mu_mat %*% s_nl_inv) * mu_mat)
    cross <- mu_mat %*% mb
    tt <- colSums(tmat * tmat)
    logr <- -0.5 *
      (outer(mm, bmb, "+") - 2 * cross) -
      0.5 * 2 * sum(log(diag(ch))) +
      sum(log(diag(a_up))) +
      rep(0.5 * tt, each = nrow(delta))
  }
  nl_path_rows <- which(path_tab$type == "lv")
  path_nl_z <- match(path_tab$z.idx[nl_path_rows], nl_z)
  beta_cells <- cbind(info$nl.beta.row, info$nl.beta.col)
  imp_list <- vector("list", nnodes)
  glist_i <- glist
  for (i in seq_len(nnodes)) {
    b_i <- bmat[, i]
    glist_i[[info$nl.beta.mm]][beta_cells] <- b_i[path_nl_z]
    imp_i <- rs_implied(lavmodel, info, glist_i)
    if (is.null(imp_i)) {
      next
    }
    imp_list[[i]] <- list(
      sigma_w = imp_i$sigma_w,
      mu_y = imp_i$mu_y,
      mu_y_b = imp_i$mu_y_b,
      P = imp_i$P,
      lmat = imp_i$lmat[, ov_rows, drop = FALSE],
      q0 = imp_i$q0[, lin_v, drop = FALSE],
      z.v.idx = z_v_idx_c,
      pv = length(lin_v),
      sigma.v = sigma_v_c,
      mu.v = imp0$mu.v[lin_v] + as.numeric(r_mat %*% (b_i - mu_nl)),
      cc = cc_c,
      mu.exo = imp0$mu.exo,
      mu_z = imp_i$mu_z,
      gmat = imp_i$gmat[, lin_v, drop = FALSE],
      sigma.z = imp_i$sigma.z
    )
  }
  list(imp = imp_list, logw = logw, nnodes = nnodes, logr = logr)
}

# The description of the route-A problem at a node: the paths on observed
# covariates only, and the slopes on them. It does not depend on the
# parameters.
rs_cond_info <- function(info) {
  path_tab <- info$path.tab
  ov_rows <- which(path_tab$type == "ov")
  z_keep <- which(info$z.type == "ov")
  path_tab_c <- path_tab[ov_rows, , drop = FALSE]
  path_tab_c$z.idx <- match(path_tab$rv[ov_rows], info$z.names[z_keep])
  info_c <- info
  info_c$path.tab <- path_tab_c
  info_c$z.names <- info$z.names[z_keep]
  info_c$q <- length(z_keep)
  info_c$nl.flag <- FALSE
  info_c
}

## ----- Plan ------------------------------------------------------------------

# The route-A plan of the problem at a node, with what the quadrature needs
rs_quad_plan <- function(rs, ntotal) {
  info <- rs$info
  plan <- rs_kernel_plan(
    list(info = rs_cond_info(info), stats = rs$stats),
    ntotal
  )
  plan$quad <- list(info = info, exo_b = rs$stats$exo.b)
  plan
}

## ----- Log-likelihood and gradient -----------------------------------------------

# The log-likelihood of every cluster at every node plus the log-weight of the
# node, one column per node, from the conditional pieces `cond`. NULL sends the
# call back to lavaan.
rs_quad_hmat <- function(plan, cond) {
  hmat <- matrix(-Inf, plan$n_cl, cond$nnodes)
  for (i in seq_len(cond$nnodes)) {
    if (is.null(cond$imp[[i]])) {
      next
    }
    ll_i <- rs_kernel_loglik(plan, cond$imp[[i]], per_cluster = TRUE)
    if (is.null(ll_i)) {
      return(NULL)
    }
    if (length(ll_i) == 1L && !is.finite(ll_i)) {
      next
    }
    hmat[, i] <- ll_i + cond$logw[i]
  }
  if (!is.null(cond$logr)) {
    hmat <- hmat + cond$logr
  }
  hmat
}

# Log-sum-exp over the nodes, one value per cluster
rs_quad_lse <- function(hmat) {
  hmax <- apply(hmat, 1L, max)
  hmax + log(rowSums(exp(hmat - hmax)))
}

# The log-likelihood with the 2 pi terms, as lavaan's lav_mvn_cl_rs_m2ll()
# returns it with `log2pi = TRUE, minus_two = FALSE` on route B, -Inf where
# lavaan gives +Inf, or NULL to go back to lavaan
rs_quad_loglik <- function(plan, lavmodel_x, per_cluster = FALSE) {
  cond <- rs_cond(lavmodel_x, plan$quad$info, plan$quad$exo_b)
  if (is.null(cond)) {
    return(-Inf)
  }
  hmat <- rs_quad_hmat(plan, cond)
  if (is.null(hmat)) {
    return(NULL)
  }
  if (any(!is.finite(apply(hmat, 1L, max)))) {
    return(-Inf)
  }
  ll_j <- rs_quad_lse(hmat)
  if (per_cluster) ll_j else sum(ll_j)
}

# The gradient of lavaan's fit function with respect to the packed free
# parameters on route B, as lav_model_grad() returns it, or NULL to go back
# to lavaan. At each node, each cluster's gradient with respect to the
# conditional pieces is weighted by the posterior weight of the node. The
# derivative of the conditional pieces (and of the weight correction) with
# respect to the parameters is taken by central differences.
rs_quad_grad <- function(lavmodel_x, plan, h = 1e-6) {
  info <- plan$quad$info
  exo_b <- plan$quad$exo_b
  cond0 <- rs_cond(lavmodel_x, info, exo_b)
  if (is.null(cond0) || any(vapply(cond0$imp, is.null, logical(1)))) {
    return(NULL)
  }
  hmat <- rs_quad_hmat(plan, cond0)
  if (is.null(hmat) || any(!is.finite(apply(hmat, 1L, max)))) {
    return(NULL)
  }
  pw <- exp(hmat - rs_quad_lse(hmat))
  dphi <- vector("list", cond0$nnodes)
  for (i in seq_len(cond0$nnodes)) {
    g_i <- rs_kernel_dphi(plan, cond0$imp[[i]], weights = pw[, i])
    if (is.null(g_i)) {
      return(NULL)
    }
    dphi[[i]] <- rs_dphi_vec(g_i)
  }
  # S(x) = sum over the nodes of dphi_i . phi_i(x), and with between-level
  # covariates -2 sum over the clusters and nodes of pw * logr(x), so that
  # its central difference is the chain rule
  s_of <- function(x) {
    cond <- rs_cond(
      lavaan::lav_model_set_parameters(lavmodel_x, x),
      info,
      exo_b
    )
    if (is.null(cond) || any(vapply(cond$imp, is.null, logical(1)))) {
      return(NA_real_)
    }
    out <- 0
    for (i in seq_len(cond$nnodes)) {
      out <- out + sum(dphi[[i]] * rs_imp_phi_vec(cond$imp[[i]]))
    }
    if (!is.null(cond$logr)) {
      out <- out - 2 * sum(pw * cond$logr)
    }
    out
  }
  x0 <- lavaan::lav_model_get_parameters(lavmodel_x)
  dx <- numeric(length(x0))
  for (k in seq_along(x0)) {
    h_k <- h * max(1, abs(x0[k]))
    x_right <- x_left <- x0
    x_right[k] <- x0[k] + h_k
    x_left[k] <- x0[k] - h_k
    dx[k] <- (s_of(x_right) - s_of(x_left)) / (2 * h_k)
  }
  if (anyNA(dx)) {
    return(NULL)
  }
  dx / (2 * plan$ntotal)
}

# The conditional pieces as one vector, in the order of rs_dphi_vec()
rs_imp_phi_vec <- function(imp) {
  c(
    as.numeric(imp$sigma_w),
    imp$mu_y,
    as.numeric(imp$P),
    as.numeric(imp$lmat),
    as.numeric(imp$q0),
    as.numeric(imp$sigma.v),
    imp$mu.v,
    as.numeric(imp$cc),
    imp$mu.exo,
    imp$mu_z,
    as.numeric(imp$gmat),
    as.numeric(imp$sigma.z)
  )
}
