# The implied pieces of a random-slope model and their exact derivative.
#
# The random-slope kernels (R/random-slope-kernels.R) score the data from a
# set of implied pieces: Sigma_w, mu_y, P, L and Q_0 at level 1, and
# Sigma_v, mu_v, C and mu_exo at level 2. rs_implied() builds them from the
# model matrices of the two levels, as lavaan's lav_mvn_cl_rs_implied() does,
# with the same arithmetic. rs_implied_back() takes the derivative of a
# function of the pieces with respect to each piece and returns its derivative
# with respect to lavaan's free parameters, by the chain rule through the same
# steps in reverse. lavaan instead differences the pieces numerically, two
# calls of lav_mvn_cl_rs_implied() per free parameter.

## ----- Forward ---------------------------------------------------------------

# The inverse of a symmetric matrix as lavaan's lav_mat_sym_inverse() takes it
# (eigen decomposition, closed forms for 1 x 1 and 2 x 2, zero rows and
# columns left out, rescaled when the diagonal spans more than 1e6)
rs_sym_inverse <- function(s) {
  zero_idx <- which(rowSums(abs(s)) == 0)
  if (length(zero_idx) > 0L) {
    s <- s[-zero_idx, -zero_idx, drop = FALSE]
  }
  p <- NCOL(s)
  pre_d <- NULL
  if (p > 2L) {
    d_pre <- sqrt(abs(diag(s)))
    d_pre[!is.finite(d_pre) | d_pre < .Machine$double.eps] <- 1
    if (max(d_pre) / min(d_pre) > 1e6) {
      pre_d <- d_pre
      s <- s / tcrossprod(d_pre)
    }
  }
  if (p == 0L) {
    s_inv <- matrix(0, 0L, 0L)
  } else if (p == 1L) {
    s_inv <- matrix(1 / s[1L, 1L], 1L, 1L)
  } else if (p == 2L) {
    tmp <- s[1L, 1L] * s[2L, 2L] - s[1L, 2L] * s[2L, 1L]
    s_inv <- if (tmp == 0) {
      matrix(Inf, 2L, 2L)
    } else {
      matrix(c(s[2L, 2L], -s[2L, 1L], -s[1L, 2L], s[1L, 1L]) / tmp, 2L, 2L)
    }
  } else {
    ev <- eigen(s, symmetric = TRUE)
    s_inv <- tcrossprod(
      ev$vectors / rep(ev$values, each = length(ev$values)),
      ev$vectors
    )
  }
  if (!is.null(pre_d)) {
    s_inv <- s_inv / tcrossprod(pre_d)
  }
  if (length(zero_idx) > 0L) {
    out <- matrix(0, p + length(zero_idx), p + length(zero_idx))
    out[-zero_idx, -zero_idx] <- s_inv
    s_inv <- out
  }
  s_inv
}

# The steps of the map, with what the reverse pass needs. NULL where lavaan's
# map is not defined (a singular I - B).
rs_implied_steps <- function(lavmodel, info, glist = lavmodel@GLIST) {
  nmat <- lavmodel@nmat

  ## ----- Level 1 ---------------------------------------------------------------
  mm_w <- seq_len(nmat[1L])
  mw <- glist[mm_w]
  ov_w <- lavmodel@dimNames[mm_w][[which(names(mw) == "lambda")]][[1L]]
  lv_w <- lavmodel@dimNames[mm_w][[which(names(mw) == "lambda")]][[2L]]
  y_idx <- match(info$y.names, ov_w)
  x_lv_idx <- match(info$x.names, lv_w)
  nlv_w <- ncol(mw$lambda)
  ib_w <- diag(nlv_w)
  if (!is.null(mw$beta)) {
    ib_w <- tryCatch(solve(diag(nlv_w) - mw$beta), error = function(e) NULL)
    if (is.null(ib_w)) {
      return(NULL)
    }
  }
  rf_w <- mw$lambda %*% ib_w
  psi_w0 <- mw$psi
  alpha_w0 <- if (is.null(mw$alpha)) matrix(0, nlv_w, 1L) else mw$alpha
  if (length(x_lv_idx) > 0L) {
    psi_w0[x_lv_idx, ] <- 0
    psi_w0[, x_lv_idx] <- 0
    alpha_w0[x_lv_idx, 1L] <- 0
  }
  rf_y <- rf_w[y_idx, , drop = FALSE]
  sigma_w <- rf_y %*% psi_w0 %*% t(rf_y) + mw$theta[y_idx, y_idx, drop = FALSE]
  nu_w0 <- if (is.null(mw$nu)) matrix(0, nrow(mw$lambda), 1L) else mw$nu
  mu_w <- as.numeric(nu_w0[y_idx, 1L] + rf_y %*% alpha_w0)
  pmat <- rf_y[, x_lv_idx, drop = FALSE]
  path_tab <- info$path.tab
  npaths <- nrow(path_tab)
  lmat <- matrix(0, info$p1, npaths)
  lhs_lv <- match(path_tab$lhs, lv_w)
  for (i in seq_len(npaths)) {
    if (is.na(lhs_lv[i])) {
      lmat[match(path_tab$lhs[i], info$y.names), i] <- 1
    } else {
      lmat[, i] <- rf_y[, lhs_lv[i]]
    }
  }

  ## ----- Level 2 ---------------------------------------------------------------
  mm_b <- seq_len(nmat[2L]) + nmat[1L]
  mb <- glist[mm_b]
  dn_b <- lavmodel@dimNames[mm_b][[which(names(mb) == "lambda")]]
  ov_b <- dn_b[[1L]]
  lv_b <- dn_b[[2L]]
  yb_idx <- match(info$yb.names, ov_b)
  z_lv_idx <- match(info$z.names, lv_b)
  exo_ov_idx <- match(info$exo.b.names, ov_b)
  nlv_b <- ncol(mb$lambda)
  ib_b <- diag(nlv_b)
  if (!is.null(mb$beta)) {
    ib_b <- tryCatch(solve(diag(nlv_b) - mb$beta), error = function(e) NULL)
    if (is.null(ib_b)) {
      return(NULL)
    }
  }
  e_eta <- if (is.null(mb$alpha)) {
    matrix(0, nlv_b, 1L)
  } else {
    ib_b %*% mb$alpha
  }
  v_eta <- ib_b %*% mb$psi %*% t(ib_b)
  nu_b0 <- if (is.null(mb$nu)) matrix(0, nrow(mb$lambda), 1L) else mb$nu
  nexo <- info$nexo.b
  neps <- info$neps
  exo_lv_idx <- match(info$exo.b.names, lv_b)
  eta_idx <- if (nexo > 0L) seq_len(nlv_b)[-exo_lv_idx] else seq_len(nlv_b)
  meta <- length(eta_idx)
  pv <- meta + neps
  v_ee_inv <- NULL
  if (nexo > 0L) {
    v_ee <- v_eta[exo_lv_idx, exo_lv_idx, drop = FALSE]
    v_ee_inv <- rs_sym_inverse(v_ee)
    cc_eta <- v_eta[eta_idx, exo_lv_idx, drop = FALSE] %*% v_ee_inv
    v_eta_c <- v_eta[eta_idx, eta_idx, drop = FALSE] -
      cc_eta %*% v_eta[exo_lv_idx, eta_idx, drop = FALSE]
    v_eta_c <- (v_eta_c + t(v_eta_c)) / 2
    mu_exo <- as.numeric(nu_b0[exo_ov_idx, 1L] + e_eta[exo_lv_idx, 1L])
  } else {
    cc_eta <- matrix(0, meta, 0L)
    v_eta_c <- v_eta
    mu_exo <- numeric(0L)
  }
  sigma_v <- matrix(0, pv, pv)
  sigma_v[seq_len(meta), seq_len(meta)] <- v_eta_c
  mu_v <- numeric(pv)
  mu_v[seq_len(meta)] <- e_eta[eta_idx, 1L]
  cc <- matrix(0, pv, nexo)
  cc[seq_len(meta), ] <- cc_eta
  eps_b_idx <- integer(0L)
  if (neps > 0L) {
    eps_b_idx <- match(info$eps.names, ov_b)
    eps_pos <- meta + seq_len(neps)
    sigma_v[eps_pos, eps_pos] <- mb$theta[eps_b_idx, eps_b_idx, drop = FALSE]
  }
  q0 <- matrix(0, info$p1, pv)
  yb_y_idx <- info$yb.y.idx
  q0[yb_y_idx, seq_len(meta)] <- mb$lambda[yb_idx, eta_idx, drop = FALSE]
  if (neps > 0L) {
    eps_y_idx <- match(info$eps.names, info$y.names)
    for (i in seq_len(neps)) {
      q0[eps_y_idx[i], meta + i] <- 1
    }
  }
  mu_y_b <- numeric(info$p1)
  mu_y_b[yb_y_idx] <- nu_b0[yb_idx, 1L]
  kz <- info$kz
  gmat <- matrix(0, kz, pv)
  mu_z <- numeric(kz)
  sigma_z <- matrix(0, kz, kz)
  zb_idx <- integer(0L)
  if (kz > 0L) {
    zb_idx <- match(info$zb.names, ov_b)
    gmat[, seq_len(meta)] <- mb$lambda[zb_idx, eta_idx, drop = FALSE]
    mu_z <- as.numeric(nu_b0[zb_idx, 1L])
    sigma_z <- mb$theta[zb_idx, zb_idx, drop = FALSE]
  }
  imp <- list(
    sigma_w = sigma_w,
    mu_y = mu_w + mu_y_b,
    mu_y_b = mu_y_b,
    P = pmat,
    lmat = lmat,
    q0 = q0,
    z.v.idx = match(z_lv_idx, eta_idx),
    pv = pv,
    sigma.v = sigma_v,
    mu.v = mu_v,
    cc = cc,
    mu.exo = mu_exo,
    eta.names = lv_b[eta_idx],
    meta = meta,
    mu_z = mu_z,
    gmat = gmat,
    sigma.z = sigma_z
  )
  list(
    imp = imp,
    mm_w = mm_w,
    mm_b = mm_b,
    mw = mw,
    mb = mb,
    y_idx = y_idx,
    x_lv_idx = x_lv_idx,
    lhs_lv = lhs_lv,
    ib_w = ib_w,
    rf_y = rf_y,
    psi_w0 = psi_w0,
    alpha_w0 = alpha_w0,
    ib_b = ib_b,
    v_eta = v_eta,
    v_ee_inv = v_ee_inv,
    cc_eta = cc_eta,
    eta_idx = eta_idx,
    exo_lv_idx = exo_lv_idx,
    exo_ov_idx = exo_ov_idx,
    eps_b_idx = eps_b_idx,
    yb_idx = yb_idx,
    yb_y_idx = yb_y_idx,
    zb_idx = zb_idx
  )
}

# The implied pieces, as lav_mvn_cl_rs_implied() returns them
rs_implied <- function(lavmodel, info, glist = lavmodel@GLIST) {
  steps <- rs_implied_steps(lavmodel, info, glist)
  if (is.null(steps)) NULL else steps$imp
}

## ----- Reverse ---------------------------------------------------------------

# `g` holds the derivative of a function F with respect to each piece:
# sigma_w, mu_y, P, lmat, q0, sigma.v, mu.v, cc and mu.exo, every element taken
# on its own. Returns dF/dx for lavaan's free parameters (packed, one per
# equality group).
rs_implied_back <- function(lavmodel, steps, g) {
  mw <- steps$mw
  mb <- steps$mb
  imp <- steps$imp
  meta <- imp$meta
  pv <- imp$pv
  glist_g <- lapply(lavmodel@GLIST, function(m) m * 0)

  ## ----- Level 1 ---------------------------------------------------------------
  w <- steps$mm_w
  names_w <- names(mw)
  slot_w <- function(nm) w[which(names_w == nm)]
  rf_y <- steps$rf_y
  g_sw <- g$sigma_w
  g_rfy <- (g_sw + t(g_sw)) %*%
    rf_y %*%
    steps$psi_w0 +
    outer(g$mu_y, as.numeric(steps$alpha_w0))
  if (length(steps$x_lv_idx) > 0L) {
    g_rfy[, steps$x_lv_idx] <- g_rfy[, steps$x_lv_idx, drop = FALSE] + g$P
  }
  for (i in seq_along(steps$lhs_lv)) {
    if (!is.na(steps$lhs_lv[i])) {
      g_rfy[, steps$lhs_lv[i]] <- g_rfy[, steps$lhs_lv[i]] + g$lmat[, i]
    }
  }
  g_psi_w <- t(rf_y) %*% g_sw %*% rf_y
  g_alpha_w <- crossprod(rf_y, g$mu_y)
  if (length(steps$x_lv_idx) > 0L) {
    g_psi_w[steps$x_lv_idx, ] <- 0
    g_psi_w[, steps$x_lv_idx] <- 0
    g_alpha_w[steps$x_lv_idx, ] <- 0
  }
  g_rf <- matrix(0, nrow(mw$lambda), ncol(mw$lambda))
  g_rf[steps$y_idx, ] <- g_rfy
  ib_w <- steps$ib_w
  glist_g[[slot_w("lambda")]] <- g_rf %*% t(ib_w)
  if (!is.null(mw$beta)) {
    g_ib <- t(mw$lambda) %*% g_rf
    glist_g[[slot_w("beta")]] <- t(ib_w) %*% g_ib %*% t(ib_w)
  }
  glist_g[[slot_w("psi")]] <- g_psi_w
  g_theta_w <- glist_g[[slot_w("theta")]]
  g_theta_w[steps$y_idx, steps$y_idx] <- g_sw
  glist_g[[slot_w("theta")]] <- g_theta_w
  if (!is.null(mw$nu)) {
    g_nu_w <- glist_g[[slot_w("nu")]]
    g_nu_w[steps$y_idx, 1L] <- g$mu_y
    glist_g[[slot_w("nu")]] <- g_nu_w
  }
  if (!is.null(mw$alpha)) {
    glist_g[[slot_w("alpha")]] <- g_alpha_w
  }

  ## ----- Level 2 ---------------------------------------------------------------
  b <- steps$mm_b
  names_b <- names(mb)
  slot_b <- function(nm) b[which(names_b == nm)]
  eta_idx <- steps$eta_idx
  exo_idx <- steps$exo_lv_idx
  nlv_b <- ncol(mb$lambda)
  meta_seq <- seq_len(meta)
  g_v <- matrix(0, nlv_b, nlv_b)
  g_e <- numeric(nlv_b)
  g_e[eta_idx] <- g$mu.v[meta_seq]
  g_vc <- g$sigma.v[meta_seq, meta_seq, drop = FALSE]
  if (length(exo_idx) > 0L) {
    g_s <- (g_vc + t(g_vc)) / 2
    v_eta <- steps$v_eta
    v_ex <- v_eta[eta_idx, exo_idx, drop = FALSE]
    v_xe <- v_eta[exo_idx, eta_idx, drop = FALSE]
    w_inv <- steps$v_ee_inv
    g_c <- g$cc[meta_seq, , drop = FALSE]
    g_v[eta_idx, eta_idx] <- g_s
    g_v[eta_idx, exo_idx] <- -g_s %*% t(w_inv %*% v_xe) + g_c %*% t(w_inv)
    g_v[exo_idx, eta_idx] <- -t(v_ex %*% w_inv) %*% g_s
    g_w <- -t(v_ex) %*% g_s %*% t(v_xe) + t(v_ex) %*% g_c
    g_v[exo_idx, exo_idx] <- -t(w_inv) %*% g_w %*% t(w_inv)
    g_e[exo_idx] <- g_e[exo_idx] + g$mu.exo
  } else {
    g_v[eta_idx, eta_idx] <- g_vc
  }
  ib_b <- steps$ib_b
  glist_g[[slot_b("psi")]] <- t(ib_b) %*% g_v %*% ib_b
  g_ib_b <- g_v %*% ib_b %*% t(mb$psi) + t(g_v) %*% ib_b %*% mb$psi
  if (!is.null(mb$alpha)) {
    glist_g[[slot_b("alpha")]] <- crossprod(ib_b, g_e)
    g_ib_b <- g_ib_b + outer(g_e, as.numeric(mb$alpha))
  }
  if (!is.null(mb$beta)) {
    glist_g[[slot_b("beta")]] <- t(ib_b) %*% g_ib_b %*% t(ib_b)
  }
  g_lambda_b <- glist_g[[slot_b("lambda")]]
  g_lambda_b[steps$yb_idx, eta_idx] <- g$q0[
    steps$yb_y_idx,
    meta_seq,
    drop = FALSE
  ]
  glist_g[[slot_b("lambda")]] <- g_lambda_b
  if (length(steps$eps_b_idx) > 0L) {
    eps_pos <- meta + seq_along(steps$eps_b_idx)
    g_theta_b <- glist_g[[slot_b("theta")]]
    g_theta_b[steps$eps_b_idx, steps$eps_b_idx] <- g$sigma.v[
      eps_pos,
      eps_pos,
      drop = FALSE
    ]
    glist_g[[slot_b("theta")]] <- g_theta_b
  }
  if (!is.null(mb$nu)) {
    g_nu_b <- glist_g[[slot_b("nu")]]
    g_nu_b[steps$yb_idx, 1L] <- g$mu_y[steps$yb_y_idx]
    if (length(steps$exo_ov_idx) > 0L) {
      g_nu_b[steps$exo_ov_idx, 1L] <- g_nu_b[steps$exo_ov_idx, 1L] + g$mu.exo
    }
    glist_g[[slot_b("nu")]] <- g_nu_b
  }

  ## ----- To the free parameters ------------------------------------------------
  dx <- numeric(lavmodel@nx.free)
  for (mm in seq_along(glist_g)) {
    x_idx <- lavmodel@x.free.idx[[mm]]
    if (length(x_idx) == 0L) {
      next
    }
    vals <- glist_g[[mm]][lavmodel@m.free.idx[[mm]]]
    sums <- rowsum(vals, x_idx)
    dx[as.integer(rownames(sums))] <- dx[as.integer(rownames(sums))] + sums
  }
  dx
}
