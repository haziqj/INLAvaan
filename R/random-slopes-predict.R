# Casewise values of a random-slope fit on the closed-form route, from
# lavaan's kernel pieces (rs_implied_pieces()). For observation i of cluster
# j, the outcomes given the level-2 vector v and the covariates are
#   E[y_ij | v, x_ij] = mu_y + P x_ij + Q_ij v.
# fitted(type = "casewise") takes v at its mean given the between-level
# covariates, which is the population-average value given the covariates.
# predict(type = "yhat") takes v at the empirical Bayes draws of the cluster,
# as predict() does for other two-level fits.

# The rows of E[y | v, x] for every observation, with `V` one level-2 vector
# per cluster.
rs_y_given_v <- function(imp, info, X1, cl, V) {
  paths <- info$path.tab
  zcol <- imp$z.v.idx[paths$z.idx]
  Xc <- X1[, info$x.data.idx, drop = FALSE]
  Vi <- V[cl, , drop = FALSE]
  Y <- matrix(imp$mu_y, nrow(X1), info$p1, byrow = TRUE) +
    Xc %*% t(imp$P) +
    Vi %*% t(imp$q0)
  for (p in seq_len(nrow(paths))) {
    Y <- Y + (Xc[, paths$x.idx[p]] * Vi[, zcol[p]]) %o% imp$lmat[, p]
  }
  Y
}

# The mean of the level-2 vector of each cluster given its between-level
# covariates.
rs_v_mean <- function(imp, info, rs) {
  J <- rs$stats$nclusters
  D <- matrix(imp$mu.v, J, imp$pv, byrow = TRUE)
  if (info$nexo.b > 0L) {
    D <- D + sweep(rs$stats$exo.b, 2L, imp$mu.exo) %*% t(imp$cc)
  }
  D
}

check_rs_casewise <- function(spec, what) {
  if (spec$route == "B") {
    cli_abort(
      c(
        "{what} is not available for a random slope on a latent or split
         covariate.",
        "i" = "It is available on the closed-form route, where the covariate
               carrying the slope is observed and purely within-cluster."
      ),
      class = "inlavaan_rs_casewise"
    )
  }
}

# fitted(type = "casewise") and residuals(type = "casewise"): one row per
# observation and one column per level-1 variable, the outcomes at their
# expectation given the covariates and the covariates as observed.
rs_casewise <- function(object, residual = FALSE) {
  int <- get_inlavaan_internal(object)
  spec <- rs_spec(int)
  check_rs_casewise(spec, "Casewise output")
  info <- spec$rs$info
  lavdata <- object@Data
  imp <- rs_implied_pieces(object@Model, info)
  X1 <- lavdata@X[[1L]]
  cl <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  Y <- rs_y_given_v(imp, info, X1, cl, rs_v_mean(imp, info, spec$rs))
  out <- X1
  out[, info$y.data.idx] <- Y
  colnames(out) <- lavdata@ov.names[[1L]]
  out <- out[, lavdata@Lp[[1L]]$ov.idx[[1L]], drop = FALSE]
  if (residual) {
    obs <- X1[, lavdata@Lp[[1L]]$ov.idx[[1L]], drop = FALSE]
    out <- obs - out
  }
  out
}

# The within-level parts that predict() needs. Given v and the covariates, the
# outcomes are y = E[y | v, x] + e, with e ~ N(0, Sigma_w) and
# e = Lambda (eta - E[eta | v, x]) + epsilon. For the latent variables `r` that
# have empirical Bayes values, C = Cov(e, eta_r), K = C Phi_rr^-1 carries
# eta_r - E[eta_r | v, x] to the outcomes, and H = K C' is the part of Sigma_w
# that runs through eta_r. Observed covariates are fixed, so they carry no
# variance here.
rs_within_parts <- function(w, info, r) {
  lv <- colnames(w$lambda)
  nlv <- length(lv)
  B <- if (is.null(w$beta)) matrix(0, nlv, nlv) else w$beta
  dimnames(B) <- list(lv, lv)
  alpha <- if (is.null(w$alpha)) numeric(nlv) else w$alpha[, 1L]
  names(alpha) <- lv
  xv <- intersect(info$x.names, lv)
  psi <- w$psi
  psi[xv, ] <- 0
  psi[, xv] <- 0
  ib <- solve(diag(nlv) - B)
  phi <- ib %*% psi %*% t(ib)
  dimnames(phi) <- list(lv, lv)
  p1 <- length(info$y.names)
  K <- matrix(0, p1, length(r), dimnames = list(info$y.names, r))
  H <- matrix(0, p1, p1)
  if (length(r) > 0L) {
    C <- w$lambda[info$y.names, , drop = FALSE] %*% phi[, r, drop = FALSE]
    K <- C %*% solve(phi[r, r, drop = FALSE])
    H <- K %*% t(C)
  }
  list(B = B, alpha = alpha, xv = xv, K = K, H = (H + t(H)) / 2)
}

# The means of the latent variables `r` given the covariates and the level-2
# vectors `V`, one row per observation.
rs_eta_mean <- function(parts, info, imp, X1, cl, V, r) {
  B <- parts$B
  xv <- parts$xv
  n <- setdiff(rownames(B), xv)
  paths <- info$path.tab
  zcol <- imp$z.v.idx[paths$z.idx]
  base <- matrix(parts$alpha[n], nrow(X1), length(n), byrow = TRUE) +
    X1[, info$x.data.idx[match(xv, info$x.names)], drop = FALSE] %*%
      t(B[n, xv, drop = FALSE])
  colnames(base) <- n
  for (p in which(paths$lhs %in% n)) {
    base[, paths$lhs[p]] <- base[, paths$lhs[p]] +
      X1[, info$x.data.idx[paths$x.idx[p]]] * V[cl, zcol[p]]
  }
  m <- base %*% t(solve(diag(length(n)) - B[n, n, drop = FALSE]))
  colnames(m) <- n
  m[, r, drop = FALSE]
}

# Draws of the within residual that "ypred" adds, given each row's empirical
# Bayes latent values. For a row with observed outcomes o, the residual has
# covariance Sigma_w - H[, o] Sigma_w[o, o]^-1 H[o, ]: the residual itself
# plus the uncertainty of the latent values.
rs_draw_within <- function(Y, sigma_w, H) {
  out <- matrix(0, nrow(Y), ncol(Y))
  obs <- !is.na(Y)
  key <- apply(obs, 1L, function(z) paste(which(z), collapse = ","))
  for (k in unique(key)) {
    rows <- which(key == k)
    o <- which(obs[rows[1L], ])
    R <- sigma_w
    if (length(o) > 0L) {
      R <- R -
        H[, o, drop = FALSE] %*%
          solve(sigma_w[o, o, drop = FALSE], H[o, , drop = FALSE])
    }
    out[rows, ] <- matrix(
      stats::rnorm(length(rows) * ncol(R)),
      length(rows)
    ) %*%
      rs_psd_root(R)
  }
  out
}

# predict(type = "yhat" or "ypred") at each row of `x_samp`. The level-2
# latent variables are drawn from their empirical Bayes means and standard
# deviations, as for predict(type = "lv"), and the between residuals stay at
# their means. The level-1 latent variables enter at their empirical Bayes
# means. "ypred" adds draws of the within residual given those values, of the
# between residuals and of the between-only outcomes' residuals.
rs_predict_y <- function(object, lavmodel, lavdata, x_samp, type) {
  spec <- rs_spec(object)
  check_rs_casewise(spec, "{.code predict(type = \"{type}\")}")
  info <- spec$rs$info
  X1 <- lavdata@X[[1L]]
  cl <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  J <- spec$rs$stats$nclusters
  lapply(seq_len(nrow(x_samp)), function(i) {
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x_samp[i, ])
    imp <- rs_implied_pieces(lavmodel_x, info)
    eb <- lavaan___lav_mvn_cl_rs_eb(
      lavmodel = lavmodel_x,
      lavdata = lavdata,
      lavcache = object$lavcache,
      se = TRUE
    )
    V <- rs_v_mean(imp, info, spec$rs)
    lat <- seq_len(imp$meta)
    V[, lat] <- eb$l2 + eb$se2 * stats::rnorm(length(eb$l2))
    Y <- rs_y_given_v(imp, info, X1, cl, V)
    w <- rs_glist_block(lavmodel_x, lavmodel_x@GLIST, 1L)$mats
    r <- colnames(eb$l1)
    parts <- rs_within_parts(w, info, r)
    if (length(r) > 0L) {
      m <- rs_eta_mean(parts, info, imp, X1, cl, V, r)
      Y <- Y + (eb$l1 - m) %*% t(parts$K)
    }
    if (type == "ypred") {
      Y <- Y +
        rs_draw_within(
          X1[, info$y.data.idx, drop = FALSE],
          imp$sigma_w,
          parts$H
        )
      eps <- setdiff(seq_len(imp$pv), lat)
      if (length(eps) > 0L) {
        e_b <- matrix(stats::rnorm(J * length(eps)), J) %*%
          rs_psd_root(imp$sigma.v[eps, eps, drop = FALSE])
        Y <- Y + e_b[cl, , drop = FALSE] %*% t(imp$q0[, eps, drop = FALSE])
      }
    }
    out <- X1
    out[, info$y.data.idx] <- Y
    if (info$kz > 0L) {
      Z <- matrix(imp$mu_z, J, info$kz, byrow = TRUE) + V %*% t(imp$gmat)
      if (type == "ypred") {
        Z <- Z +
          matrix(stats::rnorm(J * info$kz), J) %*% rs_psd_root(imp$sigma.z)
      }
      out[, info$zb.data.idx] <- Z[cl, , drop = FALSE]
    }
    colnames(out) <- lavdata@ov.names[[1L]]
    out
  })
}
