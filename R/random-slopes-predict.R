# Casewise values of a random-slope fit on the closed-form route, from
# lavaan's kernel pieces (rs_implied_pieces()). For observation i of cluster
# j, the outcomes given the level-2 vector v and the covariates are
#   E[y_ij | v, x_ij] = mu_y + P x_ij + Q_ij v.
# fitted(type = "casewise") takes v at its mean given the between-level
# covariates, which is the population-average value given the covariates.
# predict(type = "yhat") takes v at the empirical Bayes draws of the cluster,
# as predict() does for other two-level fits.

# The rows of E[y | v, x] for every observation, with `V` one level-2 vector
# per cluster
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
# covariates
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
# expectation given the covariates and the covariates as observed
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

# predict(type = "yhat" or "ypred") at each row of `x_samp`. The level-2
# latent variables are drawn from their empirical Bayes means and standard
# deviations, as for predict(type = "lv"), and the between residuals stay at
# their means. The level-1 latent variables enter at their empirical Bayes
# means, through Lambda_w (l1 - E[eta_w | v, x]). "ypred" adds draws of the
# level-1 residuals, the between residuals and the between-only outcomes'
# residuals.
rs_predict_y <- function(object, lavmodel, lavdata, x_samp, type) {
  spec <- rs_spec(object)
  check_rs_casewise(spec, "{.code predict(type = \"{type}\")}")
  info <- spec$rs$info
  X1 <- lavdata@X[[1L]]
  cl <- lavdata@Lp[[1L]]$cluster.idx[[2L]]
  J <- spec$rs$stats$nclusters
  paths <- info$path.tab
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
    # Level-1 latent variables: their empirical Bayes means less their means
    # given v and x, through the within loadings
    w <- rs_glist_block(lavmodel_x, lavmodel_x@GLIST, 1L)$mats
    r <- colnames(eb$l1)
    if (length(r) > 0L) {
      lv <- colnames(w$lambda)
      xv <- intersect(info$x.names, lv)
      B <- if (is.null(w$beta)) matrix(0, length(lv), length(lv)) else w$beta
      dimnames(B) <- list(lv, lv)
      alpha <- if (is.null(w$alpha)) numeric(length(lv)) else w$alpha[, 1L]
      names(alpha) <- lv
      base <- matrix(alpha[r], nrow(X1), length(r), byrow = TRUE) +
        X1[, match(xv, lavdata@ov.names[[1L]]), drop = FALSE] %*%
          t(B[r, xv, drop = FALSE])
      colnames(base) <- r
      zcol <- imp$z.v.idx[paths$z.idx]
      for (p in which(paths$lhs %in% r)) {
        base[, paths$lhs[p]] <- base[, paths$lhs[p]] +
          X1[, info$x.data.idx[paths$x.idx[p]]] * V[cl, zcol[p]]
      }
      m <- base %*% t(solve(diag(length(r)) - B[r, r, drop = FALSE]))
      Y <- Y + (eb$l1 - m) %*% t(w$lambda[info$y.names, r, drop = FALSE])
    }
    if (type == "ypred") {
      th <- w$theta[info$y.names, info$y.names, drop = FALSE]
      Y <- Y + matrix(stats::rnorm(length(Y)), nrow(Y)) %*% rs_psd_root(th)
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
