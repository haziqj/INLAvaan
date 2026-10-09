# Casewise values of a random-slope fit on the closed-form route, from
# lavaan's kernel pieces (rs_implied_pieces()). For observation i of cluster
# j, the outcomes given the level-2 vector v and the covariates are
#   E[y_ij | v, x_ij] = mu_y + P x_ij + Q_ij v.
# fitted(type = "casewise") takes v at its mean given the between-level
# covariates, which is the population-average value given the covariates.

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
