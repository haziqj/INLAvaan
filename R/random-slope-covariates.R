# The density of the observed covariates of a random-slope model with
# fixed.x = FALSE.
#
# The random-slope kernel is the density of the responses given the covariates
# that carry slopes and the between-level exogenous covariates. A covariate
# with no slope (observed at level 1 only, or at both levels) is in the
# kernel's response set instead, and the kernel scores it with its own mean and
# variance, as if it were independent of the conditioned covariates. So the
# kernel is
#   p(y | x) p_k(x_r),
# with x_r those response covariates and p_k their two-level normal density at
# the implied moments. With fixed.x = FALSE the covariates are modelled, and
# the joint log-likelihood is
#   log p(y, x) = kernel - log p_k(x_r) + log p(x),
# with p(x) the two-level normal density of all the observed exogenous
# covariates at the implied moments, their covariances included. The two
# extra terms are rs_x_loglik() on two column sets. lavaan's own fit leaves
# them out, so there the covariate moments do not enter the likelihood.
#
# In the two-level normal model of a set of covariates, a covariate observed
# at level 1 (v) is v_ij = mu_v + b_j + e_ij with e_ij ~ N(0, Sigma_w), where
# b_j is zero for a covariate at level 1 only. A covariate at level 2 only (z)
# is one value per cluster. (z_j, b_j) is normal with the between covariance.
# A cluster then contributes the within scatter of v (n_j - 1 degrees of
# freedom) and the density of (z_j, vbar_j), whose covariance is the between
# block plus Sigma_w / n_j for v. The clusters are worked by size, since that
# covariance depends on n_j only. Missing covariate values are not allowed
# here: lavaan refuses them in a random-slope model.

## ----- Plan ------------------------------------------------------------------

# Everything about covariate columns `cols` (names) that depends on the data
# only. NULL for an empty set.
rs_x_plan <- function(lavdata, lavsamplestats, cols) {
  if (length(cols) == 0L) {
    return(NULL)
  }
  ovn <- lavdata@ov.names[[1L]]
  l1 <- lavdata@ov.names.l[[1L]][[1L]]
  l2 <- lavdata@ov.names.l[[1L]][[2L]]
  lp <- lavdata@Lp[[1L]]
  idx <- match(cols, ovn)
  z_cols <- intersect(idx, lp$between.idx[[2L]])
  v_cols <- setdiff(idx, z_cols)
  X <- lavdata@X[[1L]]
  y2 <- lavsamplestats@YLp[[1L]][[2L]]$Y2
  if (anyNA(X[, v_cols]) || anyNA(y2[, z_cols])) {
    cli_abort(
      c(
        "A random-slope model with {.code fixed.x = FALSE} needs complete
         covariates.",
        "x" = "{.field {ovn[c(v_cols, z_cols)]}} {?has/have} missing values.",
        "i" = "Use {.code fixed.x = TRUE}, or remove the incomplete rows."
      ),
      class = "inlavaan_rs_x_missing"
    )
  }
  cl <- lp$cluster.idx[[2L]]
  n_cl <- lp$nclusters[[2L]]
  n_j <- tabulate(cl, n_cl)
  qz <- length(z_cols)
  qv <- length(v_cols)
  vbar <- rowsum.default(X[, v_cols, drop = FALSE], cl, reorder = TRUE) / n_j
  vc <- X[, v_cols, drop = FALSE] - vbar[cl, , drop = FALSE]
  zv2 <- match(ovn[c(z_cols, v_cols)], l2)
  sizes <- sort(unique(n_j))
  list(
    qz = qz,
    qv = qv,
    v1 = match(ovn[v_cols], l1),
    zv2 = zv2,
    has_b = !is.na(zv2),
    p_w = length(l1),
    p_b = length(l2),
    n_j = n_j,
    n_within = sum(n_j - 1),
    sum_log_n = sum(log(n_j)),
    # Within scatter of v, all clusters, and per cluster (for the LOO)
    a_tot = crossprod(vc),
    vc = vc,
    cl = cl,
    # (z_j, vbar_j), one row per cluster
    obs = cbind(y2[, z_cols, drop = FALSE], vbar),
    sizes = sizes,
    size_idx = lapply(sizes, function(s) which(n_j == s))
  )
}

# The plans of a random-slope fit with fixed.x = FALSE: all the observed
# exogenous covariates, and those the kernel scores as responses. NULL when
# fixed.x is TRUE or the model has no observed exogenous covariate.
rs_x_setup <- function(lavmodel, lavdata, lavsamplestats, lavcache) {
  if (isTRUE(lavmodel@fixed.x) || !has_random_slopes(lavmodel)) {
    return(NULL)
  }
  info <- lavcache[[1L]]$rs$info
  # Under fixed.x = FALSE lavaan keeps no covariate list in lavdata. The
  # observed exogenous variables of each level are its dummy latent variables
  # for exogenous observed variables.
  ov_l <- lavdata@ov.names.l[[1L]]
  ov_x <- unique(unlist(lapply(1:2, function(b) {
    ov_l[[b]][lavmodel@ov.x.dummy.ov.idx[[b]]]
  })))
  cond <- unique(c(info$x.names, info$exo.b.names))
  cond <- cond[nzchar(cond)]
  resp <- unique(c(info$y.names, info$zb.names))
  all_x <- union(cond, intersect(resp, ov_x))
  if (length(all_x) == 0L) {
    return(NULL)
  }
  list(
    all = rs_x_plan(lavdata, lavsamplestats, all_x),
    resp = rs_x_plan(lavdata, lavsamplestats, intersect(resp, ov_x))
  )
}

# The random-slope models with fixed.x = FALSE that rs_x_setup() describes
# exactly. The identity at the top needs the kernel to be p(y | x) times the
# density of its response covariates on their own. That holds when each
# covariate with no slope is observed at one level only. A covariate at both
# levels splits into a within and a between part that the outcomes regress
# on, and the kernel finds those parts as if the covariate were independent
# of the conditioned ones, which their free covariances deny. The quadrature
# route (a slope on a latent or split covariate) has the same problem.
check_rs_fixedx <- function(lavmodel, lavdata, lavcache) {
  if (isTRUE(lavmodel@fixed.x) || !has_random_slopes(lavmodel)) {
    return(invisible(NULL))
  }
  info <- lavcache[[1L]]$rs$info
  ov_l <- lavdata@ov.names.l[[1L]]
  ov_x <- unique(unlist(lapply(1:2, function(b) {
    ov_l[[b]][lavmodel@ov.x.dummy.ov.idx[[b]]]
  })))
  if (length(ov_x) == 0L) {
    return(invisible(NULL))
  }
  why <- c(
    "i" = "Refit with {.code fixed.x = TRUE}, which conditions on the
           covariates."
  )
  if (isTRUE(info$nl.flag)) {
    cli_abort(
      c(
        "{.code fixed.x = FALSE} is not available for a random slope on a
         latent or split covariate.",
        "x" = "The quadrature route does not model the observed covariates
               jointly with the outcomes.",
        why
      ),
      class = "inlavaan_rs_fixedx"
    )
  }
  resp <- unique(c(info$y.names, info$zb.names))
  split <- intersect(intersect(resp, ov_x), intersect(ov_l[[1L]], ov_l[[2L]]))
  if (length(split) > 0L) {
    cli_abort(
      c(
        "{.code fixed.x = FALSE} is not available in a random-slope model
         with a covariate at both levels.",
        "x" = "{.field {split}} {?is/are} observed at both levels, and the
               random-slope likelihood cannot condition {?its/their} within
               and between parts on the other covariates.",
        why
      ),
      class = "inlavaan_rs_fixedx"
    )
  }
  invisible(NULL)
}

## ----- Moments ---------------------------------------------------------------

# The within covariance of v, the between block of (z, v), and the means of
# (z_j, vbar_j), from lavaan's implied moments of the two levels
rs_x_moments <- function(plan, implied) {
  q <- plan$qz + plan$qv
  v_pos <- plan$qz + seq_len(plan$qv)
  hb <- plan$has_b
  sw <- implied$cov[[1L]][plan$v1, plan$v1, drop = FALSE]
  b0 <- matrix(0, q, q)
  b0[hb, hb] <- implied$cov[[2L]][plan$zv2[hb], plan$zv2[hb], drop = FALSE]
  mu_w <- as.numeric(implied$mean[[1L]])
  mu_b <- as.numeric(implied$mean[[2L]])
  mu <- numeric(q)
  mu[v_pos] <- mu_w[plan$v1]
  mu[hb] <- mu[hb] + mu_b[plan$zv2[hb]]
  list(sw = sw, b0 = b0, mu = mu, v_pos = v_pos)
}

# The covariance of (z_j, vbar_j) for a cluster of size n
rs_x_bmat <- function(mom, n) {
  b <- mom$b0
  b[mom$v_pos, mom$v_pos] <- b[mom$v_pos, mom$v_pos] + mom$sw / n
  b
}

## ----- Log-likelihood and gradient -------------------------------------------

# The log-likelihood of the covariates of `plan` at the implied moments, with
# the 2 pi terms. With `grad = TRUE` it also returns the derivative of -2 log L
# with respect to the implied moments of each level (lavaan's list form, unique
# elements). NULL for an empty plan, -Inf where a covariance is not positive
# definite.
rs_x_loglik <- function(plan, implied, grad = FALSE) {
  if (is.null(plan)) {
    return(NULL)
  }
  mom <- rs_x_moments(plan, implied)
  q <- plan$qz + plan$qv
  out <- 0
  g_sw <- matrix(0, plan$qv, plan$qv)
  g_b <- matrix(0, q, q)
  g_mu <- numeric(q)
  if (plan$qv > 0L) {
    ch <- tryCatch(chol(mom$sw), error = function(e) NULL)
    if (is.null(ch)) {
      return(-Inf)
    }
    sw_inv <- chol2inv(ch)
    logdet <- 2 * sum(log(diag(ch)))
    out <- out -
      0.5 *
        (plan$n_within *
          (plan$qv * log(2 * pi) + logdet) +
          sum(sw_inv * plan$a_tot) +
          plan$qv * plan$sum_log_n)
    if (grad) {
      g_sw <- -0.5 *
        plan$n_within *
        sw_inv +
        0.5 * sw_inv %*% plan$a_tot %*% sw_inv
    }
  }
  for (k in seq_along(plan$sizes)) {
    n <- plan$sizes[k]
    rows <- plan$size_idx[[k]]
    b <- rs_x_bmat(mom, n)
    ch <- tryCatch(chol(b), error = function(e) NULL)
    if (is.null(ch)) {
      return(-Inf)
    }
    b_inv <- chol2inv(ch)
    r <- sweep(plan$obs[rows, , drop = FALSE], 2L, mom$mu)
    rr <- crossprod(r)
    m <- length(rows)
    out <- out -
      0.5 * (m * (q * log(2 * pi) + 2 * sum(log(diag(ch)))) + sum(b_inv * rr))
    if (grad) {
      h <- -0.5 * (m * b_inv - b_inv %*% rr %*% b_inv)
      g_b <- g_b + h
      if (plan$qv > 0L) {
        g_sw <- g_sw + h[mom$v_pos, mom$v_pos, drop = FALSE] / n
      }
      g_mu <- g_mu + as.numeric(b_inv %*% colSums(r))
    }
  }
  if (!grad) {
    return(out)
  }
  attr(out, "dmom") <- rs_x_dmom(plan, g_sw, g_b, g_mu)
  out
}

# The derivative of log L with respect to (Sigma_w of v, the between block, the
# means of (z_j, vbar_j)), placed in the moments of each level and turned into
# lavaan's form: the derivative of -2 log L with respect to the unique
# elements, each off-diagonal entry holding the derivative of the pair
rs_x_dmom <- function(plan, g_sw, g_b, g_mu) {
  hb <- plan$has_b
  v_pos <- plan$qz + seq_len(plan$qv)
  sigma_w <- matrix(0, plan$p_w, plan$p_w)
  sigma_w[plan$v1, plan$v1] <- g_sw
  sigma_b <- matrix(0, plan$p_b, plan$p_b)
  sigma_b[plan$zv2[hb], plan$zv2[hb]] <- g_b[hb, hb, drop = FALSE]
  mu_w <- numeric(plan$p_w)
  mu_w[plan$v1] <- g_mu[v_pos]
  mu_b <- numeric(plan$p_b)
  mu_b[plan$zv2[hb]] <- g_mu[hb]
  unique_form <- function(g) {
    out <- 2 * g
    diag(out) <- diag(g)
    -2 * out
  }
  list(
    Mu.W = -2 * mu_w,
    Sigma.W = unique_form(sigma_w),
    Mu.B = -2 * mu_b,
    Sigma.B = unique_form(sigma_b)
  )
}

# The covariate term of a fit, log p(x) - log p_k(x_r), with `grad = TRUE`
# also its derivative of -2 log L with respect to the moments
rs_x_term <- function(rs_x, implied, grad = FALSE) {
  l_all <- rs_x_loglik(rs_x$all, implied, grad)
  l_resp <- rs_x_loglik(rs_x$resp, implied, grad)
  if (is.null(l_resp)) {
    return(l_all)
  }
  out <- as.numeric(l_all) - as.numeric(l_resp)
  if (grad && is.finite(out)) {
    d_all <- attr(l_all, "dmom")
    d_resp <- attr(l_resp, "dmom")
    attr(out, "dmom") <- Map(`-`, d_all, d_resp)
  }
  out
}

# The derivative of log p(x) - log p_k(x_r) with respect to lavaan's free
# parameters (unpacked under ceq.simple), or NULL where it is not defined
rs_x_grad <- function(rs_x, lavmodel_x) {
  term <- rs_x_term(rs_x, lavaan::lav_model_implied(lavmodel_x), grad = TRUE)
  dmom <- attr(term, "dmom")
  if (is.null(dmom)) {
    return(NULL)
  }
  -0.5 * twolevel_moments_to_x(lavmodel_x, dmom)
}

## ----- Per cluster (LOO) -----------------------------------------------------

# The covariate log-likelihood of each cluster of `units`, and with
# `delta` (twolevel_delta()) the matrix of its derivatives with respect to
# lavaan's free parameters, one row per cluster
rs_x_cluster <- function(plan, implied, units, delta = NULL) {
  if (is.null(plan)) {
    return(list(
      loglik = numeric(length(units)),
      grad = if (!is.null(delta)) matrix(0, length(units), ncol(delta))
    ))
  }
  mom <- rs_x_moments(plan, implied)
  q <- plan$qz + plan$qv
  sw_inv <- if (plan$qv > 0L) solve(mom$sw)
  logdet_sw <- if (plan$qv > 0L) {
    as.numeric(determinant(mom$sw, logarithm = TRUE)$modulus)
  } else {
    0
  }
  ll <- numeric(length(units))
  dm <- if (!is.null(delta)) matrix(0, length(units), nrow(delta))
  for (u in seq_along(units)) {
    j <- units[u]
    n <- plan$n_j[j]
    a_j <- crossprod(plan$vc[plan$cl == j, , drop = FALSE])
    b <- rs_x_bmat(mom, n)
    b_inv <- solve(b)
    r <- plan$obs[j, ] - mom$mu
    l_w <- 0
    if (plan$qv > 0L) {
      l_w <- -0.5 *
        ((n - 1) *
          (plan$qv * log(2 * pi) + logdet_sw) +
          sum(sw_inv * a_j) +
          plan$qv * log(n))
    }
    ll[u] <- l_w -
      0.5 *
        (q *
          log(2 * pi) +
          as.numeric(determinant(b, logarithm = TRUE)$modulus) +
          sum(r * (b_inv %*% r)))
    if (!is.null(delta)) {
      br <- as.numeric(b_inv %*% r)
      h <- -0.5 * (b_inv - tcrossprod(br))
      g_sw <- matrix(0, plan$qv, plan$qv)
      if (plan$qv > 0L) {
        g_sw <- -0.5 *
          (n - 1) *
          sw_inv +
          0.5 * sw_inv %*% a_j %*% sw_inv +
          h[mom$v_pos, mom$v_pos, drop = FALSE] / n
      }
      dm[u, ] <- twolevel_dmoments_vec(rs_x_dmom(plan, g_sw, h, br))
    }
  }
  list(
    loglik = ll,
    grad = if (!is.null(delta)) -0.5 * dm %*% delta
  )
}

# The covariate term of each cluster, log p(x_j) - log p_k(x_rj), and its
# derivatives with respect to lavaan's free parameters
rs_x_cluster_term <- function(rs_x, lavmodel_x, units, grad = FALSE) {
  implied <- lavaan::lav_model_implied(lavmodel_x)
  delta <- if (grad) twolevel_delta(lavmodel_x)
  a <- rs_x_cluster(rs_x$all, implied, units, delta)
  r <- rs_x_cluster(rs_x$resp, implied, units, delta)
  list(
    loglik = a$loglik - r$loglik,
    grad = if (grad) a$grad - r$grad
  )
}

## ----- Saturated covariate model (fit indices) -------------------------------

# The covariate term log p(x) - log p_k(x_r) at the covariates' saturated
# maximum, and the number of covariate moments, for the reference model of the
# Bayesian fit indices, which conditions on the covariates. In the models
# check_rs_fixedx() allows, a covariate is within-only (no between part) or
# between-only, so the saturated maximum is the sample mean and the ML
# covariance of the within-only covariates over the rows and of the
# between-only covariates over the clusters.
rs_x_saturated <- function(rs_x) {
  sat <- function(plan) {
    if (is.null(plan)) {
      return(0)
    }
    implied <- list(
      mean = list(numeric(plan$p_w), numeric(plan$p_b)),
      cov = list(matrix(0, plan$p_w, plan$p_w), matrix(0, plan$p_b, plan$p_b))
    )
    z_pos <- seq_len(plan$qz)
    v_pos <- plan$qz + seq_len(plan$qv)
    if (plan$qv > 0L) {
      n <- sum(plan$n_j)
      vbar <- plan$obs[, v_pos, drop = FALSE]
      mu_v <- colSums(vbar * plan$n_j) / n
      dev <- sweep(vbar, 2L, mu_v)
      s_v <- (plan$a_tot + crossprod(dev * sqrt(plan$n_j))) / n
      implied$mean[[1L]][plan$v1] <- mu_v
      implied$cov[[1L]][plan$v1, plan$v1] <- s_v
    }
    if (plan$qz > 0L) {
      z <- plan$obs[, z_pos, drop = FALSE]
      z2 <- plan$zv2[z_pos]
      mu_z <- colMeans(z)
      implied$mean[[2L]][z2] <- mu_z
      implied$cov[[2L]][z2, z2] <- crossprod(sweep(z, 2L, mu_z)) / nrow(z)
    }
    rs_x_loglik(plan, implied)
  }
  q <- rs_x$all$qz + rs_x$all$qv
  list(
    loglik = sat(rs_x$all) - sat(rs_x$resp),
    npar = q +
      rs_x$all$qv * (rs_x$all$qv + 1) / 2 +
      rs_x$all$qz * (rs_x$all$qz + 1) / 2
  )
}
