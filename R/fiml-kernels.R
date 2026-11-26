# Single-level FIML kernels: the log-likelihood of data with missing values and
# its gradient, for fits without covariates or weights (fiml_fast_path()).
#
# A missing-data pattern that observes the variables o and misses the variables
# m is scored with the inverse of the block Sigma_oo. lavaan's log-likelihood
# takes that inverse from the inverse of Sigma with one update for each
# pattern, and its gradient factors the block of each pattern, both in a loop
# over the patterns. Here every pattern takes one of two routes, chosen once
# per model.
#
# Shared route, for patterns that miss few variables. With A = Sigma^-1 and
# K = A[m, m],
#   Sigma_oo^-1 padded with zeros = A - A[, m] K^-1 A[m, ],
#   log|Sigma_oo| = log|Sigma| + log|K|,
# so all patterns share one inverse and a pattern needs only its k x k matrix
# K. Patterns with the same k are worked together: the Cholesky factors of
# their K are found in one pass of vector operations, with no loop over the
# patterns. The quadratic forms in A come from sufficient statistics that are
# summed over the patterns once per model.
#
# Direct route, for patterns that miss many variables (planned missingness):
# the Cholesky factor of Sigma_oo itself, one pattern at a time.
#
# The two routes give the same log-likelihood up to rounding. Everything that
# depends only on the data is in the plan of fiml_plan(). The direct route
# keeps the arithmetic of the kernels before the plan, so a model whose
# patterns are all direct gives the same bits as it did then.

fiml_fast_path <- function(lavmodel, lavsamplestats, lavdata) {
  isTRUE(lavsamplestats@missing.flag) &&
    lavdata@nlevels == 1L &&
    !isTRUE(lavmodel@conditional.x) &&
    all(lengths(lavsamplestats@x.idx) == 0L) &&
    all(vapply(lavdata@weights, is.null, logical(1))) &&
    !has_random_slopes(lavmodel)
}

# The gradient follows lav_model_grad() for single-level ML: its own LISREL
# derivative of the moments and its own map to the free parameters, so the
# result is lavaan's gradient for any model it would treat this way.
fiml_grad_fast_path <- function(lavmodel, lavsamplestats, lavdata) {
  lavmodel@estimator == "ML" &&
    fiml_fast_path(lavmodel, lavsamplestats, lavdata) &&
    lavmodel@representation == "LISREL" &&
    isTRUE(lavmodel@meanstructure) &&
    !isTRUE(lavmodel@composites) &&
    !isTRUE(lavmodel@correlation) &&
    !isTRUE(lavmodel@group.w.free)
}

# What a fit needs to know about FIML, found once per model: if the
# log-likelihood and the gradient take the kernels of this file, and the plan
fiml_setup <- function(lavmodel, lavsamplestats, lavdata) {
  loglik <- fiml_fast_path(lavmodel, lavsamplestats, lavdata)
  list(
    loglik = loglik,
    grad = loglik && fiml_grad_fast_path(lavmodel, lavsamplestats, lavdata),
    plan = if (loglik) fiml_plan(lavsamplestats) else NULL
  )
}

## ----- Plan ------------------------------------------------------------------

# One plan per group
fiml_plan <- function(lavsamplestats) {
  lapply(lavsamplestats@missing, fiml_plan_group)
}

# TRUE for the patterns that take the shared route. `k` is the number of
# missing variables of each pattern, and `many` is TRUE for a pattern with
# more than one case. The rule compares rough costs of one call of the
# log-likelihood plus one call of the gradient, in microseconds, from timings
# on models of 10 to 60 variables. A set of patterns that miss k variables each
# costs about 15 + 8 k + 2.4 k^2 on the shared route, however many patterns it
# holds. A pattern that observes q variables costs about 12 + q^3 / 2400 on
# the direct route, and about 14 + q^3 / 900 when it has more than one case.
# So many patterns that miss few variables are shared, and a few patterns that
# miss many are direct. The shared route also pays about 30 + p^3 / 800 once
# per group for the inverse of Sigma and the sufficient statistics. When that
# is more than the direct route costs for the patterns it would take, every
# pattern is direct: this is a small model with few patterns.
fiml_route_shared <- function(k, p, many = rep(FALSE, length(k))) {
  q <- p - k
  cost_direct <- ifelse(many, 14 + q^3 / 900, 12 + q^3 / 2400)
  cost_set <- 15 + 8 * k + 2.4 * k^2
  cost_direct_set <- tapply(cost_direct, k, sum)[as.character(k)]
  shared <- k == 0L | cost_set < as.numeric(cost_direct_set)
  cost_shared <- 30 +
    p^3 / 800 +
    sum(cost_set[shared & k > 0L & !duplicated(k)])
  if (sum(cost_direct[shared]) < cost_shared) {
    shared[] <- FALSE
  }
  shared
}

fiml_plan_group <- function(missing_g) {
  p <- length(missing_g[[1L]]$var.idx)
  pats <- lapply(missing_g, function(pat) {
    list(
      o = which(pat$var.idx),
      m = which(!pat$var.idx),
      freq = pat$freq,
      my = as.numeric(pat$MY),
      sy = if (pat$freq > 1L) pat$SY else NULL
    )
  })
  k <- vapply(pats, function(pat) length(pat$m), integer(1))
  many <- vapply(pats, function(pat) pat$freq > 1L, logical(1))
  shared <- fiml_route_shared(k, p, many)

  # The means are held as deviations from the mean of the available cases, so
  # that the sums of squares below lose no digits when a variable has a mean
  # that is large next to its SD.
  centre <- nobs <- numeric(p)
  for (pat in pats[shared]) {
    centre[pat$o] <- centre[pat$o] + pat$freq * pat$my
    nobs[pat$o] <- nobs[pat$o] + pat$freq
  }
  centre <- centre / pmax(nobs, 1)

  # Sufficient statistics of the shared patterns, with r the residual of a
  # case and zeros where it is missing: sum(r r') at the mean mu is
  # t2 - X - t(X) + cnt * tcrossprod(mu) with X = b_t * mu.
  t2 <- b_t <- cnt <- matrix(0, p, p)
  ysum <- numeric(p)
  const <- 0
  for (pat in pats[shared]) {
    o <- pat$o
    d <- pat$my - centre[o]
    w <- tcrossprod(d)
    if (!is.null(pat$sy)) {
      w <- w + pat$sy
    }
    t2[o, o] <- t2[o, o] + pat$freq * w
    b_t[o, o] <- b_t[o, o] + pat$freq * rep(d, each = length(o))
    cnt[o, o] <- cnt[o, o] + pat$freq
    ysum[o] <- ysum[o] + pat$freq * d
    const <- const + pat$freq * length(o) * log(2 * pi)
  }

  k_shared <- sort(unique(k[shared & k > 0L]))
  sets <- lapply(k_shared, function(kk) {
    fiml_plan_rows(pats[shared & k == kk], kk, p, centre)
  })
  miss_idx <- unlist(lapply(sets, function(set) as.vector(set$miss)))
  pair_idx <- unlist(lapply(sets, function(set) set$idx_k))
  list(
    p = p,
    pats = pats,
    direct = which(!shared),
    freq_shared = sum(vapply(pats[shared], function(pat) pat$freq, numeric(1))),
    centre = centre,
    nobs = nobs,
    ysum = ysum,
    t2 = t2,
    b_t = b_t,
    cnt = cnt,
    const = const,
    sets = sets,
    miss_idx = miss_idx,
    miss_pos = unique(miss_idx),
    pair_idx = pair_idx,
    pair_pos = unique(pair_idx)
  )
}

# The rows of the shared patterns with k missing variables. A pattern of f
# cases with mean ybar and covariance S has sum(r r') = f S + f d d' with
# d = ybar - mu, so it is held as one mean row sqrt(f) d, which moves with mu,
# and fixed rows whose cross-product is f S. `y` and `w` give a row as
# y - w * mu. `miss` holds the missing variables of each row, and `idx_k` the
# positions of the entries of K = A[miss, miss] in A (lower triangle, by row).
fiml_plan_rows <- function(pats, k, p, centre) {
  parts <- lapply(pats, function(pat) {
    fixed <- fiml_fixed_rows(pat, p)
    n_fixed <- nrow(fixed)
    y <- w <- numeric(p)
    y[pat$o] <- sqrt(pat$freq) * (pat$my - centre[pat$o])
    w[pat$o] <- sqrt(pat$freq)
    list(
      y = rbind(y, fixed, deparse.level = 0L),
      w = rbind(w, matrix(0, n_fixed, p), deparse.level = 0L),
      freq = c(pat$freq, numeric(n_fixed)),
      miss = matrix(pat$m, n_fixed + 1L, k, byrow = TRUE)
    )
  })
  y <- do.call(rbind, lapply(parts, `[[`, "y"))
  miss <- do.call(rbind, lapply(parts, `[[`, "miss"))
  freq <- unlist(lapply(parts, `[[`, "freq"))
  list(
    k = k,
    y = y,
    w = do.call(rbind, lapply(parts, `[[`, "w")),
    col = rep(seq_len(p), each = nrow(y)),
    miss = miss,
    freq = freq,
    sqrt_freq = sqrt(freq),
    idx_k = lapply(seq_len(k), function(a) {
      lapply(seq_len(a), function(b) (miss[, b] - 1L) * p + miss[, a])
    })
  )
}

# Rows whose cross-product is freq * SY, padded with zeros to p columns. The
# eigenvalues at rounding level are dropped, so a pattern of f cases gives at
# most f - 1 rows.
fiml_fixed_rows <- function(pat, p) {
  out <- matrix(0, 0L, p)
  if (is.null(pat$sy)) {
    return(out)
  }
  e <- eigen(pat$freq * pat$sy, symmetric = TRUE)
  keep <- which(e$values > 1e-13 * max(e$values[1L], 0))
  if (length(keep) == 0L) {
    return(out)
  }
  out <- matrix(0, length(keep), p)
  out[, pat$o] <- sqrt(e$values[keep]) * t(e$vectors[, keep, drop = FALSE])
  out
}

## ----- Shared route ----------------------------------------------------------

# Cholesky factors L of K = A[miss, miss] for all rows of a set at once.
# L[[a]][[b]] holds the entry (a, b), b <= a, as a vector over the rows. NULL
# when a K is not positive definite.
fiml_batch_chol <- function(A, idx_k, k) {
  L <- vector("list", k)
  for (a in seq_len(k)) {
    L_a <- vector("list", a)
    for (b in seq_len(a)) {
      v <- A[idx_k[[a]][[b]]]
      L_b <- if (b < a) L[[b]] else L_a
      for (j in seq_len(b - 1L)) {
        v <- v - L_a[[j]] * L_b[[j]]
      }
      if (b < a) {
        L_a[[b]] <- v / L_b[[b]]
      } else {
        if (!isTRUE(all(v > 0))) {
          return(NULL)
        }
        L_a[[a]] <- sqrt(v)
      }
    }
    L[[a]] <- L_a
  }
  L
}

# Inverses of the factors of fiml_batch_chol(), in the same layout
fiml_batch_tri_inv <- function(L, k) {
  L_inv <- lapply(seq_len(k), function(a) vector("list", a))
  for (b in seq_len(k)) {
    L_inv[[b]][[b]] <- 1 / L[[b]][[b]]
    for (a in seq_len(k)[-seq_len(b)]) {
      v <- L[[a]][[b]] * L_inv[[b]][[b]]
      for (j in seq_len(a - 1L)[-seq_len(b)]) {
        v <- v + L[[a]][[j]] * L_inv[[j]][[b]]
      }
      L_inv[[a]][[b]] <- -v / L[[a]][[a]]
    }
  }
  L_inv
}

# The shared patterns of one group. Returns the log-likelihood, and with
# `grad` the sums over the cases that the gradient needs: `omega` is
# Sigma_oo^-1 (W - Sigma_oo) Sigma_oo^-1 and `omega_mu` is Sigma_oo^-1 times
# the residual, both padded with zeros. NULL when Sigma or a K has no Cholesky
# factor.
#
# The gradient is the EM identity in terms of A. A case with residual r on
# its observed variables has s = (A r)[m], and its missing residuals have the
# conditional mean -K^-1 s and the conditional covariance K^-1. With H the
# sum over the cases of the second moments of the completed residuals and h
# the sum of the completed residuals,
#   omega = A H A - n A,   omega_mu = A h.
fiml_shared <- function(plan, mu, Sigma, grad = FALSE) {
  p <- plan$p
  if (plan$freq_shared == 0) {
    return(list(loglik = 0, omega = matrix(0, p, p), omega_mu = numeric(p)))
  }
  chol_sigma <- tryCatch(chol(Sigma), error = function(e) NULL)
  if (is.null(chol_sigma)) {
    return(NULL)
  }
  A <- chol2inv(chol_sigma)
  mu <- mu - plan$centre
  X <- plan$b_t * mu
  M <- plan$t2 - X - t(X) + plan$cnt * tcrossprod(mu)
  quad <- sum(A * M)
  logdet <- 2 * plan$freq_shared * sum(log(diag(chol_sigma)))
  n_sets <- length(plan$sets)
  if (grad) {
    cross <- pairs <- vector("list", n_sets)
  }

  for (i in seq_len(n_sets)) {
    set <- plan$sets[[i]]
    k <- set$k
    k_seq <- seq_len(k)
    R <- set$y - set$w * mu[set$col]
    L <- fiml_batch_chol(A, set$idx_k, k)
    if (is.null(L)) {
      return(NULL)
    }

    # z = L^-1 s, with s = (A r)[m]
    z <- vector("list", k)
    for (a in k_seq) {
      v <- rowSums(R * A[set$miss[, a], , drop = FALSE])
      L_a <- L[[a]]
      for (j in seq_len(a - 1L)) {
        v <- v - L_a[[j]] * z[[j]]
      }
      v <- v / L_a[[a]]
      quad <- quad - sum(v^2)
      logdet <- logdet + 2 * sum(set$freq * log(L_a[[a]]))
      z[[a]] <- v
    }
    if (!grad) {
      next
    }

    # w = K^-1 s, and the entries of K^-1 = t(L^-1) L^-1
    L_inv <- fiml_batch_tri_inv(L, k)
    w <- vector("list", k)
    for (a in k_seq) {
      v <- L_inv[[a]][[a]] * z[[a]]
      for (j in k_seq[-seq_len(a)]) {
        v <- v + L_inv[[j]][[a]] * z[[j]]
      }
      w[[a]] <- v
    }
    # The diagonal entries are halved, because the lower triangle is added to
    # its transpose below.
    pairs_i <- vector("list", k)
    for (a in k_seq) {
      pairs_a <- vector("list", a)
      for (b in seq_len(a)) {
        v <- L_inv[[a]][[a]] * L_inv[[a]][[b]]
        for (j in k_seq[-seq_len(a)]) {
          v <- v + L_inv[[j]][[a]] * L_inv[[j]][[b]]
        }
        v <- w[[a]] * w[[b]] + set$freq * v
        pairs_a[[b]] <- if (b < a) v else 0.5 * v
      }
      pairs_i[[a]] <- pairs_a
    }
    pairs[[i]] <- pairs_i
    # Rows r w_a for the second moments, with a last column for the sum of the
    # completed residuals
    R <- cbind(R, set$sqrt_freq, deparse.level = 0L)
    cross[[i]] <- lapply(w, function(w_a) R * w_a)
  }

  out <- list(loglik = -0.5 * (plan$const + logdet + quad))
  if (grad) {
    H <- M
    h <- plan$ysum - plan$nobs * mu
    if (n_sets > 0L) {
      D <- matrix(0, p, p)
      D[plan$pair_pos] <- rowsum(
        unlist(pairs),
        plan$pair_idx,
        reorder = FALSE
      )
      cross <- rowsum(
        do.call(rbind, unlist(cross, recursive = FALSE)),
        plan$miss_idx,
        reorder = FALSE
      )
      D[plan$miss_pos, ] <- D[plan$miss_pos, ] - cross[, -(p + 1L)]
      H <- H + D + t(D)
      h[plan$miss_pos] <- h[plan$miss_pos] - cross[, p + 1L]
    }
    out$omega <- A %*% H %*% A - plan$freq_shared * A
    out$omega_mu <- as.numeric(A %*% h)
  }
  out
}

## ----- Direct route ----------------------------------------------------------

# Adds the log-likelihood of the given patterns to `out`, each from the
# Cholesky factor of its observed block. Stops when a block has no factor.
fiml_direct_loglik <- function(pats, mu, Sigma, out = 0) {
  log_2pi <- log(2 * pi)
  for (pat in pats) {
    o <- pat$o
    L <- chol(Sigma[o, o, drop = FALSE])
    z <- backsolve(L, pat$my - mu[o], transpose = TRUE)
    dist <- sum(z^2)
    if (!is.null(pat$sy)) {
      dist <- dist + sum(chol2inv(L) * pat$sy)
    }
    out <- out -
      0.5 * pat$freq * (length(o) * log_2pi + 2 * sum(log(diag(L))) + dist)
  }
  out
}

# Adds the given patterns to `omega` and `omega_mu`, each with the weight
# freq / n_total of lavaan's Omega. The matrix of a pattern is
# a a' + Sigma_oo^-1 S Sigma_oo^-1 - Sigma_oo^-1 with
# a = Sigma_oo^-1 (ybar - mu), and S is zero for a pattern of one case.
fiml_direct_omega <- function(pats, mu, Sigma, omega, omega_mu, n_total) {
  for (pat in pats) {
    o <- pat$o
    S_inv <- chol2inv(chol(Sigma[o, o, drop = FALSE]))
    a <- S_inv %*% (pat$my - mu[o])
    omega_o <- tcrossprod(a) - S_inv
    if (!is.null(pat$sy)) {
      omega_o <- omega_o + S_inv %*% pat$sy %*% S_inv
    }
    w <- pat$freq / n_total
    omega[o, o] <- omega[o, o] + w * omega_o
    omega_mu[o, 1L] <- omega_mu[o, 1L] + w * a
  }
  list(omega = omega, omega_mu = omega_mu)
}

## ----- Kernels ---------------------------------------------------------------

# Only the first group's Sigma is checked upstream, so a failed factorisation
# in another group returns the optimiser's sentinel. A group whose Sigma has
# no Cholesky factor is scored on the direct route, which needs only the
# observed blocks.
fiml_loglik_samp <- function(lavsamplestats, lavimplied, plan = NULL) {
  if (is.null(plan)) {
    plan <- fiml_plan(lavsamplestats)
  }
  tryCatch(
    {
      out <- 0
      for (g in seq_along(plan)) {
        plan_g <- plan[[g]]
        mu <- as.numeric(lavimplied$mean[[g]])
        Sigma <- lavimplied$cov[[g]]
        shared <- fiml_shared(plan_g, mu, Sigma)
        out <- if (is.null(shared)) {
          fiml_direct_loglik(plan_g$pats, mu, Sigma, out)
        } else {
          fiml_direct_loglik(
            plan_g$pats[plan_g$direct],
            mu,
            Sigma,
            out + shared$loglik
          )
        }
      }
      out
    },
    error = function(e) -1e40
  )
}

# lavaan's Omega and Omega_mu (lav_model_omega(), FIML branch), one list
# element per group
fiml_omega_samp <- function(lavsamplestats, lavimplied, plan = NULL) {
  if (is.null(plan)) {
    plan <- fiml_plan(lavsamplestats)
  }
  n_total <- lavsamplestats@ntotal
  lapply(seq_along(plan), function(g) {
    plan_g <- plan[[g]]
    mu <- as.numeric(lavimplied$mean[[g]])
    Sigma <- lavimplied$cov[[g]]
    shared <- fiml_shared(plan_g, mu, Sigma, grad = TRUE)
    if (is.null(shared)) {
      fiml_direct_omega(
        plan_g$pats,
        mu,
        Sigma,
        matrix(0, plan_g$p, plan_g$p),
        matrix(0, plan_g$p, 1L),
        n_total
      )
    } else {
      fiml_direct_omega(
        plan_g$pats[plan_g$direct],
        mu,
        Sigma,
        shared$omega / n_total,
        matrix(shared$omega_mu / n_total, plan_g$p, 1L),
        n_total
      )
    }
  })
}

# Gradient of lavaan's FIML fit function w.r.t. the free parameters (unpacked
# under ceq.simple), or NULL when a pattern's block cannot be factorised, in
# which case the caller falls back to lav_model_grad()
fiml_grad_samp <- function(lavmodel_x, lavsamplestats, plan = NULL) {
  om <- tryCatch(
    fiml_omega_samp(
      lavsamplestats,
      lavaan::lav_model_implied(lavmodel_x),
      plan
    ),
    error = function(e) NULL
  )
  if (is.null(om)) {
    return(NULL) # nocov
  }
  glist <- lavmodel_x@GLIST
  nmat <- lavmodel_x@nmat
  unco <- isTRUE(lavmodel_x@ceq.simple.only)
  dx <- numeric(if (unco) lavmodel_x@nx.unco else lavmodel_x@nx.free)
  for (g in seq_len(lavmodel_x@nblocks)) {
    mm_in_group <- seq_len(nmat[g]) + cumsum(c(0, nmat))[g]
    dx_group <- lavaan___lav_lisrel_df_dmlist(
      glist[mm_in_group],
      om[[g]]$omega,
      om[[g]]$omega_mu
    )
    dx_group <- dx_group[names(glist[mm_in_group])]
    for (k in seq_along(mm_in_group)) {
      mm <- mm_in_group[k]
      x_idx <- if (unco) {
        lavmodel_x@x.unco.idx[[mm]]
      } else {
        lavmodel_x@x.free.idx[[mm]]
      }
      dx[x_idx] <- dx_group[[k]][lavmodel_x@m.free.idx[[mm]]]
    }
  }
  dx
}
