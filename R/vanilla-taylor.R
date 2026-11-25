# Leave-one-out cross-validation and WAIC by a second-order Taylor expansion
# of each unit's log-likelihood about the fitted Gaussian N(theta, Omega), with
# no refits and no draws (Alhyari, Jamil, Montcho & Rue, 2026). A unit is an
# observation, or a cluster for leave-one-cluster-out. The caller takes each
# unit's log-likelihood l_u, score s_u and Hessian H_u at theta by whatever
# route it has; taylor_terms() turns them into the per-unit terms, and
# taylor_loo() and taylor_waic() into the estimates.
#
# Alhyari, M., Jamil, H., Montcho, H., & Rue, H. (2026). Deterministic
# leave-one-cluster-out cross-validation for multilevel Bayesian structural
# equation models (arXiv:2609.00670 [stat.ME]). arXiv.
# https://doi.org/10.48550/arXiv.2609.00670

# The per-unit terms. `l` holds the n log-likelihoods, `s` the n x m matrix of
# scores (a row per unit), `H` the m x m x n array of Hessians, or NULL for the
# first order alone, and `Omega` the covariance. `root` is any A with
# A A' = Omega, by default cov_root(Omega). Every term is the same whichever
# root is taken, and with derivatives already in whitened coordinates (where
# Omega is the identity) the caller passes `Omega = root = diag(m)`.
#
# In whitened coordinates the score is A' s_u, and the eigenvalues lambda of
# A' H_u A give everything else: the leverages are k = -lambda, the
# eigenvalues of -Omega H_u, and with p the squares of the score along the
# eigenvectors,
#   log CPO_u = l_u - sum p / (1 + lambda) / 2 + sum log(1 + lambda) / 2,
#   lpd_u     = l_u + sum p / (1 - lambda) / 2 - sum log(1 - lambda) / 2,
# which exist only while every 1 + lambda, or every 1 - lambda, is positive
# (k_max < 1, or k_min > -1), and are NA otherwise. At first order,
# log CPO_u = l_u - s' Omega s / 2 and lpd_u = l_u + s' Omega s / 2.
#
# Returns a data frame with a row per unit: `l_star`, `quad` (s' Omega s),
# `log_cpo_1`, `lpd_1`, `log_cpo_2`, `lpd_2`, `det_term` (the determinant term
# of the log CPO, log |I + Omega H_u| / 2, NA where the log CPO is), `k_max`
# (at least 0), `k_min`, `k_sum` (the total leverage, -tr(Omega H_u)) and
# `k_ssq` (tr[(Omega H_u)^2]), with the second-order terms NA when `H` is
# NULL.
taylor_terms <- function(l, s, H = NULL, Omega, root = cov_root(Omega)) {
  n <- length(l)
  l <- unname(as.numeric(l))
  S <- s %*% root
  quad <- rowSums(S^2)
  log_cpo_2 <- lpd_2 <- det_term <- rep(NA_real_, n)
  k_max <- k_min <- k_sum <- k_ssq <- rep(NA_real_, n)
  if (!is.null(H)) {
    for (u in seq_len(n)) {
      Hw <- crossprod(root, H[,, u] %*% root)
      eg <- eigen(Hw, symmetric = TRUE)
      lam <- eg$values
      proj2 <- drop(crossprod(eg$vectors, S[u, ]))^2
      k_max[u] <- max(-lam, 0)
      k_min[u] <- min(-lam)
      k_sum[u] <- -sum(lam)
      k_ssq[u] <- sum(lam^2)
      if (all(1 + lam > 0)) {
        det_term[u] <- 0.5 * sum(log1p(lam))
        log_cpo_2[u] <- l[u] - 0.5 * sum(proj2 / (1 + lam)) + det_term[u]
      }
      if (all(1 - lam > 0)) {
        lpd_2[u] <- l[u] +
          0.5 * sum(proj2 / (1 - lam)) -
          0.5 * sum(log1p(-lam))
      }
    }
  }
  data.frame(
    l_star = l,
    quad = quad,
    log_cpo_1 = l - 0.5 * quad,
    lpd_1 = l + 0.5 * quad,
    log_cpo_2 = log_cpo_2,
    lpd_2 = lpd_2,
    det_term = det_term,
    k_max = k_max,
    k_min = k_min,
    k_sum = k_sum,
    k_ssq = k_ssq
  )
}

# The standard error of a sum over the units, as the loo package takes it
loo_se <- function(v) {
  sqrt(length(v) * stats::var(v))
}

# The LOO from the per-unit terms. It is second order for every unit, or, when
# any unit has no second-order log CPO (a leverage of 1 or more, where the
# case-deletion integral diverges), first order for every unit: a first-order
# stand-in for that unit alone would be wrong by an unbounded amount, and
# dropping it would score the model on fewer units. A missing second-order
# lpd is an artefact of the quadratic, so p_loo takes that unit's first-order
# term, s' Omega s. Returns the estimates (`elpd_loo`, `p_loo`, `looic`, with
# standard errors), the pointwise terms with each unit's largest leverage `k`,
# the order, and the units that sent it to first order.
taylor_loo <- function(terms) {
  cpo_ok <- !is.na(terms$log_cpo_2)
  lpd_ok <- !is.na(terms$lpd_2)
  second <- all(cpo_ok)
  log_cpo <- if (second) terms$log_cpo_2 else terms$log_cpo_1
  p_u <- if (second) {
    ifelse(lpd_ok, terms$lpd_2 - terms$log_cpo_2, terms$quad)
  } else {
    terms$quad
  }
  est <- rbind(
    elpd_loo = c(sum(log_cpo), loo_se(log_cpo)),
    p_loo = c(sum(p_u), loo_se(p_u)),
    looic = c(-2 * sum(log_cpo), 2 * loo_se(log_cpo))
  )
  colnames(est) <- c("Estimate", "SE")
  list(
    estimates = est,
    pointwise = cbind(
      elpd_loo = log_cpo,
      p_loo = p_u,
      looic = -2 * log_cpo,
      k = terms$k_max
    ),
    order = if (second) 2L else 1L,
    first_order = which(!cpo_ok)
  )
}

# The WAIC from the same terms. Per unit, p_waic = Var[l_u(theta)] =
# s' Omega s + tr[(Omega H_u)^2] / 2, and elpd_waic = lpd - p_waic. It reads
# no case-deletion term, so only the lpd must exist; where it does not, every
# estimate falls to first order, where elpd_waic equals the first-order LOO
# exactly. Returns the estimates (`elpd_waic`, `p_waic`, `waic`), the
# pointwise terms, the order, and the units that sent it to first order.
taylor_waic <- function(terms) {
  lpd_ok <- !is.na(terms$lpd_2)
  second <- all(lpd_ok)
  lpd <- if (second) terms$lpd_2 else terms$lpd_1
  p_w <- if (second) terms$quad + 0.5 * terms$k_ssq else terms$quad
  elpd_u <- lpd - p_w
  est <- rbind(
    elpd_waic = c(sum(elpd_u), loo_se(elpd_u)),
    p_waic = c(sum(p_w), loo_se(p_w)),
    waic = c(-2 * sum(elpd_u), 2 * loo_se(elpd_u))
  )
  colnames(est) <- c("Estimate", "SE")
  list(
    estimates = est,
    pointwise = cbind(elpd_waic = elpd_u, p_waic = p_w, waic = -2 * elpd_u),
    order = if (second) 2L else 1L,
    first_order = which(!lpd_ok)
  )
}

# The LOO or the WAIC (`what`) from the scores and the total leverage alone,
# with no second-order terms. To first order in the leverages,
# log |I + Omega H_u| is tr(Omega H_u) = -k_sum and the quadratic form is
# s' Omega s, so
#   log CPO_u = l_u - s' Omega s / 2 - k_sum / 2,
#   lpd_u     = l_u + s' Omega s / 2 - k_sum / 2,
# and p_u = s' Omega s for both criteria, which then agree unit by unit.
# Summed over the units, k_sum is the effective number of parameters of the
# likelihood, so this is the first-order total less half of it. What is left
# out is of second order in the leverages, so k_sum is reported as each
# unit's k, and the units at 1 or more are returned as `over`.
taylor_trace <- function(terms, what) {
  elpd_u <- terms$log_cpo_1 - 0.5 * terms$k_sum
  p_u <- terms$quad
  names_est <- if (what == "loo") {
    c("elpd_loo", "p_loo", "looic")
  } else {
    c("elpd_waic", "p_waic", "waic")
  }
  est <- rbind(
    c(sum(elpd_u), loo_se(elpd_u)),
    c(sum(p_u), loo_se(p_u)),
    c(-2 * sum(elpd_u), 2 * loo_se(elpd_u))
  )
  dimnames(est) <- list(names_est, c("Estimate", "SE"))
  pointwise <- cbind(elpd_u, p_u, -2 * elpd_u)
  colnames(pointwise) <- names_est
  if (what == "loo") {
    pointwise <- cbind(pointwise, k = terms$k_sum)
  }
  list(
    estimates = est,
    pointwise = pointwise,
    order = 1L,
    over = which(terms$k_sum >= 1)
  )
}
