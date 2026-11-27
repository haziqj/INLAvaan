# ---------------------------------------------------------------------------
# Shared helper: summarise a numeric vector of posterior samples into the
# standard (Mean, SD, 2.5%, 50%, 97.5%, Mode) list + pdf_data table.
# ---------------------------------------------------------------------------
summarise_samples <- function(y) {
  res <- rep(NA_real_, 8)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")
  # Fewer than two draws (e.g. a := parameter that is almost nowhere defined)
  # leave the summary NA.
  if (length(y) < 2) {
    return(list(summary = res, pdf_data = NULL))
  }
  res[1:7] <- c(
    mean(y),
    stats::sd(y),
    stats::quantile(y, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  )
  # A constant (e.g. k := 2) has no density, and its mode is its value
  if (is_constant(y)) {
    res["Mode"] <- res["Mean"]
    return(list(summary = res, pdf_data = NULL))
  }
  dens <- stats::density(y)
  res["Mode"] <- dens$x[which.max(dens$y)]
  list(
    summary = res,
    pdf_data = data.frame(x = dens$x, y = dens$y)
  )
}

# Draws that do not vary beyond rounding error relative to their size. All zeros
# count as constant.
is_constant <- function(y) {
  stats::sd(y) <= sqrt(.Machine$double.eps) * max(abs(y))
}

# ---------------------------------------------------------------------------
# Core sampling function: draw from the Gaussian-copula posterior through the
# engine's sn_draws(), which couples the marginals by the correlation matrix
# of Omega. The skew-normal marginals are the first m rows of approx_data:
# the rows after them are the fits to the draws of covariances and defined
# parameters (sn_fit_sample). Gaussian marginals (method "gaussian", or
# "marggaus" and "sampling" in fits made before 0.4.0) are those of the
# Laplace approximation: marginals at theta_star with the Laplace SDs and
# alpha = 0, which sn_draws() maps linearly. Returns a list with:
#   $theta_samp  - nsamp x m matrix in the internal (theta) parameterisation
#   $x_samp      - nsamp x p matrix in the lavaan (x) parameterisation
# ---------------------------------------------------------------------------
sample_params <- function(
  theta_star,
  Omega,
  method,
  approx_data,
  pt,
  lavmodel,
  nsamp = 1000
) {
  m <- length(theta_star)
  corr <- stats::cov2cor(Omega)
  if (method == "skewnorm") {
    sn <- approx_data[seq_len(m), c("xi", "omega", "alpha"), drop = FALSE]
  } else if (method %in% c("gaussian", "marggaus", "sampling")) {
    sn <- cbind(xi = theta_star, omega = sqrt(diag(Omega)), alpha = 0)
  } else {
    # nocov start
    cli_abort("Unknown marginal method {.val {method}}.", .internal = TRUE)
  } # nocov end
  theta <- unname(sn_draws(sn, Omega, nsamp, corr = corr))

  # theta stays packed (one column per free parameter), and pars_to_x() takes it
  # unpacked to every free row.
  theta_full <- theta
  if (lavmodel@ceq.simple.only) {
    K <- lavmodel@ceq.simple.K
    theta_full <- tcrossprod(theta, K)
  }

  x <- apply_rows(
    theta_full,
    pars_to_x,
    pt = pt,
    map = pars_to_x_map(pt)
  )

  list(theta_samp = theta, x_samp = x)
}

## ----- PPP and DIC over the posterior draws ---------------------------------

# The single-level PPP (after Levy 2011) and the DIC both score every posterior
# draw x_s, and both need the implied moments of x_s. score_draws() therefore
# goes over the draws once: per draw it sets the parameters and computes the
# implied moments, then takes the PPP exceedance (ppp_draw()) and the
# log-likelihood (implied_loglik()) from them.
#
# PPP. With Sigma_s the implied covariance of x_s and Srep_s one replicate
# drawn from Wishart(n - 1, Sigma_s) / (n - 1), the discrepancy is
#   F(S, Sigma) = log|Sigma| + tr(Sigma^{-1} S) - log|S| - p
# and PPP = mean over s of 1{F(Srep_s, Sigma_s) >= F(S, Sigma_s)}.
#
# DIC (Spiegelhalter et al. 2002). D(x) = -2 log p(y | x), Dbar is the mean of
# D(x_s) over the draws whose log-likelihood does not fail, Dhat = D(x_hat) at
# the VB mean, pD = Dbar - Dhat and DIC = Dbar + pD.
#
# The log-likelihood of each draw without the term of the marginalised
# saturated means is kept (loglik_samp), because the per-draw chi-square of
# bfit_indices() is that log-likelihood (see compute_chisq_dev()).

# The parts of the PPP that do not change across draws: per block the observed
# covariance S with divisor n - 1, n, log|S| and the positions of the fixed
# covariates, and the plan for composites whose indicator (co)variances are
# fixed at the sample covariance (see ppp_composite_draw()).
ppp_setup <- function(lavmodel, lavsamplestats, lavdata, lavpartable = NULL) {
  n_blocks <- lavmodel@nblocks
  has_missing <- isTRUE(lavsamplestats@missing.flag)
  block_obs <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    g <- b
    n <- lavsamplestats@nobs[[g]]
    # The observed covariance: the EM (saturated) estimate under FIML,
    # the plain sample covariance otherwise. Both have divisor n, and
    # the replicates are Wishart(n - 1, Sigma) / (n - 1) objects,
    # so rescale to the unbiased (n - 1) form to match.
    S <- if (has_missing && !is.null(lavsamplestats@missing.h1[[g]]$sigma)) {
      lavsamplestats@missing.h1[[g]]$sigma
    } else {
      lavsamplestats@cov[[g]]
    }
    S <- S * n / (n - 1)
    block_obs[[b]] <- list(
      S = S,
      n = n,
      logdet_S = as.numeric(determinant(S, logarithm = TRUE)$modulus),
      x_idx = lavsamplestats@x.idx[[g]]
    )
  }
  t_fixed <- composite_fixed_t(lavmodel, lavpartable, lavdata)
  list(
    block_obs = block_obs,
    t_fixed = t_fixed,
    scale_plan = if (!is.null(t_fixed)) {
      composite_scale_plan(lavmodel, lavpartable)
    }
  )
}

# The PPP exceedance of one draw xx: 1 when F(Srep, Sigma) >= F(S, Sigma) and
# 0 otherwise. lavmodel_x is lavmodel at xx and lavimplied its implied
# moments. plan comes from ppp_setup(). A block whose implied covariance is not
# positive definite is left out of both discrepancies.
ppp_draw <- function(xx, lavmodel_x, lavimplied, lavmodel, plan) {
  block_obs <- plan$block_obs
  if (!is.null(plan$t_fixed)) {
    return(ppp_composite_draw(
      xx,
      lavmodel_x,
      lavimplied,
      lavmodel,
      plan$t_fixed,
      block_obs,
      plan$scale_plan
    ))
  }
  Tobs <- 0
  Trep <- 0
  for (b in seq_along(block_obs)) {
    S <- block_obs[[b]]$S
    n <- block_obs[[b]]$n
    x_idx <- block_obs[[b]]$x_idx
    Sigma <- implied_joint_moments(lavimplied, b, x_idx)$cov
    # One Cholesky factor for the check, the log-determinant, the inverse and
    # both discrepancies
    L <- chol_or_null(Sigma)
    if (is.null(L)) {
      next
    }
    p <- nrow(Sigma)
    logdet_Sigma <- 2 * sum(log(diag(L)))
    Sigma_inv <- chol2inv(L)

    W <- matrix(stats::rWishart(1, df = n - 1, Sigma = Sigma)[,, 1], p, p)
    Srep <- W / (n - 1)
    logdet_Srep <- as.numeric(determinant(Srep, logarithm = TRUE)$modulus)
    Tobs <- Tobs +
      logdet_Sigma +
      sum(Sigma_inv * S) -
      block_obs[[b]]$logdet_S -
      p
    Trep <- Trep + logdet_Sigma + sum(Sigma_inv * Srep) - logdet_Srep - p

    # Fixed covariates are conditioned on, not modelled, so the discrepancy
    # is that of the outcomes given the covariates: F less its covariate
    # block. The replicates vary that block too, and without this they
    # would carry extra misfit and push the PPP up.
    if (length(x_idx) > 0L) {
      Sigma_xx <- Sigma[x_idx, x_idx, drop = FALSE]
      Tobs <- Tobs - ml_discrepancy(S[x_idx, x_idx, drop = FALSE], Sigma_xx)
      Trep <- Trep - ml_discrepancy(Srep[x_idx, x_idx, drop = FALSE], Sigma_xx)
    }
  }
  as.numeric(Trep >= Tobs)
}

# The plan of complete_loglik(): per group the sample covariance and mean, n,
# and the log-likelihood of the fixed covariates, which lavaan subtracts and
# which does not change across draws. NULL where the closed form does not
# apply: an estimator other than ML, two levels, missing data, conditional.x,
# random slopes, or an exogenous categorical variable under fixed.x = FALSE
# (lavaan has no log-likelihood there).
complete_loglik_setup <- function(
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions
) {
  exo_categorical <- !isTRUE(lavoptions$fixed.x) &&
    any(lavdata@ov$exo == 1L & lavdata@ov$nlev > 1L)
  if (
    lavmodel@estimator != "ML" ||
      lavdata@nlevels > 1L ||
      isTRUE(lavsamplestats@missing.flag) ||
      isTRUE(lavmodel@conditional.x) ||
      has_random_slopes(lavmodel) ||
      exo_categorical
  ) {
    return(NULL)
  }
  groups <- lapply(seq_len(lavdata@ngroups), function(g) {
    x_idx <- lavsamplestats@x.idx[[g]]
    mean_g <- as.numeric(lavsamplestats@mean[[g]])
    cov_g <- lavsamplestats@cov[[g]]
    loglik_x <- 0
    if (length(x_idx) > 0L) {
      # Positional arguments: their names differ across lavaan versions
      loglik_x <- lavaan___lav_mvn_loglik_samp(
        mean_g[x_idx],
        cov_g[x_idx, x_idx, drop = FALSE],
        lavsamplestats@nobs[[g]],
        lavsamplestats@mean.x[[g]],
        lavsamplestats@cov.x[[g]]
      )
    }
    list(
      S = cov_g,
      ybar = mean_g,
      n = lavsamplestats@nobs[[g]],
      loglik_x = loglik_x
    )
  })
  list(groups = groups, meanstructure = isTRUE(lavmodel@meanstructure))
}

# The multivariate normal log-likelihood of single-level complete data from the
# sample statistics, as lavaan's lav_mvn_loglik_samp() computes it, but with a
# Cholesky factor of Sigma in place of lavaan's eigen decomposition:
#   sum over groups of -n/2 [p log(2 pi) + log|Sigma| + tr(Sigma^{-1} S)
#                            + (ybar - mu)' Sigma^{-1} (ybar - mu)] - loglik_x.
# Without a mean structure mu is the sample mean. The value agrees with
# lavaan's to rounding. Returns -1e40 when an implied covariance is not
# positive definite or the value is not finite. plan comes from
# complete_loglik_setup().
complete_loglik <- function(lavimplied, plan) {
  out <- 0
  for (g in seq_along(plan$groups)) {
    grp <- plan$groups[[g]]
    L <- chol_or_null(lavimplied$cov[[g]])
    if (is.null(L)) {
      return(-1e40)
    }
    Sigma_inv <- chol2inv(L)
    dist <- sum(Sigma_inv * grp$S)
    if (plan$meanstructure) {
      d <- grp$ybar - as.numeric(lavimplied$mean[[g]])
      dist <- dist + sum(as.numeric(crossprod(d, Sigma_inv)) * d)
    }
    logdet <- 2 * sum(log(diag(L)))
    p <- length(grp$ybar)
    out <- out + (-grp$n / 2 * (p * log(2 * pi) + logdet + dist) - grp$loglik_x)
  }
  if (!is.finite(out)) {
    return(-1e40)
  }
  out
}

# The plan of independence_loglik(). In an independence model every free
# parameter is a variance or an intercept and Sigma is diagonal, so the
# implied variances and means are linear in the free parameters: the implied
# moments at x = 0 and at each unit vector give them, as v0 + x A and m0 + x B
# per group. Per variable it keeps the data the log-likelihood needs: n, the
# sample mean and variance (complete data), or the number, sum and sum of
# squares of the observed values (missing data, which has a mean structure).
# NULL where this does not apply: not an independence model, an estimator other
# than ML, two levels, conditional.x, random slopes, fixed covariates, missing
# data without a mean structure, or an implied covariance with an off-diagonal
# entry.
independence_loglik_setup <- function(lavmodel, lavsamplestats, lavdata, pt) {
  if (
    !is_independence_partable(pt) ||
      lavmodel@estimator != "ML" ||
      lavdata@nlevels > 1L ||
      isTRUE(lavmodel@conditional.x) ||
      has_random_slopes(lavmodel) ||
      any(lengths(lavsamplestats@x.idx) > 0L) ||
      (isTRUE(lavsamplestats@missing.flag) && !isTRUE(lavmodel@meanstructure))
  ) {
    return(NULL)
  }
  k <- lavmodel@nx.free
  at <- rbind(0, diag(k))
  implied <- lapply(seq_len(k + 1L), function(r) {
    lavaan::lav_model_implied(lavaan::lav_model_set_parameters(
      lavmodel,
      at[r, ]
    ))
  })
  missing <- isTRUE(lavsamplestats@missing.flag)
  groups <- vector("list", lavdata@ngroups)
  for (g in seq_along(groups)) {
    covs <- lapply(implied, function(e) e$cov[[g]])
    if (any(vapply(covs, function(m) any(m[row(m) != col(m)] != 0), NA))) {
      return(NULL)
    }
    v0 <- diag(covs[[1]])
    A <- do.call(rbind, lapply(covs[-1], function(m) diag(m) - v0))
    grp <- list(v0 = v0, A = A, n = lavsamplestats@nobs[[g]])
    if (isTRUE(lavmodel@meanstructure)) {
      means <- lapply(implied, function(e) as.numeric(e$mean[[g]]))
      grp$m0 <- means[[1]]
      grp$B <- do.call(rbind, lapply(means[-1], function(m) m - means[[1]]))
    }
    if (missing) {
      X <- lavdata@X[[g]]
      obs <- !is.na(X)
      X[!obs] <- 0
      grp$n_obs <- colSums(obs)
      grp$sum1 <- colSums(X)
      grp$sum2 <- colSums(X^2)
    } else {
      grp$ybar <- as.numeric(lavsamplestats@mean[[g]])
      grp$s2 <- diag(lavsamplestats@cov[[g]])
    }
    groups[[g]] <- grp
  }
  list(
    groups = groups,
    missing = missing,
    meanstructure = isTRUE(lavmodel@meanstructure)
  )
}

# The log-likelihood of every draw (rows of x_samp) of an independence model at
# once, as the sum over variables of univariate normal log-likelihoods: with
# complete data -n/2 [log(2 pi) + log(v) + (s2 + (ybar - m)^2) / v] per
# variable, and with missing data the same over the observed values of each
# variable. Without a mean structure m is the sample mean. Returns raw (the
# log-likelihood, -1e40 where a variance is not positive) and corr (the term of
# the marginalised saturated means, 0.5 log|Sigma| + 0.5 p log(2 pi / n) per
# group, as marginalised_means_loglik_corr() computes it). plan comes from
# independence_loglik_setup().
independence_loglik <- function(x_samp, plan) {
  raw <- corr <- numeric(nrow(x_samp))
  for (grp in plan$groups) {
    v <- sweep(x_samp %*% grp$A, 2L, grp$v0, "+")
    m <- if (plan$meanstructure) {
      sweep(x_samp %*% grp$B, 2L, grp$m0, "+")
    }
    log_v <- suppressWarnings(log(v))
    if (plan$missing) {
      n_obs <- rep(grp$n_obs, each = nrow(v))
      ss <- rep(grp$sum2, each = nrow(v)) -
        2 * m * rep(grp$sum1, each = nrow(v)) +
        n_obs * m^2
      raw <- raw - 0.5 * rowSums(n_obs * (log(2 * pi) + log_v) + ss / v)
    } else {
      dev <- rep(grp$s2, each = nrow(v))
      if (!is.null(m)) {
        dev <- dev + (rep(grp$ybar, each = nrow(v)) - m)^2
      }
      raw <- raw - grp$n / 2 * rowSums(log(2 * pi) + log_v + dev / v)
    }
    corr <- corr +
      0.5 * rowSums(log_v) +
      0.5 * ncol(v) * log(2 * pi / grp$n)
  }
  bad <- !is.finite(raw)
  raw[bad] <- -1e40
  corr[bad] <- 0
  list(raw = raw, corr = corr)
}

# One pass over the draws for the single-level PPP and the DIC. ppp_plan is
# the result of ppp_setup(), or NULL for no PPP. With dic = TRUE the DIC is
# computed with the plug-in at theta_star, and loglik_x_adj is added to every
# log-likelihood (the fixed-x constant of two-level fits). fiml and multilevel
# are the plans of the likelihood kernels. Returns list(ppp, dic), where dic is
# list(dic, Dbar, Dhat, pD, loglik_samp) or NULL.
score_draws <- function(
  x_samp,
  theta_star,
  pt,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache,
  fiml,
  multilevel,
  loglik_x_adj = 0,
  ppp_plan = NULL,
  dic = TRUE,
  cli_env = NULL
) {
  nsamp <- nrow(x_samp)
  do_ppp <- !is.null(ppp_plan)
  exceed <- if (do_ppp) numeric(nsamp)
  marg <- isTRUE(dic) &&
    lavmodel@estimator == "ML" &&
    marginalised_means_active(lavmodel)
  ll_raw <- ll <- if (isTRUE(dic)) numeric(nsamp)
  n_loop <- nsamp

  # The log-likelihood of one draw at its moments, without the term of the
  # marginalised means: in closed form for single-level complete data (see
  # complete_loglik()), and by implied_loglik() otherwise
  complete <- if (isTRUE(dic)) {
    complete_loglik_setup(lavmodel, lavsamplestats, lavdata, lavoptions)
  }
  draw_loglik <- function(lavmodel_x, lavimplied) {
    if (!is.null(complete)) {
      return(complete_loglik(lavimplied, complete))
    }
    implied_loglik(
      lavmodel_x,
      lavimplied,
      lavmodel,
      lavsamplestats,
      lavdata,
      lavoptions,
      lavcache,
      fiml = fiml,
      multilevel = multilevel
    )
  }
  full_loglik <- function(out, lavimplied) {
    if (marg && out != -1e40) {
      out <- out + marginalised_means_loglik_corr(lavimplied, lavsamplestats)
    }
    out + loglik_x_adj
  }

  # An independence model with no PPP (the baseline of bfit_indices()) is
  # scored for all draws at once. The first draw is checked against the route
  # of every other model, which is used where they disagree.
  indep <- if (isTRUE(dic) && !do_ppp) {
    independence_loglik_setup(lavmodel, lavsamplestats, lavdata, pt)
  }
  if (!is.null(indep)) {
    vec <- independence_loglik(x_samp, indep)
    lavmodel_1 <- lavaan::lav_model_set_parameters(lavmodel, x_samp[1, ])
    implied_1 <- lavaan::lav_model_implied(lavmodel_1)
    ll_1 <- full_loglik(draw_loglik(lavmodel_1, implied_1), implied_1)
    full <- ifelse(vec$raw == -1e40, -1e40, vec$raw + marg * vec$corr)
    if (abs(full[1] + loglik_x_adj - ll_1) <= 1e-8 * max(1, abs(ll_1))) {
      ll_raw <- vec$raw
      ll <- full + loglik_x_adj
      n_loop <- 0L
    }
  }

  for (i in seq_len(n_loop)) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env) # nocov
    }
    xx <- x_samp[i, ]
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x)
    if (do_ppp) {
      exceed[i] <- ppp_draw(xx, lavmodel_x, lavimplied, lavmodel, ppp_plan)
    }
    if (isTRUE(dic)) {
      ll_raw[i] <- draw_loglik(lavmodel_x, lavimplied)
      ll[i] <- full_loglik(ll_raw[i], lavimplied)
    }
  }

  dic_list <- NULL
  if (isTRUE(dic)) {
    xhat <- if (lavmodel@ceq.simple.only) {
      pars_to_x(as.numeric(lavmodel@ceq.simple.K %*% theta_star), pt) # nocov
    } else {
      pars_to_x(theta_star, pt)
    }
    lavmodel_hat <- lavaan::lav_model_set_parameters(lavmodel, xhat)
    implied_hat <- lavaan::lav_model_implied(lavmodel_hat)
    Dhat <- -2 *
      full_loglik(draw_loglik(lavmodel_hat, implied_hat), implied_hat)
    Dbar_samp <- -2 * ll
    Dbar <- mean(Dbar_samp[Dbar_samp < 1e40])
    pD <- Dbar - Dhat
    dic_list <- list(
      dic = Dbar + pD,
      Dbar = Dbar,
      Dhat = Dhat,
      pD = pD,
      loglik_samp = ll_raw
    )
  }

  list(ppp = if (do_ppp) mean(exceed), dic = dic_list)
}

# One posterior draw of the PPP for a fit with composites. lavaan fixes the
# (co)variances of the composite indicators at the sample covariance of the data
# it is given, so the observed data have no misfit in that block. A replicate is
# therefore scored against the moments implied with its own sample covariance
# there (divisor n, as lavaan uses), which is what lavaan would fix had it been
# given the replicate. Scored against the observed values instead, every
# replicate would carry a misfit in that block that the observed data cannot
# have, and the PPP would drift towards 1. The new block also moves the variance
# w'Tw of each composite, so the draw is rescaled with it (see
# composite_rescale_x()). Composites are single-level, so the blocks are the
# groups. lavmodel_x is lavmodel at xx. Returns 1 when Trep >= Tobs and 0
# otherwise. A replicate whose implied moments are not a covariance matrix
# counts as an exceedance (Trep = Inf).
ppp_composite_draw <- function(
  xx,
  lavmodel_x,
  lavimplied,
  lavmodel,
  t_fixed,
  block_obs,
  scale_plan
) {
  n_blocks <- length(block_obs)
  Tobs <- 0
  Srep <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    x_idx <- block_obs[[b]]$x_idx
    Sigma <- implied_joint_moments(lavimplied, b, x_idx)$cov
    if (is_bad_cov(Sigma)) {
      next
    }
    n <- block_obs[[b]]$n
    W <- stats::rWishart(1, df = n - 1, Sigma = Sigma)[,, 1]
    Srep[[b]] <- matrix(W, nrow(Sigma)) / (n - 1)
    Tobs <- Tobs + ppp_discrepancy(block_obs[[b]]$S, Sigma, x_idx)
  }

  lavmodel_rep <- lavmodel
  for (b in seq_len(n_blocks)) {
    e <- t_fixed[[b]]
    if (is.null(Srep[[b]]) || length(e$pos) == 0L) {
      next
    }
    n <- block_obs[[b]]$n
    lavmodel_rep@GLIST[[e$mm]][e$pos] <- Srep[[b]][e$rc] * (n - 1) / n
  }
  # lavaan re-derives the composite variances from the replaced block, and the
  # draw is rescaled to follow them.
  x_rows <- composite_rescale_x(
    xx,
    lavmodel_x,
    lavaan::lav_model_set_parameters(lavmodel_rep, xx),
    scale_plan
  )
  implied_rep <- lavaan::lav_model_implied(
    composite_set_rows(lavmodel_rep, x_rows, scale_plan)
  )

  Trep <- 0
  for (b in seq_len(n_blocks)) {
    if (is.null(Srep[[b]])) {
      next
    }
    x_idx <- block_obs[[b]]$x_idx
    Sigma_rep <- implied_joint_moments(implied_rep, b, x_idx)$cov
    if (is_bad_cov(Sigma_rep)) {
      return(1) # nocov
    }
    Trep <- Trep + ppp_discrepancy(Srep[[b]], Sigma_rep, x_idx)
  }
  as.numeric(Trep >= Tobs)
}

# The draw xx carried over to a replicate whose composite indicator blocks are
# its own. With them, the variance w'Tw of composite k changes by s_k^2, while a
# raw covariance with the composite, a loading on it or a path into it keeps its
# value. Every replicate would then carry an artificial misfit, and the PPP
# would drift towards 1. So these are rescaled to keep the composites'
# standardised relations at the draw's values. A covariance is multiplied by the
# scales of both sides, and a loading on a variable or a path into it by the
# variable's scale over that of the factor or predictor. A path out of a
# composite into anything other than a composite is left alone, because the
# response given the indicators does not depend on T. A factor whose scale is
# set by a fixed loading (its marker) takes the marker's scale, which then
# reaches its variance, its other loadings, its covariances and the paths out of
# it. Observed variables keep their scale. lavmodel_x and lavmodel_rep_x are the
# model at xx with the observed and the replicate blocks. Returns the rescaled
# draw by parameter table row (see composite_set_rows()).
composite_rescale_x <- function(xx, lavmodel_x, lavmodel_rep_x, scale_plan) {
  s <- composite_scales(lavmodel_x, lavmodel_rep_x, scale_plan)
  s_lhs <- s[scale_plan$lhs]
  s_lhs[is.na(s_lhs)] <- 1
  s_rhs <- s[scale_plan$rhs]
  s_rhs[is.na(s_rhs)] <- 1
  s_from <- ifelse(scale_plan$rhs_comp & !scale_plan$lhs_comp, 1, s_rhs)
  op <- scale_plan$op
  mult <- rep(1, length(op))
  mult[op == "~~"] <- (s_lhs * s_rhs)[op == "~~"]
  mult[op == "=~"] <- (s_rhs / s_lhs)[op == "=~"]
  mult[op == "~"] <- (s_lhs / s_from)[op == "~"]
  x_rows <- numeric(scale_plan$n_rows)
  x_rows[scale_plan$rows] <- xx[scale_plan$x_idx] * mult
  x_rows
}

# lavmodel at a draw given by parameter table row (x_rows) instead of the packed
# x. Rows held equal share a free index and so one entry of x, but the rescaled
# draw can need a different value for each, as for a covariance held equal
# across groups whose composites change scale by different amounts.
composite_set_rows <- function(lavmodel, x_rows, scale_plan) {
  lavmodel@x.free.idx <- scale_plan$x_free_rows
  lavaan::lav_model_set_parameters(lavmodel, x_rows)
}

# The scale in a replicate of each composite, sqrt(w'T_rep w / w'T w) at the
# draw's weights, and of each factor scaled by a marker, named by variable and
# block.
composite_scales <- function(lavmodel_x, lavmodel_rep_x, scale_plan) {
  s <- numeric(0L)
  for (b in seq_along(scale_plan$blocks)) {
    pb <- scale_plan$blocks[[b]]
    if (length(pb$comps) == 0L) {
      next # nocov
    }
    w <- lavmodel_x@GLIST[[pb$wmat]][, pb$comp_col, drop = FALSE]
    v_obs <- colSums(w * (lavmodel_x@GLIST[[pb$theta]] %*% w))
    v_rep <- colSums(w * (lavmodel_rep_x@GLIST[[pb$theta]] %*% w))
    s_b <- stats::setNames(
      c(sqrt(v_rep / v_obs), rep(1, length(pb$factors))),
      c(pb$comps, pb$factors)
    )
    # A marker can itself be a factor scaled by a marker (a higher-order chain),
    # so pass the scales along until they settle.
    for (k in seq_along(pb$factors)) {
      s_new <- s_b
      s_new[pb$factors] <- ifelse(
        pb$markers %in% names(s_b),
        s_b[pb$markers],
        1
      )
      if (identical(s_new, s_b)) {
        break
      }
      s_b <- s_new
    }
    s[paste(names(s_b), b)] <- s_b
  }
  s
}

# What composite_rescale_x() needs from the model, worked out once per fit. Per
# block: the GLIST indices of wmat and theta, the composites and their columns
# in wmat, and each factor whose scale is set by a fixed loading (its marker)
# because its (residual) variance is free. A factor with a fixed variance (as
# under std.lv) keeps its scale. Then the free rows of the parameter table with
# their entries in the packed x, and the row of each free position in GLIST
# (x_free_rows, for composite_set_rows()). `pt` is lavaan's parameter table.
composite_scale_plan <- function(lavmodel, pt) {
  nmat <- lavmodel@nmat
  offset <- cumsum(c(0L, nmat))
  blocks <- lapply(seq_len(lavmodel@nblocks), function(b) {
    mm_b <- offset[b] + seq_len(nmat[b])
    mm_wmat <- mm_b[names(lavmodel@GLIST)[mm_b] == "wmat"]
    in_b <- pt$block == b
    comps <- unique(pt$lhs[in_b & pt$op == "<~"])
    var_free <- pt$lhs[in_b & pt$op == "~~" & pt$lhs == pt$rhs & pt$free > 0L]
    marker <- which(
      in_b &
        pt$op == "=~" &
        pt$free == 0L &
        !is.na(pt$ustart) &
        pt$ustart != 0 &
        pt$lhs %in% var_free
    )
    marker <- marker[!duplicated(pt$lhs[marker])]
    list(
      wmat = mm_wmat,
      theta = mm_b[names(lavmodel@GLIST)[mm_b] == "theta"],
      comps = comps,
      comp_col = match(comps, lavmodel@dimNames[[mm_wmat]][[2L]]),
      factors = pt$lhs[marker],
      markers = pt$rhs[marker]
    )
  })
  rows <- which(pt$free > 0L)
  comp_keys <- unlist(lapply(seq_along(blocks), function(b) {
    paste(blocks[[b]]$comps, b)
  }))
  lhs <- paste(pt$lhs[rows], pt$block[rows])
  rhs <- paste(pt$rhs[rows], pt$block[rows])
  list(
    blocks = blocks,
    rows = rows,
    n_rows = length(pt$lhs),
    x_idx = pt$free[rows],
    op = pt$op[rows],
    lhs = lhs,
    rhs = rhs,
    lhs_comp = lhs %in% comp_keys,
    rhs_comp = rhs %in% comp_keys,
    x_free_rows = lapply(seq_along(lavmodel@GLIST), function(mm) {
      lavmodel@x.user.idx[[mm]][
        match(lavmodel@m.free.idx[[mm]], lavmodel@m.user.idx[[mm]])
      ]
    })
  )
}

# F(S, Sigma) of the outcomes given the fixed covariates at x_idx, which is F
# less its covariate block, as in ppp_draw().
ppp_discrepancy <- function(S, Sigma, x_idx) {
  out <- ml_discrepancy(S, Sigma)
  if (length(x_idx) > 0L) {
    out <- out -
      ml_discrepancy(
        S[x_idx, x_idx, drop = FALSE],
        Sigma[x_idx, x_idx, drop = FALSE]
      )
  }
  out
}

# Rows of block b of a parameter table that hold a composite indicator
# (co)variance fixed at its sample value (lavaan's composites.cov = "fixed").
# Such a row joins two indicators of the same composite, and lavaan leaves its
# value to the data (ustart NA). A (co)variance the user fixes at a value of
# their own is a constraint, not a plug-in of the data, so it is not included.
composite_t_rows <- function(pt, b) {
  in_b <- pt$block == b
  is_weight <- in_b & pt$op == "<~"
  composite_of <- stats::setNames(pt$lhs[is_weight], pt$rhs[is_weight])
  comp_lhs <- unname(composite_of[pt$lhs])
  comp_rhs <- unname(composite_of[pt$rhs])
  which(
    in_b &
      pt$op == "~~" &
      pt$free == 0L &
      is.na(pt$ustart) &
      !is.na(comp_lhs) &
      !is.na(comp_rhs) &
      comp_lhs == comp_rhs
  )
}

# The composite indicator (co)variances fixed at the sample covariance, per
# block: the index of the block's theta matrix in GLIST (mm), their positions in
# it (pos, both triangles), the matching rows and columns of the block's
# observed covariance (rc, one row per position) and their parameter-table rows
# (rows). These entries are a plug-in of the data rather than parameters. NULL
# for a fit without them. Composites are single-level, so the blocks are the
# groups. `pt` is lavaan's parameter table or INLAvaan's (same rows).
composite_fixed_t <- function(lavmodel, pt, lavdata) {
  # A fit saved under a lavaan without composites lacks the slot
  has_composites <- .hasSlot(lavmodel, "composites") &&
    isTRUE(lavmodel@composites)
  if (!has_composites || is.null(pt) || lavdata@nlevels > 1L) {
    return(NULL)
  }
  nmat <- lavmodel@nmat
  offset <- cumsum(c(0L, nmat))
  out <- lapply(seq_len(lavmodel@nblocks), function(b) {
    mm_b <- offset[b] + seq_len(nmat[b])
    mm <- mm_b[names(lavmodel@GLIST)[mm_b] == "theta"]
    user_idx <- lavmodel@x.user.idx[[mm]]
    keep <- user_idx %in% composite_t_rows(pt, b)
    rows <- user_idx[keep]
    ov <- lavdata@ov.names[[b]]
    list(
      mm = mm,
      pos = lavmodel@m.user.idx[[mm]][keep],
      rc = cbind(match(pt$lhs[rows], ov), match(pt$rhs[rows], ov)),
      rows = unique(rows)
    )
  })
  if (all(vapply(out, function(e) length(e$pos) == 0L, logical(1)))) {
    return(NULL)
  }
  out
}

# F(S, Sigma) = log|Sigma| + tr(Sigma^{-1} S) - log|S| - p
ml_discrepancy <- function(S, Sigma) {
  L <- chol(Sigma)
  logdet_Sigma <- 2 * sum(log(diag(L)))
  logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
  logdet_Sigma + sum(chol2inv(L) * S) - logdet_S - nrow(S)
}

# The implied covariance and mean (NULL without a mean structure) of all
# observed variables of block b, with the fixed covariates at x_idx. Under
# conditional.x lavaan implies only the outcomes given the covariates (res.cov,
# res.int, res.slopes) and the covariate moments, so the joint moments are
# rebuilt from those. (lavimplied$cov and $mean would partially match cov.x and
# mean.x, hence [["cov"]] and [["mean"]].)
implied_joint_moments <- function(lavimplied, b, x_idx) {
  if (!is.null(lavimplied[["cov"]])) {
    return(list(
      cov = lavimplied[["cov"]][[b]],
      mean = lavimplied[["mean"]][[b]]
    ))
  }
  res_cov <- lavimplied$res.cov[[b]]
  res_int <- as.numeric(lavimplied$res.int[[b]])
  if (length(x_idx) == 0L) {
    return(list(cov = res_cov, mean = res_int)) # nocov
  }
  B <- lavimplied$res.slopes[[b]]
  Sigma_xx <- lavimplied$cov.x[[b]]
  mu_x <- as.numeric(lavimplied$mean.x[[b]])
  p <- nrow(res_cov) + nrow(Sigma_xx)
  y_idx <- setdiff(seq_len(p), x_idx)
  BS <- B %*% Sigma_xx
  Sigma <- matrix(0, p, p)
  Sigma[y_idx, y_idx] <- res_cov + tcrossprod(BS, B)
  Sigma[y_idx, x_idx] <- BS
  Sigma[x_idx, y_idx] <- t(BS)
  Sigma[x_idx, x_idx] <- Sigma_xx
  mu <- numeric(p)
  mu[y_idx] <- res_int + as.numeric(B %*% mu_x)
  mu[x_idx] <- mu_x
  list(cov = Sigma, mean = mu)
}

# implied_joint_moments() for block b of a fitted model. Only a conditional.x
# fit lacks the joint moments, and it is single-level, so its blocks are its
# groups.
implied_block_moments <- function(lavimplied, b, lavmodel, lavsamplestats) {
  x_idx <- if (isTRUE(lavmodel@conditional.x)) {
    lavsamplestats@x.idx[[b]]
  } else {
    integer(0L)
  }
  implied_joint_moments(lavimplied, b, x_idx)
}

sample_covariances <- function(x_samp, pt) {
  pt_cov_rows <- grep("cov", pt$mat)
  # One per free parameter (covariances held equal share a free index)
  pt_cov_free_rows <- pt_cov_rows[
    pt$free[pt_cov_rows] > 0 & !duplicated(pt$free)[pt_cov_rows]
  ]
  idxcov <- pt$free[pt_cov_free_rows]

  cov_samp <- x_samp[, idxcov, drop = FALSE]
  colnames(cov_samp) <- pt$names[pt_cov_free_rows]

  apply(cov_samp, 2, summarise_samples)
}

# Draws of the defined (:=) parameters, one column per := row. lavaan's own
# def.function maps x to the definitions in dependency order, so a definition
# may use another defined parameter.
sample_defpars <- function(x_samp, pt, lavmodel) {
  pt_def_rows <- which(pt$op == ":=")
  def_samp <- muffle_nan_warnings(do.call(
    "rbind",
    lapply(seq_len(nrow(x_samp)), function(i) {
      lavmodel@def.function(x_samp[i, ])
    })
  ))
  def_samp <- def_samp[, pt$lhs[pt_def_rows], drop = FALSE]
  colnames(def_samp) <- pt$names[pt_def_rows]
  def_samp
}

# The draws of each defined parameter where it is defined, with the share of
# undefined draws as attribute "undefined" (named by labels). A definition can
# be undefined for part of the posterior, such as log(b) when b can be negative.
# lavaan's def.function returns Inf there, so any non-finite draw counts as
# undefined. Dropping those draws summarises the posterior given that the
# parameter is defined.
defined_draws <- function(def_samp, labels = colnames(def_samp)) {
  ok <- is.finite(def_samp)
  share <- setNames(colMeans(!ok), labels)
  if (any(share > 0)) {
    warn_undefined_draws(share[share > 0], colSums(ok)[share > 0])
  }
  out <- lapply(
    setNames(seq_len(ncol(def_samp)), colnames(def_samp)),
    function(j) def_samp[ok[, j], j]
  )
  attr(out, "undefined") <- share
  out
}

# One warning for the := parameters that could not be computed for every
# posterior draw. share is named by the user's := labels.
warn_undefined_draws <- function(share, n_defined, scale = NULL) {
  bullets <- paste0(
    "{.code ",
    cli_escape(names(share)),
    "}: undefined in ",
    format_share(share),
    ifelse(n_defined < 2, ", so its summary is NA", "")
  )
  names(bullets) <- rep("*", length(bullets))
  on_scale <- if (is.null(scale)) "" else paste0(" (", cli_escape(scale), ")")
  cli_warn(c(
    paste0(
      "Some defined parameters could not be computed for every posterior ",
      "draw",
      on_scale,
      "."
    ),
    bullets,
    "i" = "Summaries use only the draws where each parameter is defined."
  ))
}

# Share of draws as text, e.g. "16.3% of draws" or "every draw"
format_share <- function(share) {
  pct <- sprintf("%.1f%%", 100 * share)
  pct[share < 0.001] <- "<0.1%"
  pct[share > 0.999] <- ">99.9%"
  out <- paste(pct, "of draws")
  out[share >= 1] <- "every draw"
  out
}

# Escape braces so cli prints text such as a := label literally
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

# Evaluate expr without R's "NaNs produced" warnings, one per draw where a :=
# parameter is undefined. defined_draws() reports those draws once instead.
muffle_nan_warnings <- function(expr) {
  nan_msg <- gettext("NaNs produced", domain = "R")
  withCallingHandlers(expr, warning = function(w) {
    if (identical(conditionMessage(w), nan_msg)) {
      invokeRestart("muffleWarning")
    }
  })
}

# Summaries of the defined parameters, each over the draws where it is defined.
# Attribute "undefined" holds the share of undefined draws.
get_defpars <- function(x_samp, pt, lavmodel, summarise = summarise_samples) {
  def_draws <- defined_draws(
    sample_defpars(x_samp, pt, lavmodel),
    labels = pt$lhs[pt$op == ":="]
  )
  out <- lapply(def_draws, summarise)
  attr(out, "undefined") <- attr(def_draws, "undefined")
  out
}

sample_covariances_fit_sn <- function(x_samp, pt) {
  pt_cov_rows <- grep("cov", pt$mat)
  # One per free parameter (covariances held equal share a free index)
  pt_cov_free_rows <- pt_cov_rows[
    pt$free[pt_cov_rows] > 0 & !duplicated(pt$free)[pt_cov_rows]
  ]
  idxcov <- pt$free[pt_cov_free_rows]

  cov_samp <- x_samp[, idxcov, drop = FALSE]
  colnames(cov_samp) <- pt$names[pt_cov_free_rows]

  apply(cov_samp, 2, summarise_samples_sn)
}

# Like summarise_samples(), but from a skew-normal fitted to the draws by
# maximum likelihood. Also returns the fitted parameters (sn_params).
# FIXME: Repeated code in post_marg_skewnorm
summarise_samples_sn <- function(y) {
  # A skew-normal needs draws that vary (a constant := parameter does not), so
  # summarise other draws directly.
  if (length(y) < 2 || is_constant(y)) {
    out <- summarise_samples(y)
    out$sn_params <- approx_data_row(NULL)
    return(out)
  }
  sn <- fit_skew_normal_samp(y)
  xi <- sn$xi
  omega <- sn$omega
  alpha <- sn$alpha
  delta <- alpha / sqrt(1 + alpha^2)

  Ex <- xi + omega * delta * sqrt(2 / pi)
  Vx <- omega^2 * (1 - 2 * delta^2 / pi)
  SDx <- sqrt(Vx)
  qq <- qsnorm(
    c(0.025, 0.25, 0.5, 0.75, 0.975),
    xi = xi,
    omega = omega,
    alpha = alpha
  )

  x <- seq(Ex - 4 * SDx, Ex + 4 * SDx, length.out = 200)
  fx <- dsnorm(x, xi = xi, omega = omega, alpha = alpha)

  xmax <- density_mode(
    function(x) dsnorm(x, xi = xi, omega = omega, alpha = alpha),
    range(x)
  )

  res <- c(Ex, SDx, qq, xmax)
  names(res) <- c("Mean", "SD", "2.5%", "25%", "50%", "75%", "97.5%", "Mode")

  list(
    summary = res,
    pdf_data = data.frame(x = x, y = fx),
    sn_params = approx_data_row(list(xi = xi, omega = omega, alpha = alpha))
  )
}


get_thetaparamerization_deltas <- function(x_samp, lavmodel) {
  delta_samp <- apply(x_samp, 1, function(xx) {
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x, delta = FALSE)
    Sigma_list <- lavimplied$cov
    ngroups <- length(Sigma_list)
    delta_list <- vector("list", ngroups)
    for (g in seq_len(ngroups)) {
      delta_list[[g]] <- sqrt(diag(Sigma_list[[g]]))
    }
    unlist(delta_list)
  })

  apply(delta_samp, 1, summarise_samples)
}
