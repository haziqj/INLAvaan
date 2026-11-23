inlav_model_loglik <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache = NULL,
  marginalise_means = TRUE
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- implied_block_moments(lavimplied, 1L, lavmodel, lavsamplestats)$cov

  out <- -1e40
  if (!is_bad_cov(Sigma)) {
    if (lavmodel@estimator == "ML") {
      # Multivariate normal log-likelihood. A random-slope model is scored
      # from the GLIST of `lavmodel` and from the per-cluster statistics in
      # `lavcache`, not from `lavimplied`, so both have to be the updated
      # ones -- otherwise the log-likelihood is constant in `x`. Passing
      # `lavmodel_x` is inert for every other model, which reads
      # `lavimplied`, itself already built from `lavmodel_x`.
      out <- if (fiml_fast_path(lavmodel, lavsamplestats, lavdata)) {
        fiml_loglik_samp(lavsamplestats, lavimplied)
      } else {
        lavaan___lav_model_loglik(
          lavdata = lavdata,
          lavsamplestats = lavsamplestats,
          lavimplied = lavimplied,
          lavmodel = lavmodel_x,
          lavoptions = lavoptions,
          lavcache = lavcache
        )$loglik
      }
      # is_bad_cov() above only inspects the fixed-slope implied covariance,
      # so a random-slope kernel can still return a non-finite value.
      if (!is.finite(out)) {
        out <- -1e40
      }
      if (
        out != -1e40 &&
          isTRUE(marginalise_means) &&
          marginalised_means_active(lavmodel)
      ) {
        out <- out + marginalised_means_loglik_corr(lavimplied, lavsamplestats)
      }
    } else if (lavmodel@estimator == "PML") {
      # Pairwise log-likelihood
      no_ord <- length(lavdata@ordered)
      kappa <- 1 / sqrt(no_ord) # scaling factor for PML
      fx <- lavaan___lav_model_objective(
        lavmodel = lavmodel_x,
        lavsamplestats = lavsamplestats,
        lavdata = lavdata,
        lavcache = lavcache
      )
      logl <- sum(attr(fx, "logl.group"))
      if (is.na(logl)) {
        return(-1e40) # nocov
      }
      out <- kappa * logl
    }
  }

  out
}

# lavaan scores each missing-data pattern with the inverse of its observed block
# of Sigma, which it gets by downdating the full inverse once per missing
# variable. That costs O(p^2) per missing variable and dominates when most items
# are missing by design (planned missingness), so single-level fits without
# covariates or weights score each pattern from the Cholesky factor of its
# observed block instead. The two give the same log-likelihood.
fiml_fast_path <- function(lavmodel, lavsamplestats, lavdata) {
  isTRUE(lavsamplestats@missing.flag) &&
    lavdata@nlevels == 1L &&
    !isTRUE(lavmodel@conditional.x) &&
    all(lengths(lavsamplestats@x.idx) == 0L) &&
    all(vapply(lavdata@weights, is.null, logical(1))) &&
    !has_random_slopes(lavmodel)
}

# Only the first group's Sigma is checked upstream, so a failed factorisation
# in another group returns the optimiser's sentinel.
fiml_loglik_samp <- function(lavsamplestats, lavimplied) {
  log_2pi <- log(2 * pi)
  tryCatch(
    {
      out <- 0
      for (g in seq_along(lavsamplestats@missing)) {
        mu <- as.numeric(lavimplied$mean[[g]])
        Sigma <- lavimplied$cov[[g]]
        for (pat in lavsamplestats@missing[[g]]) {
          o <- pat$var.idx
          L <- chol(Sigma[o, o, drop = FALSE])
          z <- backsolve(L, pat$MY - mu[o], transpose = TRUE)
          dist <- sum(z^2)
          if (pat$freq > 1L) {
            dist <- dist + sum(chol2inv(L) * pat$SY)
          }
          out <- out -
            0.5 *
              pat$freq *
              (length(z) * log_2pi + 2 * sum(log(diag(L))) + dist)
        }
      }
      out
    },
    error = function(e) -1e40
  )
}

# The FIML gradient has the same bottleneck in lav_model_omega(), whose loop
# over patterns forms Sigma_oo^-1 (W - Sigma_oo) Sigma_oo^-1 with two matrix
# products under try(). The same matrix is a a' + Sigma_oo^-1 S Sigma_oo^-1 -
# Sigma_oo^-1 with a = Sigma_oo^-1 (ybar - mu), and S is zero for a pattern of
# one case. The rest follows lav_model_grad() for single-level ML: its own
# LISREL derivative of the moments and its own map to the free parameters, so
# the result is lavaan's gradient for any model it would treat this way.
fiml_grad_fast_path <- function(lavmodel, lavsamplestats, lavdata) {
  lavmodel@estimator == "ML" &&
    fiml_fast_path(lavmodel, lavsamplestats, lavdata) &&
    lavmodel@representation == "LISREL" &&
    isTRUE(lavmodel@meanstructure) &&
    !isTRUE(lavmodel@composites) &&
    !isTRUE(lavmodel@correlation) &&
    !isTRUE(lavmodel@group.w.free)
}

# lavaan's Omega and Omega_mu (lav_model_omega(), FIML branch), one list
# element per group
fiml_omega_samp <- function(lavsamplestats, lavimplied) {
  n_total <- lavsamplestats@ntotal
  lapply(seq_along(lavsamplestats@missing), function(g) {
    mu <- as.numeric(lavimplied$mean[[g]])
    Sigma <- lavimplied$cov[[g]]
    p <- length(mu)
    omega <- matrix(0, p, p)
    omega_mu <- matrix(0, p, 1L)
    for (pat in lavsamplestats@missing[[g]]) {
      o <- which(pat$var.idx)
      S_inv <- chol2inv(chol(Sigma[o, o, drop = FALSE]))
      a <- S_inv %*% (pat$MY - mu[o])
      omega_o <- tcrossprod(a) - S_inv
      if (pat$freq > 1L) {
        omega_o <- omega_o + S_inv %*% pat$SY %*% S_inv
      }
      w <- pat$freq / n_total
      omega[o, o] <- omega[o, o] + w * omega_o
      omega_mu[o, 1L] <- omega_mu[o, 1L] + w * a
    }
    list(omega = omega, omega_mu = omega_mu)
  })
}

# Gradient of lavaan's FIML fit function w.r.t. the free parameters (unpacked
# under ceq.simple), or NULL when a pattern's block cannot be factorised, in
# which case the caller falls back to lav_model_grad()
fiml_grad_samp <- function(lavmodel_x, lavsamplestats) {
  om <- tryCatch(
    fiml_omega_samp(lavsamplestats, lavaan::lav_model_implied(lavmodel_x)),
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

inlav_model_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavcache
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)

  # Without a mean structure, the gradient of the marginalised log-likelihood
  # is lavaan's ML gradient at rescaled sample statistics (see
  # marginalised_samplestats()), so it costs no more than the profiled one.
  if (lavmodel@estimator == "ML" && marginalised_means_active(lavmodel)) {
    lavsamplestats <- marginalised_samplestats(lavsamplestats)
  }

  # Gradient of fit function F_ML (not loglik yet). lavaan weights each group by
  # its share of the sample only where the fit function averages over the
  # groups. Under FIML and PML, and in two-level models, it already sums them
  # (the rule in lav_model_estimate()).
  group_weight <- !(isTRUE(lavsamplestats@missing.flag) ||
    lavmodel@estimator == "PML" ||
    lavdata@nlevels > 1L)
  grad_F <- NULL
  if (fiml_grad_fast_path(lavmodel, lavsamplestats, lavdata)) {
    grad_F <- fiml_grad_samp(lavmodel_x, lavsamplestats)
  }
  if (is.null(grad_F)) {
    grad_F <- lavaan___lav_model_grad(
      lavmodel = lavmodel_x,
      lavsamplestats = lavsamplestats,
      lavdata = lavdata,
      lavcache = lavcache,
      group_weight = group_weight
    )
  }

  # lavaan takes an early return for random-slope models and builds the
  # gradient from the packed free parameters, one entry per equality group,
  # where every other model returns one entry per free partable row. The
  # chain rule downstream expects the unpacked convention.
  if (rs_grad_is_packed(length(grad_F), lavmodel)) {
    grad_F <- rs_unpack_grad(grad_F, lavmodel)
  }

  out <-
    if (lavmodel@estimator == "ML") {
      -1 * lavsamplestats@ntotal * grad_F
    } else if (lavmodel@estimator == "PML") {
      # nocov start
      no_ord <- length(lavdata@ordered)
      kappa <- 1 / sqrt(no_ord) # scaling factor for PML
      -1 * kappa * grad_F
    } else {
      0 * x
    } # nocov end

  out
}

# Without a mean structure, lavaan's log-likelihood profiles the saturated
# means at the sample means -- a frequentist device with no Bayesian
# counterpart. The coherent reading assigns the saturated means flat priors
# and integrates them out; the integral is closed form and equals the
# profiled log-likelihood plus, per group,
#   (1/2) log|Sigma| + (p/2) log(2*pi/n).
# The correction below applies this to the loglik, and
# marginalised_samplestats() to the gradient, so the whole posterior (mode,
# Hessian, marginals, samples) is built from the marginalised likelihood.
# Under fixed.x the likelihood is that of y given x, so only the outcome
# intercepts are integrated and Sigma is replaced by the conditional
# covariance, with log|Sigma_y.x| = log|Sigma| - log|Sigma_xx|. Sigma_xx is
# fixed at the sample covariance, so it does not enter the gradient.
marginalised_means_active <- function(lavmodel) {
  !isTRUE(lavmodel@meanstructure) && !isTRUE(lavmodel@conditional.x)
}

# In theta, the marginalised log-likelihood of group g is
#   -(n_g - 1)/2 [log|Sigma| + tr(Sigma^{-1} S*_g)],
# with S*_g = n_g S_g / (n_g - 1). Up to a constant, this is the Wishart
# likelihood of the scatter matrix (n_g - 1) S*_g with n_g - 1 degrees of
# freedom, and lavaan's ML log-likelihood of n_g - 1 observations with
# covariance S*_g. So lavaan's ML gradient at these statistics is the gradient
# of the marginalised log-likelihood, with no Delta matrix to build. Under
# fixed.x the x block of S*_g differs from the fixed Sigma_xx, but only by a
# constant in theta. The inverse and log-determinant of S*_g follow from those
# of S_g in closed form. Under sampling weights lavaan's n_g is the sum of the
# normalised weights, and the marginalised likelihood is unbounded in |Sigma|
# when it is 1 or less.
marginalised_samplestats <- function(lavsamplestats) {
  for (g in seq_len(lavsamplestats@ngroups)) {
    n_g <- lavsamplestats@nobs[[g]]
    if (n_g <= 1) {
      cli_abort(c(
        "Marginalising the saturated means needs a sample size above 1 in
         every group.",
        "i" = "Group {g} has sample size {signif(n_g, 3)}, the sum of its
               normalised sampling weights.",
        "i" = "Use {.code sampling.weights.normalization = \"group\"}, the
               default, or fit with {.code meanstructure = TRUE}."
      ))
    }
    k <- n_g / (n_g - 1)
    lavsamplestats@cov[[g]] <- k * lavsamplestats@cov[[g]]
    lavsamplestats@icov[[g]] <- lavsamplestats@icov[[g]] / k
    lavsamplestats@cov.log.det[[g]] <- lavsamplestats@cov.log.det[[g]] +
      nrow(lavsamplestats@cov[[g]]) * log(k)
    lavsamplestats@nobs[[g]] <- n_g - 1L
  }
  lavsamplestats@ntotal <- sum(unlist(lavsamplestats@nobs))
  lavsamplestats
}

marginalised_means_loglik_corr <- function(lavimplied, lavsamplestats) {
  corr <- 0
  for (g in seq_len(lavsamplestats@ngroups)) {
    Sigma_g <- lavimplied$cov[[g]]
    n_g <- lavsamplestats@nobs[[g]]
    corr <- corr +
      0.5 * as.numeric(determinant(Sigma_g, logarithm = TRUE)$modulus) +
      0.5 * ncol(Sigma_g) * log(2 * pi / n_g)
    x_idx <- lavsamplestats@x.idx[[g]]
    if (length(x_idx) > 0L) {
      Sigma_xx <- Sigma_g[x_idx, x_idx, drop = FALSE]
      corr <- corr -
        0.5 * as.numeric(determinant(Sigma_xx, logarithm = TRUE)$modulus) -
        0.5 * length(x_idx) * log(2 * pi / n_g)
    }
  }
  corr
}

# One draw of the saturated means minus the sample means, from the posterior
# N(0, Sigma / n). Under fixed.x the covariate means are not parameters, so
# they stay at zero and the outcome means draw from Sigma_y.x / n. NULL when
# Sigma is not positive definite.
draw_marginalised_mean_shift <- function(Sigma, x_idx, n) {
  shift <- numeric(ncol(Sigma))
  y_idx <- setdiff(seq_len(ncol(Sigma)), x_idx)
  S <- Sigma[y_idx, y_idx, drop = FALSE]
  if (length(x_idx) > 0L) {
    S <- S -
      Sigma[y_idx, x_idx, drop = FALSE] %*%
        solve(
          Sigma[x_idx, x_idx, drop = FALSE],
          Sigma[x_idx, y_idx, drop = FALSE]
        )
  }
  ch <- tryCatch(chol(S), error = function(e) NULL)
  if (is.null(ch)) {
    return(NULL) # nocov
  }
  shift[y_idx] <- as.numeric(crossprod(ch, stats::rnorm(length(y_idx)))) /
    sqrt(n)
  shift
}

# Under fixed.x the evidence should be that of y given x, so the joint kernel
# must lose the covariate log-density. A two-level lavaan loglik subtracts its
# own saturated term (YLp loglik.x), which omits covariates that vary at both
# levels. This returns loglik.x less the frozen covariate log-density of every
# cluster, the amount to add to lavaan's loglik to make it conditional. It is
# theta-free, so it moves the marginal likelihood and DIC but not the
# posterior. Any x (lavaan-side, unpacked) gives the same value.
twolevel_fixedx_loglik_adj <- function(lavmodel, x, lavdata, lavsamplestats) {
  if (!isTRUE(lavmodel@fixed.x) || lavdata@nlevels < 2L) {
    return(0)
  }
  if (length(lavsamplestats@x.idx[[1L]]) == 0L) {
    return(0)
  }
  Lp <- lavdata@Lp[[1L]]
  ylp <- lavsamplestats@YLp[[1L]][[2L]]
  info <- loo_fixedx_info_loco(
    list(lavdata = lavdata, lavsamplestats = lavsamplestats),
    list(Lp = Lp),
    loo_implied_moments(
      lavaan::lav_model_set_parameters(lavmodel, x),
      two_level = TRUE
    )
  )
  X <- lavdata@X[[1L]]
  cl <- Lp$cluster.idx[[2L]]
  c_sum <- sum(vapply(
    seq_len(Lp$nclusters[[2L]]),
    function(j) {
      loo_fixedx_const_raw(
        X[cl == j, , drop = FALSE],
        ylp$Y2[j, , drop = FALSE],
        info
      )
    },
    numeric(1)
  ))
  loglik_x <- if (is.null(ylp$loglik.x)) 0 else ylp$loglik.x
  loglik_x - c_sum
}
