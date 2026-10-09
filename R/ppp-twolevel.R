# Posterior predictive p-value (PPP) of a two-level fit, following blavaan's
# pp_twolevel(). Each posterior draw generates one replicate data set from the
# implied two-level moments, with the observed cluster design and the fixed
# covariates of each level at their observed values. The discrepancy is the
# likelihood-ratio statistic against the saturated two-level model,
#
#   T = -2 (loglik(theta; y) - loglik_sat(y)),
#
# with the saturated model refitted to each replicate by EM, and
# PPP = Pr(T(y_rep) > T(y)). Scoring the replicate with its own saturated fit
# is what calibrates the PPP: lavaan's saturated between-level estimate is
# noisier than a Wishart draw around the implied between covariance, so a
# replicate drawn that way rejects a correct model.

# EM settings for the saturated fit of each replicate, as in pp_twolevel(),
# and for the one fit of the observed data. The replicate fits stop at their
# iteration cap on purpose, so their warnings are muffled.
ppp2l_em <- list(
  tol = 1e-3,
  max_iter = 50L,
  acceleration = "squarem",
  quiet = TRUE
)
ppp2l_em_obs <- list(
  tol = 1e-4,
  max_iter = 5000L,
  acceleration = "none",
  quiet = FALSE
)

# A variable held fixed at both levels is outside what the generator can
# condition on, as in blavaan. Returns its names (empty when none).
ppp2l_fixed_both <- function(lavdata) {
  out <- character(0)
  for (g in seq_len(lavdata@ngroups)) {
    lp <- lavdata@Lp[[g]]
    both <- intersect(lp$ov.x.idx[[1L]], lp$ov.x.idx[[2L]])
    out <- c(out, lavdata@ov.names[[g]][both])
  }
  unique(out)
}

# Rows of a Gaussian with mean `mu` and covariance `S` for the variables `r`,
# given the values `x` of the variables `f`. Rows with a missing covariate
# condition on the covariates they have.
ppp2l_conditional_draw <- function(n, mu, S, r, f, x) {
  noise <- function(k, V) {
    matrix(stats::rnorm(k * length(r)), ncol = length(r)) %*%
      chol((V + t(V)) / 2)
  }
  if (length(f) == 0L) {
    m <- matrix(mu[r], n, length(r), byrow = TRUE)
    return(m + noise(n, S[r, r, drop = FALSE]))
  }
  out <- matrix(NA_real_, n, length(r))
  obs <- !is.na(x)
  key <- apply(obs, 1L, function(z) paste(which(z), collapse = ","))
  for (k in unique(key)) {
    rows <- which(key == k)
    fo <- f[obs[rows[1L], ]]
    m <- matrix(mu[r], length(rows), length(r), byrow = TRUE)
    V <- S[r, r, drop = FALSE]
    if (length(fo) > 0L) {
      B <- S[r, fo, drop = FALSE] %*% solve(S[fo, fo, drop = FALSE])
      xo <- x[rows, obs[rows[1L], ], drop = FALSE]
      m <- m + sweep(xo, 2L, mu[fo]) %*% t(B)
      V <- V - B %*% S[fo, r, drop = FALSE]
    }
    out[rows, ] <- m + noise(length(rows), V)
  }
  out
}

# One replicate data matrix per group from the implied two-level moments.
# lavaan's Lp slots give the columns of each level (ov.idx) and of its fixed
# covariates (ov.x.idx), in the order of the implied moments.
ppp2l_draw <- function(lavdata, lavimplied) {
  lapply(seq_len(lavdata@ngroups), function(g) {
    lp <- lavdata@Lp[[g]]
    X <- lavdata@X[[g]]
    cl <- lp$cluster.idx[[2L]]
    J <- lp$nclusters[[2L]]
    out <- matrix(0, nrow(X), ncol(X), dimnames = dimnames(X))
    for (l in 1:2) {
      cols <- lp$ov.idx[[l]]
      xcols <- lp$ov.x.idx[[l]]
      if (is.null(xcols)) {
        xcols <- integer(0)
      }
      b <- 2L * g - 2L + l
      mu <- as.numeric(lavimplied$mean[[b]])
      S <- lavimplied$cov[[b]]
      f <- match(xcols, cols)
      r <- setdiff(seq_along(cols), f)
      rows <- if (l == 1L) seq_len(nrow(X)) else match(seq_len(J), cl)
      x <- X[rows, xcols, drop = FALSE]
      draw <- matrix(0, length(rows), length(cols))
      draw[, r] <- ppp2l_conditional_draw(length(rows), mu, S, r, f, x)
      draw[, f] <- x
      if (l == 1L) {
        out[, cols] <- draw
      } else {
        out[, cols] <- out[, cols, drop = FALSE] + draw[cl, , drop = FALSE]
      }
    }
    out[is.na(X)] <- NA
    out
  })
}

# lavaan's cluster sample statistics of complete data. The function is looked
# up at call time rather than bound in .onLoad(): a branch INLAvaan never takes
# (conditional_x = TRUE) calls MASS, so a binding in the namespace would make
# R CMD check ask for MASS as a dependency.
ppp2l_cluster_stats <- function(X, lp) {
  f <- utils::getFromNamespace("lav_samp_cl_patterns", "lavaan")
  f(y = X, lp = lp, conditional_x = FALSE)
}

# Model and saturated log-likelihoods of one group's data, complete or not
ppp2l_loglik <- function(X, g, lavdata, lavimplied, missing, em = NULL) {
  saturated <- !is.null(em)
  run_em <- if (isTRUE(em$quiet)) muffle_em_warnings else identity
  lp <- lavdata@Lp[[g]]
  bw <- 2L * g - 1L
  bb <- 2L * g
  if (missing) {
    y2 <- rowsum.default(X, group = lp$cluster.idx[[2L]], reorder = FALSE) /
      lp$cluster.size[[2L]]
    fit <- lavaan___lav_mvn_cl_mi_loglik_samp_2l(
      y1 = X,
      y2 = y2,
      lp = lp,
      mp = lavdata@Mp[[g]],
      mu_w = lavimplied$mean[[bw]],
      sigma_w = lavimplied$cov[[bw]],
      mu_b = lavimplied$mean[[bb]],
      sigma_b = lavimplied$cov[[bb]],
      loglik_x = 0,
      log2pi = TRUE,
      minus_two = FALSE
    )
    sat <- if (saturated) {
      run_em(
        lavaan___lav_mvn_cl_mi_em_sat(
          y1 = X,
          y2 = y2,
          lp = lp,
          mp = lavdata@Mp[[g]],
          loglik_x = 0,
          tol = em$tol,
          max_iter = em$max_iter,
          min_variance = 1e-05,
          acceleration = em$acceleration
        )$logl
      )
    }
  } else {
    ylp <- ppp2l_cluster_stats(X, lp)
    fit <- lavaan___lav_mvn_cl_loglik_samp_2l(
      ylp = ylp,
      lp = lp,
      mu_w = lavimplied$mean[[bw]],
      sigma_w = lavimplied$cov[[bw]],
      mu_b = lavimplied$mean[[bb]],
      sigma_b = lavimplied$cov[[bb]],
      log2pi = TRUE,
      minus_two = FALSE
    )
    sat <- if (saturated) {
      run_em(
        lavaan___lav_mvn_cl_em_sat(
          ylp = ylp,
          lp = lp,
          tol = em$tol,
          max_iter = em$max_iter,
          min_variance = 1e-05,
          acceleration = em$acceleration
        )$logl
      )
    }
  }
  c(fit = fit, sat = if (saturated) sat else NA_real_)
}

muffle_em_warnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    invokeRestart("muffleWarning")
  })
}

# The PPP of a two-level fit over the posterior draws `x_samp`
get_ppp_twolevel <- function(
  x_samp,
  lavmodel,
  lavsamplestats,
  lavdata,
  cli_env = NULL
) {
  missing <- isTRUE(lavsamplestats@missing.flag)
  groups <- seq_len(lavdata@ngroups)
  # The saturated log-likelihood of the observed data, once
  lavimplied0 <- lavaan::lav_model_implied(lavmodel)
  sat_obs <- sum(vapply(
    groups,
    function(g) {
      ppp2l_loglik(
        lavdata@X[[g]],
        g,
        lavdata,
        lavimplied0,
        missing,
        ppp2l_em_obs
      )[["sat"]]
    },
    numeric(1)
  ))
  hit <- rep(NA, nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env) # nocov
    }
    hit[i] <- tryCatch(
      {
        lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x_samp[i, ])
        lavimplied <- lavaan::lav_model_implied(lavmodel_x)
        reps <- ppp2l_draw(lavdata, lavimplied)
        fit_obs <- fit_rep <- sat_rep <- 0
        for (g in groups) {
          fit_obs <- fit_obs +
            ppp2l_loglik(lavdata@X[[g]], g, lavdata, lavimplied, missing)[[
              "fit"
            ]]
          ll <- ppp2l_loglik(
            reps[[g]],
            g,
            lavdata,
            lavimplied,
            missing,
            ppp2l_em
          )
          fit_rep <- fit_rep + ll[["fit"]]
          sat_rep <- sat_rep + ll[["sat"]]
        }
        -2 * (fit_rep - sat_rep) > -2 * (fit_obs - sat_obs)
      },
      error = function(e) NA
    )
  }
  n_bad <- sum(is.na(hit))
  if (n_bad == length(hit)) {
    cli_warn("No posterior draw gave a replicate, so the PPP is missing.")
    return(NA_real_)
  }
  if (n_bad > 0L) {
    cli_warn(
      "{n_bad} of {length(hit)} posterior draws gave no replicate and are
       left out of the PPP."
    )
  }
  mean(hit, na.rm = TRUE)
}
