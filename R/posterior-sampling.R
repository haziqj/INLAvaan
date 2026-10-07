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
# Core sampling function: draw from the Gaussian-copula posterior.
# Returns a list with:
#   $theta_samp  - nsamp x m matrix in the internal (theta) parameterisation
#   $x_samp      - nsamp x p matrix in the lavaan (x) parameterisation
# ---------------------------------------------------------------------------
sample_params <- function(
  theta_star,
  Sigma_theta,
  method,
  approx_data,
  pt,
  lavmodel,
  nsamp = 1000,
  R_star = NULL # NORTA-adjusted correlation matrix for SN copula
) {
  R <- cov2cor(Sigma_theta)
  R_use <- if (method == "skewnorm" && !is.null(R_star)) R_star else R
  Lt <- chol(R_use)
  z_raw <- matrix(rnorm(nsamp * ncol(R)), nrow = nsamp)
  z <- z_raw %*% Lt
  u <- apply(z, 2, pnorm)
  if (!is.matrix(u)) {
    u <- matrix(u, nrow = 1)
  } # nocov – nsamp == 1 edge case

  # Use marginals to get theta
  if (method == "sampling") {
    D <- diag(sqrt(diag(Sigma_theta)))
    theta <- t(D %*% qnorm(t(u)) + theta_star)
  } else {
    if (method == "skewnorm") {
      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          xi <- approx_data[j, "xi"]
          omega <- approx_data[j, "omega"]
          alpha <- approx_data[j, "alpha"]
          qsnorm_fast(u[, j], xi = xi, omega = omega, alpha = alpha)
        })
      )
    } else if (method == "asymgaus") {
      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          tt <- theta_star[j] +
            seq(-4, 4, length = 100) * sqrt(Sigma_theta[j, j])
          yy <- marg_lp(
            tt,
            j = j,
            theta_star = theta_star,
            Sigma_theta = Sigma_theta,
            sigma_asym = approx_data
          )
          yy <- yy - max(yy) # stabilise
          fj_lp <- stats::splinefun(tt, yy)
          fj <- function(par) exp(fj_lp(par)) # unnormalised
          dt <- diff(tt)
          ft <- fj(tt)
          fmid <- (head(ft, -1) + tail(ft, -1)) / 2
          ymid <- (head(tt, -1) + tail(tt, -1)) / 2
          C <- sum(fmid * dt)
          ft <- ft / C
          Ft <- c(0, cumsum(fmid * dt))
          Ft <- Ft / tail(Ft, 1)
          qfj <- splinefun(Ft, tt, method = "monoH.FC")
          qfj(u[, j])
        })
      )
    } else if (method == "marggaus") {
      theta <- do.call(
        "cbind",
        lapply(seq_len(ncol(u)), function(j) {
          mu_j <- theta_star[j]
          sd_j <- sqrt(Sigma_theta[j, j])
          qnorm(u[, j], mean = mu_j, sd = sd_j)
        })
      )
    }
  }

  # theta stays packed (one column per free parameter), and pars_to_x() takes it
  # unpacked to every free row.
  theta_full <- theta
  if (lavmodel@ceq.simple.only) {
    K <- lavmodel@ceq.simple.K
    theta_full <- t(apply(theta, 1, function(pars) as.numeric(K %*% pars)))
  }

  x <- t(apply(theta_full, 1, pars_to_x, pt = pt))

  list(theta_samp = theta, x_samp = x)
}

# 1. Get correlation matrix R = cov2cor(Sigma_theta)
# 2. Sample z ~ N(0, R)
# 3. Compute u = Phi^{-1}(z)
# 4. For each j, compute theta_j = F^{-1}_j(u_j) where F_j is the marginal posterior CDF for parameter j
# 5. Get x = pars_to_x(theta)
# 6. Compute implied covariance matrix Sigma(x)
# 7. Sample Srep ~ Wishart(n - 1, Sigma(x))
# 7. Repeat 1-6 and get samples of Sigma, and Srep
#
# Define F(S, Sigma) = log |Sigma| + trace(S Sigma^{-1}) - log |S| - p
# and the test statistic is T(S, Sigma) = (n - 1) / 2 * F(S, Sigma)
# Then ppp = P(T(Srep, Sigma) >= T(S, Sigma))

get_ppp <- function(
  x_samp,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavpartable = NULL,
  h1 = NULL,
  cli_env = NULL
) {
  n_blocks <- lavmodel@nblocks
  n_levels <- lavdata@nlevels
  has_missing <- isTRUE(lavsamplestats@missing.flag)
  # lavaan's saturated (h1) covariance blocks. They are the observed
  # covariance that the discrepancy must use whenever the plain sample
  # statistics are not: under FIML the sample covariance is not the
  # saturated estimate, and the two-level cluster statistics (YLp) are
  # computed without regard to missing cells.
  h1_cov <- h1$implied$cov

  # Pre-compute per-block observed variable names (needed for alignment)
  ov_names_block <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    g <- ceiling(b / n_levels)
    ov_all <- lavdata@ov.names[[g]]
    if (!is.null(lavpartable)) {
      ov_names_block[[b]] <- unique(
        lavpartable$lhs[
          lavpartable$block == b &
            lavpartable$op == "~~" &
            lavpartable$lhs == lavpartable$rhs &
            lavpartable$lhs %in% ov_all
        ]
      )
    }
  }

  # Pre-compute per-block observed stats (S, n, logdet_S) — invariant across
  # samples, so we avoid redundant extraction and determinant calls in the loop.
  block_obs <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    g <- ceiling(b / n_levels)
    l <- (b - 1) %% n_levels + 1

    if (n_levels > 1) {
      # nocov start
      n <- if (l == 1) {
        lavdata@nobs[[g]]
      } else {
        lavdata@Lp[[g]]$nclusters[[l]]
      }
      if (!is.null(h1_cov) && length(h1_cov) >= b) {
        # Saturated within- and between-level covariances, named by the
        # variables of that level
        S <- h1_cov[[b]]
        ov_l <- lavdata@ov.names.l[[g]][[l]]
        if (nrow(S) == length(ov_l)) {
          rownames(S) <- colnames(S) <- ov_l
        }
      } else {
        cluster_stats <- lavsamplestats@YLp[[g]][[2]]
        S <- if (l == 1) cluster_stats$Sigma.W else cluster_stats$Sigma.B
        rownames(S) <- colnames(S) <- lavdata@ov.names[[g]]
        keep <- rowSums(S != 0) > 0
        S <- S[keep, keep, drop = FALSE]
      }
      # nocov end
    } else {
      n <- lavsamplestats@nobs[[g]]
      # The observed covariance: the EM (saturated) estimate under FIML,
      # the plain sample covariance otherwise. Both have divisor n, and
      # the replicates below are Wishart(n - 1, Sigma) / (n - 1) objects,
      # so rescale to the unbiased (n - 1) form to match
      S <- if (has_missing && !is.null(lavsamplestats@missing.h1[[g]]$sigma)) {
        lavsamplestats@missing.h1[[g]]$sigma
      } else {
        lavsamplestats@cov[[g]]
      }
      S <- S * n / (n - 1)
    }

    logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
    block_obs[[b]] <- list(
      S = S,
      n = n,
      logdet_S = logdet_S,
      # Positions of the fixed covariates (single-level only)
      x_idx = if (n_levels == 1) lavsamplestats@x.idx[[g]] else integer(0L)
    )
  }

  # Composite indicator (co)variances fixed at the sample covariance change
  # with the data, so they must change with each replicate too (see
  # ppp_composite_draw()).
  t_fixed <- composite_fixed_t(lavmodel, lavpartable, lavdata)

  res <- vector("numeric", length = nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env) # nocov
    }

    xx <- x_samp[i, ]
    lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
    lavimplied <- lavaan::lav_model_implied(lavmodel_x)

    if (!is.null(t_fixed)) {
      res[i] <- ppp_composite_draw(xx, lavimplied, lavmodel, t_fixed, block_obs)
      next
    }

    Tobs <- 0
    Trep <- 0

    for (b in seq_len(n_blocks)) {
      S <- block_obs[[b]]$S
      n <- block_obs[[b]]$n
      logdet_S <- block_obs[[b]]$logdet_S

      Sigma <- implied_joint_moments(lavimplied, b, block_obs[[b]]$x_idx)$cov

      # Align dimensions for multilevel models
      if (n_levels > 1 && length(ov_names_block[[b]]) == nrow(Sigma)) {
        # nocov start
        rownames(Sigma) <- colnames(Sigma) <- ov_names_block[[b]]
        shared <- intersect(rownames(S), rownames(Sigma))
        if (length(shared) > 0) {
          S <- S[shared, shared, drop = FALSE]
          Sigma <- Sigma[shared, shared, drop = FALSE]
          logdet_S <- as.numeric(determinant(S, logarithm = TRUE)$modulus)
        }
        # nocov end
      }

      if (!is_bad_cov(Sigma)) {
        p <- nrow(Sigma)

        # Cholesky of Sigma — shared for logdet, inverse, and both discrepancies
        L <- chol(Sigma)
        logdet_Sigma <- 2 * sum(log(diag(L)))
        Sigma_inv <- chol2inv(L)

        # Wishart draw for replicated data
        W <- matrix(stats::rWishart(1, df = n - 1, Sigma = Sigma)[,, 1], p, p)
        Srep <- W / (n - 1)
        logdet_Srep <- as.numeric(determinant(Srep, logarithm = TRUE)$modulus)

        # F(S, Sigma) = log|Sigma| + tr(Sigma^{-1} S) - log|S| - p
        Tobs <- Tobs + logdet_Sigma + sum(Sigma_inv * S) - logdet_S - p
        Trep <- Trep + logdet_Sigma + sum(Sigma_inv * Srep) - logdet_Srep - p

        # Fixed covariates are conditioned on, not modelled, so the discrepancy
        # is that of the outcomes given the covariates: F less its covariate
        # block. The replicates vary that block too, and without this they
        # would carry extra misfit and push the PPP up.
        x_idx <- block_obs[[b]]$x_idx
        if (length(x_idx) > 0L) {
          Sigma_xx <- Sigma[x_idx, x_idx, drop = FALSE]
          Tobs <- Tobs - ml_discrepancy(S[x_idx, x_idx, drop = FALSE], Sigma_xx)
          Trep <- Trep -
            ml_discrepancy(Srep[x_idx, x_idx, drop = FALSE], Sigma_xx)
        }
      }
    }

    res[i] <- as.numeric(Trep >= Tobs)
  }

  # A composite draw whose replicate moments are not a covariance is NA
  mean(res, na.rm = TRUE)
}

# One posterior draw of the PPP for a fit with composites. lavaan fixes the
# (co)variances of the composite indicators at the sample covariance of the data
# it is given, so the observed data have no misfit in that block. A replicate is
# therefore scored against the moments implied with its own sample covariance
# there (divisor n, as lavaan uses), which is what lavaan would fix had it been
# given the replicate. Scored against the observed values instead, every
# replicate would carry a misfit in that block that the observed data cannot
# have, and the PPP would drift towards 1. Composites are single-level, so the
# blocks are the groups. Returns 1 when Trep >= Tobs, 0 otherwise, and NA when
# the replicate moments are not a covariance matrix.
ppp_composite_draw <- function(xx, lavimplied, lavmodel, t_fixed, block_obs) {
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
  # lavaan re-derives the composite variances from the replaced block
  implied_rep <- lavaan::lav_model_implied(
    lavaan::lav_model_set_parameters(lavmodel_rep, xx)
  )

  Trep <- 0
  for (b in seq_len(n_blocks)) {
    if (is.null(Srep[[b]])) {
      next
    }
    x_idx <- block_obs[[b]]$x_idx
    Sigma_rep <- implied_joint_moments(implied_rep, b, x_idx)$cov
    if (is_bad_cov(Sigma_rep)) {
      return(NA_real_) # nocov
    }
    Trep <- Trep + ppp_discrepancy(Srep[[b]], Sigma_rep, x_idx)
  }
  as.numeric(Trep >= Tobs)
}

# F(S, Sigma) of the outcomes given the fixed covariates at x_idx, which is F
# less its covariate block, as in get_ppp().
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

# Variables at both levels whose between-level variance is under a quarter of
# the noise in a cluster mean (within variance over the mean cluster size). The
# two-level PPP takes the replicate between covariance as Wishart around the
# implied one, which leaves that noise out. For such a variable the reference
# is far too tight, and the PPP falls to 0 for a model that fits.
ppp_weak_between_vars <- function(lavdata, h1) {
  h1_cov <- h1$implied$cov
  if (lavdata@nlevels < 2L || is.null(h1_cov)) {
    return(character(0L))
  }
  weak <- character(0L)
  for (g in seq_len(lavdata@ngroups)) {
    nm <- lavdata@ov.names.l[[g]]
    both <- intersect(nm[[1L]], nm[[2L]])
    Lp <- lavdata@Lp[[g]]
    nbar <- Lp$nclusters[[1L]] / Lp$nclusters[[2L]]
    var_w <- diag(h1_cov[[2L * g - 1L]])[match(both, nm[[1L]])]
    var_b <- diag(h1_cov[[2L * g]])[match(both, nm[[2L]])]
    weak <- c(weak, both[var_b < 0.25 * var_w / nbar])
  }
  unique(weak)
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
    out$sn_params <- c(
      xi = NA_real_,
      omega = NA_real_,
      alpha = NA_real_,
      logC = NA_real_,
      k = NA_real_,
      rmse = NA_real_,
      nmad = NA_real_,
      gamma1 = NA_real_
    )
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
  qq <- qsnorm_fast(
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
    sn_params = c(
      xi = xi,
      omega = omega,
      alpha = alpha,
      logC = NA_real_,
      k = NA_real_,
      rmse = NA_real_,
      nmad = NA_real_,
      gamma1 = NA_real_
    )
  )
}

get_dic <- function(
  x_samp,
  theta_star,
  pt,
  lavmodel,
  loglik,
  cli_env = NULL
) {
  if (lavmodel@ceq.simple.only) {
    # nocov start
    xhat <- pars_to_x(as.numeric(lavmodel@ceq.simple.K %*% theta_star), pt)
  } else {
    # nocov end
    xhat <- pars_to_x(theta_star, pt)
  }

  Dhat <- -2 * loglik(xhat)
  Dbar_samp <- sapply(seq_len(nrow(x_samp)), function(i) {
    if (!is.null(cli_env)) {
      cli_progress_update(.envir = cli_env)
    }
    xx <- x_samp[i, ]
    -2 * loglik(xx)
  })
  Dbar <- mean(Dbar_samp[Dbar_samp < 1e40])
  pD <- Dbar - Dhat

  list(dic = Dbar + pD, Dbar = Dbar, Dhat = Dhat, pD = pD)
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
