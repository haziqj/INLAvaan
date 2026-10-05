get_SEM_param_matrix <- function(x, mat, lavmodel) {
  nG <- lavmodel@ngroups
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)

  GLIST <- Map(
    function(mat, dn) {
      rownames(mat) <- dn[[1]]
      colnames(mat) <- dn[[2]]
      mat
    },
    lavmodel_x@GLIST,
    lavmodel_x@dimNames
  )

  uniq_names <- unique(names(GLIST))
  k <- length(uniq_names)
  out <- vector("list", nG)
  for (g in seq_len(nG)) {
    idx <- ((g - 1) * k + 1):(g * k)
    out[[g]] <- GLIST[idx]
    names(out[[g]]) <- uniq_names
  }

  if (mat == "all" | mat == "GLIST") {
    return(out)
  } else {
    return(lapply(out, function(glist) glist[[mat]]))
  }
}

# For factor scores, there is the plugin marginal_method and sampling
# marginal_method.
#
# For plugin marginal_method, eta | y ~ N(mu(theta, y), V(theta)), where
# mu(theta,y) = E(eta | y,theta) = Phi Lambda Sigma^{-1} y
# V(theta) = Phi - Phi Lambda' Sigma^{-1} Lambda Phi
# Phi = (I - B)^{-1} Psi (I - B')^{-1}'
#
# For sampling marginal_method just sample from the above distribution.

# Indices of the latent variables that lavaan adds for observed covariates and
# observed endogenous variables in block g, with their observed columns.
dummy_lv_idx <- function(lavmodel, g) {
  list(
    lv = c(lavmodel@ov.y.dummy.lv.idx[[g]], lavmodel@ov.x.dummy.lv.idx[[g]]),
    ov = c(lavmodel@ov.y.dummy.ov.idx[[g]], lavmodel@ov.x.dummy.ov.idx[[g]])
  )
}

# Latent intercepts as a full vector. Without a mean structure, the dummy latent
# variables get the intercepts that reproduce the sample means of their observed
# variables and the others get zero, as in lavaan.
eta_intercepts <- function(alpha, glist, front, dummy, ybar) {
  alpha_vec <- rep_len(as.numeric(alpha), ncol(front))
  if (is.null(glist$alpha) && length(dummy$lv) > 0L) {
    alpha_vec[dummy$lv] <- solve(
      front[dummy$ov, dummy$lv, drop = FALSE],
      ybar[dummy$ov]
    )
  }
  alpha_vec
}

# A root L, with L L' = S, of a positive semi-definite covariance matrix S, used
# to draw from N(0, S). It is the lower Cholesky factor whenever chol()
# succeeds. A zero variance, such as a residual variance fixed to zero or a
# latent variable pinned down by its indicators, makes S singular, and then L
# comes from the eigendecomposition with negligible or negative eigenvalues set
# to zero.
psd_root <- function(S, tol = sqrt(.Machine$double.eps)) {
  L <- tryCatch(t(chol(S)), error = function(e) NULL)
  if (!is.null(L)) {
    return(L)
  }
  e <- eigen((S + t(S)) / 2, symmetric = TRUE)
  d <- e$values
  d[d < tol * max(d, 0)] <- 0
  sweep(e$vectors, 2, sqrt(d), "*")
}

# Draw each row of eta | y from N(mu_eta, V_eta). A dummy latent variable is its
# observed variable, so it has zero conditional variance. Only the other columns
# are drawn, and the dummy columns take the data values, as in lavaan.
draw_eta <- function(mu_eta, V_eta, y, dummy) {
  keep <- setdiff(seq_len(ncol(mu_eta)), dummy$lv)
  out <- mu_eta
  if (length(keep) > 0L) {
    chol_V <- psd_root(V_eta[keep, keep, drop = FALSE])
    n_obs <- nrow(mu_eta)
    nlv <- length(keep)
    Z <- matrix(rnorm(n_obs * nlv), nrow = nlv, ncol = n_obs)
    out[, keep] <- mu_eta[, keep, drop = FALSE] + t(chol_V %*% Z)
  }
  if (length(dummy$lv) > 0L) {
    out[, dummy$lv] <- y[, dummy$ov, drop = FALSE]
  }
  out
}

# Draw the residuals that ypred adds to n rows of block b, one column per
# observed variable of the block. lavaan keeps the residual variance of an
# observed outcome in Psi of its dummy latent variable, with zero Theta, so the
# indicators draw from Theta and the observed outcomes from Psi. Observed
# covariates get none.
draw_residuals <- function(n, Theta, Psi, lavmodel, b) {
  draw <- function(S) {
    k <- nrow(S)
    t(psd_root(S) %*% matrix(rnorm(n * k), nrow = k, ncol = n))
  }
  ov_y <- lavmodel@ov.y.dummy.ov.idx[[b]]
  lv_y <- lavmodel@ov.y.dummy.lv.idx[[b]]
  ov_x <- lavmodel@ov.x.dummy.ov.idx[[b]]
  ind <- setdiff(seq_len(nrow(Theta)), c(ov_y, ov_x))
  eps <- matrix(0, n, nrow(Theta))
  if (length(ind) > 0L) {
    eps[, ind] <- draw(Theta[ind, ind, drop = FALSE])
  }
  if (length(lv_y) > 0L) {
    eps[, ov_y] <- draw(Psi[lv_y, lv_y, drop = FALSE])
  }
  eps
}

# With conditional.x = TRUE, lavaan keeps the covariate effects in Gamma, so the
# latent intercepts vary by row as alpha + Gamma x. Returns the n x m matrix
# Gamma x for group g, or NULL when the model has no Gamma.
gamma_x <- function(glist, x_exo, g) {
  if (is.null(glist$gamma)) {
    return(NULL)
  }
  tcrossprod(x_exo[[g]], glist$gamma)
}

# Helper: build data matrices from newdata, reusing metadata from lavdata
build_newdata <- function(newdata, lavdata) {
  newdata <- recode_ordinal(newdata, lavdata)
  nG <- lavdata@ngroups
  grp <- lavdata@group
  has_group <- length(grp) > 0L && nzchar(grp)

  if (has_group) {
    group_labels <- lavdata@group.label
    groups_in_data <- as.character(newdata[[grp]])
    new_X <- vector("list", nG)
    new_eXo <- vector("list", nG)
    new_nobs <- vector("list", nG)
    for (g in seq_len(nG)) {
      rows_g <- which(groups_in_data == group_labels[g])
      ov_names_g <- lavdata@ov.names[[g]]
      new_X[[g]] <- as.matrix(newdata[rows_g, ov_names_g, drop = FALSE])
      x_names_g <- lavdata@ov.names.x[[g]]
      new_eXo[[g]] <- as.matrix(newdata[rows_g, x_names_g, drop = FALSE])
      new_nobs[[g]] <- length(rows_g)
    }
  } else {
    ov_names <- lavdata@ov.names[[1L]]
    new_X <- list(as.matrix(newdata[, ov_names, drop = FALSE]))
    x_names <- lavdata@ov.names.x[[1L]]
    new_eXo <- list(as.matrix(newdata[, x_names, drop = FALSE]))
    new_nobs <- list(nrow(newdata))
  }

  list(
    X = new_X,
    eXo = new_eXo,
    ngroups = nG,
    group.label = if (has_group) lavdata@group.label else character(0),
    nobs = new_nobs
  )
}

# Code ordinal columns 1, 2, ... by the categories seen when fitting, as lavaan
# does, whether newdata holds factors or the original values.
recode_ordinal <- function(newdata, lavdata) {
  ov <- lavdata@ov
  for (i in which(ov$type == "ordered" & ov$name %in% names(newdata))) {
    v <- ov$name[i]
    lev <- strsplit(ov$lnam[i], "|", fixed = TRUE)[[1]]
    value <- as.character(newdata[[v]])
    newdata[[v]] <- match(value, lev)
    unknown <- unique(value[!is.na(value) & is.na(newdata[[v]])])
    if (length(unknown) > 0L) {
      cli_warn(c(
        "{.arg newdata} has values of {.field {v}} that were not seen when
         fitting: {.val {unknown}}.",
        "i" = "They are treated as missing. The fitted categories are
               {.val {lev}}."
      ))
    }
  }
  newdata
}

# nocov start
# Solve for a covariance matrix that may be singular
psd_solve <- function(S) {
  tryCatch(solve(S), error = function(e) ginv_base(S))
}

# Moments of the latent variables by block, with the raw loadings (dummy latent
# variables loading 1 on their observed variables), for drawing eta | y.
ml_moments <- function(lavmodel_x, lavsamplestats) {
  list(
    lambda = lavaan___lav_model_lambda(
      lavmodel = lavmodel_x,
      handle_dummy_lv = FALSE
    ),
    veta = lavaan___lav_model_veta(lavmodel = lavmodel_x),
    eeta = lavaan___lav_model_eeta(
      lavmodel = lavmodel_x,
      lavsamplestats = lavsamplestats
    )
  )
}

# Draw each row of eta | y for one block from N(E(eta) + V Lambda' Sigma^-1
# (y - mu), V - V Lambda' Sigma^-1 Lambda V), with V = Var(eta).
draw_eta_given <- function(data, mu, Sigma, Lambda, eeta, veta) {
  n <- nrow(data)
  m <- ncol(veta)
  if (m == 0L) {
    return(matrix(0, n, 0L))
  }
  FSC <- veta %*% t(Lambda) %*% psd_solve(Sigma)
  mean <- t(FSC %*% (t(data) - mu) + as.numeric(eeta))
  V <- veta - FSC %*% Lambda %*% veta
  Z <- matrix(rnorm(n * m), nrow = m, ncol = n)
  mean + t(psd_root((V + t(V)) / 2) %*% Z)
}

# One exact draw, given theta, of everything random in a two-level group: the
# between-level values u_j of each cluster (the between parts of the variables
# at both levels, and the between-only variables), the missing values, and the
# latent variables at both levels. Given u_j the rows of a cluster are
# independent, so u_j is drawn first by Gaussian updates over the cluster's
# rows, and the rest follows from the conditionals within each level.
draw_ml_group <- function(y_g, Lp, lavimplied, mom, lavmodel_x, g = 1L) {
  b_w <- (g - 1L) * 2L + 1L
  b_b <- b_w + 1L
  idx1 <- Lp$ov.idx[[1]]
  idx2 <- Lp$ov.idx[[2]]
  p1 <- length(idx1)
  p2 <- length(idx2)
  cl <- Lp$cluster.idx[[2]]
  mu_w <- as.numeric(lavimplied$mean[[b_w]])
  S_w <- lavimplied$cov[[b_w]]
  mu_b <- as.numeric(lavimplied$mean[[b_b]])
  S_b <- lavimplied$cov[[b_b]]

  # A adds the between values u to the level-1 columns they belong to
  A <- matrix(0, p1, p2)
  shared <- match(idx1, idx2)
  A[cbind(which(!is.na(shared)), shared[!is.na(shared)])] <- 1
  z_pos <- which(!idx2 %in% idx1)
  y1 <- y_g[, idx1, drop = FALSE]
  key <- apply(is.na(y1), 1, function(r) paste(which(!r), collapse = ","))

  u <- matrix(0, Lp$nclusters[[2]], p2)
  for (j in seq_len(nrow(u))) {
    rows <- which(cl == j)
    m <- mu_b
    S <- S_b
    # Between-only variables are observed without error, once per cluster
    z_obs <- integer(0L)
    zj <- numeric(0L)
    if (length(z_pos) > 0L) {
      zj <- vapply(
        idx2[z_pos],
        function(k) {
          v <- y_g[rows, k]
          v <- v[!is.na(v)]
          if (length(v) > 0L) v[1L] else NA_real_
        },
        numeric(1)
      )
      z_obs <- z_pos[!is.na(zj)]
      zj <- zj[!is.na(zj)]
      if (length(z_obs) > 0L) {
        K <- S[, z_obs, drop = FALSE] %*%
          psd_solve(S[z_obs, z_obs, drop = FALSE])
        m <- m + as.numeric(K %*% (zj - m[z_obs]))
        S <- S - K %*% S[z_obs, , drop = FALSE]
      }
    }
    # Each missingness pattern contributes the mean of its rows, with the
    # within covariance divided by their number
    for (pat in unique(key[rows])) {
      if (!nzchar(pat)) {
        next
      }
      o <- as.integer(strsplit(pat, ",", fixed = TRUE)[[1]])
      rp <- rows[key[rows] == pat]
      H <- A[o, , drop = FALSE]
      R <- S_w[o, o, drop = FALSE] / length(rp)
      K <- S %*% t(H) %*% psd_solve(H %*% S %*% t(H) + R)
      ybar <- colMeans(y1[rp, o, drop = FALSE])
      m <- m + as.numeric(K %*% (ybar - mu_w[o] - H %*% m))
      S <- S - K %*% H %*% S
    }
    u[j, ] <- m + as.numeric(psd_root((S + t(S)) / 2) %*% rnorm(p2))
    u[j, z_obs] <- zj
  }

  # Within parts: observed entries are y - A u, and missing ones are drawn
  # given the observed ones in their row
  w <- y1 - u[cl, , drop = FALSE] %*% t(A)
  for (pat in unique(key)) {
    o <- if (nzchar(pat)) {
      as.integer(strsplit(pat, ",", fixed = TRUE)[[1]])
    } else {
      integer(0L)
    }
    mis <- setdiff(seq_len(p1), o)
    if (length(mis) == 0L) {
      next
    }
    rp <- which(key == pat)
    if (length(o) > 0L) {
      B <- S_w[mis, o, drop = FALSE] %*% psd_solve(S_w[o, o, drop = FALSE])
      mean <- sweep(w[rp, o, drop = FALSE], 2, mu_w[o]) %*% t(B)
      mean <- sweep(mean, 2, mu_w[mis], "+")
      V <- S_w[mis, mis, drop = FALSE] - B %*% S_w[o, mis, drop = FALSE]
    } else {
      mean <- matrix(mu_w[mis], length(rp), length(mis), byrow = TRUE)
      V <- S_w[mis, mis, drop = FALSE]
    }
    Z <- matrix(rnorm(length(rp) * length(mis)), nrow = length(mis))
    w[rp, mis] <- mean + t(psd_root((V + t(V)) / 2) %*% Z)
  }
  y_full <- y_g
  y_full[, idx1] <- w + u[cl, , drop = FALSE] %*% t(A)
  y_full[, idx2[z_pos]] <- u[cl, z_pos, drop = FALSE]
  y_full[!is.na(y_g)] <- y_g[!is.na(y_g)]

  eta_w <- draw_eta_given(
    w,
    mu_w,
    S_w,
    mom$lambda[[b_w]],
    mom$eeta[[b_w]],
    mom$veta[[b_w]]
  )
  eta_b <- draw_eta_given(
    u,
    mu_b,
    S_b,
    mom$lambda[[b_b]],
    mom$eeta[[b_b]],
    mom$veta[[b_b]]
  )
  # Dummy latent variables are their observed variables
  for (b in c(b_w, b_b)) {
    dlv <- c(
      lavmodel_x@ov.x.dummy.lv.idx[[b]],
      lavmodel_x@ov.y.dummy.lv.idx[[b]]
    )
    dov <- c(
      lavmodel_x@ov.x.dummy.ov.idx[[b]],
      lavmodel_x@ov.y.dummy.ov.idx[[b]]
    )
    if (b == b_w) {
      eta_w[, dlv] <- w[, dov, drop = FALSE]
    } else {
      eta_b[, dlv] <- u[, dov, drop = FALSE]
    }
  }

  list(
    y = y_full,
    u = u,
    eta_w = eta_w,
    eta_b = eta_b,
    A = A,
    z_pos = z_pos,
    mu_w = mu_w,
    mu_b = mu_b
  )
}
# nocov end

#' @exportS3Method predict inlavaan_internal
#' @keywords internal
predict.inlavaan_internal <- function(
  object,
  type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"),
  newdata = NULL,
  level = 1L,
  nsamp = 250,
  ymis_only = FALSE,
  summary = FALSE,
  ...
) {
  type <- match.arg(type)
  # Aliases: "ov"/"yhat" -> "yhat"; "ypred"/"ydist" -> "ypred";
  #          "ymis"/"ovmis" -> "ymis"
  if (type == "ov") {
    type <- "yhat"
  }
  if (type == "ydist") {
    type <- "ypred"
  }
  if (type == "ovmis") {
    type <- "ymis"
  }

  theta_star <- object$theta_star
  Sigma_theta <- object$Sigma_theta
  approx_data <- object$approx_data
  pt <- object$partable
  lavmodel <- object$lavmodel
  lavdata <- object$lavdata
  nlevels <- lavdata@nlevels

  # Error early: ymis does not support newdata
  if (type == "ymis" && !is.null(newdata)) {
    cli_abort("Type {.val ymis} does not support {.arg newdata}.")
  }

  # Multilevel restrictions
  if (is_multilevel(lavdata)) {
    # nocov start
    if (!is.null(newdata)) {
      cli_abort("{.arg newdata} is not supported for multilevel models.")
    }
    if (!level %in% c(1L, 2L)) {
      cli_abort("{.arg level} must be {.val 1} or {.val 2}.")
    }
  } # nocov end

  # Handle newdata: rebuild lavdata matrices
  if (!is.null(newdata)) {
    new_ld <- build_newdata(newdata, lavdata)
    y <- new_ld$X
    x_exo <- new_ld$eXo
    nG <- new_ld$ngroups
    group_labels <- new_ld$group.label
    nobs_out <- new_ld$nobs
  } else {
    y <- lavdata@X
    x_exo <- lavdata@eXo
    nG <- lavdata@ngroups
    group_labels <- lavdata@group.label
    nobs_out <- lavdata@nobs
  }

  # Saturated means for models without a mean structure: the implied mean
  # does not exist, but the conditioning kernels below still need one. The
  # sample means of the *fitted* data are the correct plug-in (they are
  # the saturated means the fit conditions on), also for newdata.
  ybar_fit <- lapply(lavdata@X, colMeans)

  # Draw exactly as the fit itself does: inherit the recorded `samp_copula`
  # choice and pass the NORTA-adjusted correlation matrix, so the factor
  # scores share a dependence structure with the fit's own draws.
  samp <- sample_params_posterior(
    object,
    nsamp = nsamp,
    samp_copula = object$samp_copula %||% TRUE
  )
  x_samp <- samp$x_samp

  # ---- type = "lv": Posterior draws of latent variable scores ----
  if (type == "lv") {
    if (is_multilevel(lavdata)) {
      # nocov start
      # ---- Multilevel path: use lavaan internals ----
      lavsamplestats <- object$lavsamplestats

      # Helper: get LV names from the psi dimNames for a given block
      get_lv_names <- function(lavmodel_x, block) {
        nmat <- lavmodel_x@nmat
        mm <- seq_len(nmat[block]) + cumsum(c(0, nmat))[block]
        psi_pos <- which(names(lavmodel_x@GLIST[mm]) == "psi")
        lavmodel_x@dimNames[[mm[psi_pos]]][[1]]
      }

      sample_lv_ml <- function(xx) {
        lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
        lavimplied <- lavaan::lav_model_implied(lavmodel_x)
        mom <- ml_moments(lavmodel_x, lavsamplestats)

        out <- vector("list", nG)
        names(out) <- group_labels

        for (g in seq_len(nG)) {
          dr <- draw_ml_group(
            y[[g]],
            lavdata@Lp[[g]],
            lavimplied,
            mom,
            lavmodel_x,
            g
          )
          FS.g <- if (level == 1L) dr$eta_w else dr$eta_b
          colnames(FS.g) <- get_lv_names(lavmodel_x, (g - 1) * nlevels + level)
          out[[g]] <- FS.g
        }

        if (nG == 1L) {
          out <- out[[1L]]
        } else {
          out <- do.call(
            rbind,
            Map(function(g, df) data.frame(group = g, df), names(out), out)
          )
        }
        rownames(out) <- NULL
        out
      }

      out <- vector("list", nsamp)
      cli_progress_bar(
        "Sampling latent variables (multilevel)",
        total = nsamp,
        clear = FALSE
      )
      for (i in seq_len(nsamp)) {
        out[[i]] <- sample_lv_ml(x_samp[i, ])
        cli_progress_update()
      }
      cli_progress_done()
    } else {
      # nocov end
      # ---- Single-level path: full posterior draw ----
      sample_lv <- function(xx) {
        GLIST <- get_SEM_param_matrix(xx, "all", lavmodel)
        out <- vector("list", nG)
        names(out) <- group_labels
        for (g in seq_len(nG)) {
          glist <- GLIST[[g]]
          Lambda <- glist$lambda
          Psi <- glist$psi
          Theta <- glist$theta
          B <- glist$beta
          alpha <- glist$alpha
          dummy <- dummy_lv_idx(lavmodel, g)

          if (is.null(alpha)) {
            alpha <- 0
          }

          if (is.null(B)) {
            Phi <- Psi
            front <- Lambda
          } else {
            IminB_inv <- solve(diag(nrow(B)) - B)
            Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
            front <- Lambda %*% IminB_inv
          }

          Sigmay_inv <- solve(front %*% Psi %*% t(front) + Theta)
          PhiLtSinv <- Phi %*% t(Lambda) %*% Sigmay_inv

          # E(eta | y) = E(eta) + Phi Lambda' Sigma^{-1} (y - mu_y): centre
          # by the implied mean, or the saturated means without one
          alpha_vec <- eta_intercepts(alpha, glist, front, dummy, ybar_fit[[g]])
          mu_y <- if (!is.null(glist$nu)) {
            as.numeric(glist$nu + front %*% alpha_vec)
          } else {
            ybar_fit[[g]]
          }
          yc <- sweep(y[[g]], 2L, mu_y)
          eeta <- if (is.null(B)) alpha_vec else IminB_inv %*% alpha_vec
          gx <- gamma_x(glist, x_exo, g)
          if (!is.null(gx)) {
            eta_x <- if (is.null(B)) gx else tcrossprod(gx, IminB_inv)
            yc <- yc - tcrossprod(eta_x, Lambda)
          }
          mu_eta <- t(as.numeric(eeta) + PhiLtSinv %*% t(yc))
          if (!is.null(gx)) {
            mu_eta <- mu_eta + eta_x
          }

          V_eta <- Phi - PhiLtSinv %*% Lambda %*% Phi
          outg <- draw_eta(mu_eta, V_eta, y[[g]], dummy)

          out[[g]] <- outg
        }

        if (nG == 1L) {
          colnames(out[[1L]]) <- colnames(Psi)
          out <- out[[1L]]
        } else {
          out <- do.call(
            rbind,
            Map(function(g, df) data.frame(group = g, df), names(out), out)
          )
          colnames(out)[-1] <- colnames(Psi)
        }
        rownames(out) <- NULL
        out
      }

      out <- vector("list", nsamp)
      cli_progress_bar(
        "Sampling latent variables",
        total = nsamp,
        clear = FALSE
      )
      for (i in seq_len(nsamp)) {
        out[[i]] <- sample_lv(x_samp[i, ])
        cli_progress_update()
      }
      cli_progress_done()
    }

    # ---- type = "yhat": Predicted means E(y | eta, theta) ----
    # ---- type = "ypred": Predicted values y = E(y|eta,theta) + eps ----
  } else if (type %in% c("yhat", "ypred")) {
    add_noise <- (type == "ypred")

    if (nlevels > 1L) {
      # nocov start
      # ---- Multilevel yhat/ypred ----
      lavsamplestats <- object$lavsamplestats

      sample_yhat_ml <- function(xx) {
        lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
        lavimplied <- lavaan::lav_model_implied(lavmodel_x)
        mom <- ml_moments(lavmodel_x, lavsamplestats)
        # Loadings that predict an observed outcome from its regressors
        LAMBDA <- lavaan___lav_model_lambda(lavmodel = lavmodel_x)
        nmat <- lavmodel_x@nmat

        out <- vector("list", nG)
        names(out) <- group_labels

        for (g in seq_len(nG)) {
          b_w <- (g - 1) * nlevels + 1 # within block
          b_b <- (g - 1) * nlevels + 2 # between block
          Lp <- lavdata@Lp[[g]]
          cl <- Lp$cluster.idx[[2]]
          ov.idx <- Lp$ov.idx
          n_obs <- nrow(y[[g]])
          p <- ncol(y[[g]])

          dr <- draw_ml_group(y[[g]], Lp, lavimplied, mom, lavmodel_x, g)
          eta_w_c <- sweep(dr$eta_w, 2, mom$eeta[[b_w]])
          yhat_w <- sweep(tcrossprod(eta_w_c, LAMBDA[[b_w]]), 2, dr$mu_w, "+")
          eta_b_c <- sweep(dr$eta_b, 2, mom$eeta[[b_b]])
          yhat_b <- sweep(tcrossprod(eta_b_c, LAMBDA[[b_b]]), 2, dr$mu_b, "+")

          yhat <- matrix(0, n_obs, p)
          if (!add_noise) {
            yhat[, ov.idx[[1]]] <- yhat_w
            yhat[, ov.idx[[2]]] <- yhat[, ov.idx[[2]], drop = FALSE] +
              yhat_b[cl, , drop = FALSE]
          } else {
            # A replicate of the same row in the same cluster: the cluster's
            # between values u are kept, and only the observation-level
            # residuals are new (within, and of the between-only variables).
            mm_w <- seq_len(nmat[b_w]) + cumsum(c(0, nmat))[b_w]
            glist_w <- lavmodel_x@GLIST[mm_w]
            eps_w <- draw_residuals(
              n_obs,
              glist_w$theta,
              glist_w$psi,
              lavmodel_x,
              b_w
            )
            yhat[, ov.idx[[1]]] <- yhat_w +
              eps_w +
              dr$u[cl, , drop = FALSE] %*% t(dr$A)
            z_pos <- dr$z_pos
            if (length(z_pos) > 0L) {
              mm_b <- seq_len(nmat[b_b]) + cumsum(c(0, nmat))[b_b]
              glist_b <- lavmodel_x@GLIST[mm_b]
              eps_b <- draw_residuals(
                Lp$nclusters[[2]],
                glist_b$theta,
                glist_b$psi,
                lavmodel_x,
                b_b
              )
              z_rep <- yhat_b[, z_pos, drop = FALSE] +
                eps_b[, z_pos, drop = FALSE]
              yhat[, ov.idx[[2]][z_pos]] <- z_rep[cl, , drop = FALSE]
            }
          }

          cn <- colnames(y[[g]])
          if (is.null(cn)) {
            cn <- lavdata@ov.names[[g]]
          }
          colnames(yhat) <- cn
          out[[g]] <- yhat
        }

        if (nG == 1L) {
          out <- out[[1L]]
        } else {
          out <- do.call(
            rbind,
            Map(function(g, df) data.frame(group = g, df), names(out), out)
          )
        }
        rownames(out) <- NULL
        out
      }

      msg <- if (add_noise) {
        "Sampling predicted values (multilevel)"
      } else {
        "Sampling fitted values (multilevel)"
      }
      out <- vector("list", nsamp)
      cli_progress_bar(msg, total = nsamp, clear = FALSE)
      for (i in seq_len(nsamp)) {
        out[[i]] <- sample_yhat_ml(x_samp[i, ])
        cli_progress_update()
      }
      cli_progress_done()
    } else {
      # nocov end
      # ---- Single-level yhat/ypred ----
      sample_yhat <- function(xx) {
        GLIST <- get_SEM_param_matrix(xx, "all", lavmodel)
        out <- vector("list", nG)
        names(out) <- group_labels

        for (g in seq_len(nG)) {
          glist <- GLIST[[g]]
          Lambda <- glist$lambda
          Psi <- glist$psi
          Theta <- glist$theta
          B <- glist$beta
          alpha <- glist$alpha
          nu <- glist$nu
          dummy <- dummy_lv_idx(lavmodel, g)

          if (is.null(alpha)) {
            alpha <- rep(0, ncol(Lambda))
          }
          if (is.null(nu)) {
            nu <- rep(0, nrow(Lambda))
          }

          if (is.null(B)) {
            Phi <- Psi
            front <- Lambda
          } else {
            IminB_inv <- solve(diag(nrow(B)) - B)
            Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
            front <- Lambda %*% IminB_inv
          }

          Sigmay_inv <- solve(front %*% Psi %*% t(front) + Theta)
          PhiLtSinv <- Phi %*% t(Lambda) %*% Sigmay_inv

          # Posterior draw of eta | y, theta, centring by the implied mean
          # (or the saturated means when no mean structure exists)
          alpha_vec <- eta_intercepts(alpha, glist, front, dummy, ybar_fit[[g]])
          mu_y <- if (!is.null(glist$nu)) {
            as.numeric(glist$nu + front %*% alpha_vec)
          } else {
            ybar_fit[[g]]
          }
          yc <- sweep(y[[g]], 2L, mu_y)
          eeta <- if (is.null(B)) alpha_vec else IminB_inv %*% alpha_vec
          gx <- gamma_x(glist, x_exo, g)
          if (!is.null(gx)) {
            eta_x <- if (is.null(B)) gx else tcrossprod(gx, IminB_inv)
            yc <- yc - tcrossprod(eta_x, Lambda)
          }
          mu_eta <- t(as.numeric(eeta) + PhiLtSinv %*% t(yc))
          if (!is.null(gx)) {
            mu_eta <- mu_eta + eta_x
          }
          V_eta <- Phi - PhiLtSinv %*% Lambda %*% Phi
          eta_draw <- draw_eta(mu_eta, V_eta, y[[g]], dummy)
          n_obs <- nrow(mu_eta)

          # An observed endogenous variable carried as a dummy latent variable
          # is predicted from its regressors rather than copied from the data.
          ydum <- lavmodel@ov.y.dummy.lv.idx[[g]]
          if (length(ydum) > 0L) {
            pred <- matrix(alpha_vec[ydum], n_obs, length(ydum), byrow = TRUE)
            if (!is.null(B)) {
              pred <- pred + tcrossprod(eta_draw, B[ydum, , drop = FALSE])
            }
            if (!is.null(gx)) {
              pred <- pred + gx[, ydum, drop = FALSE]
            }
            eta_draw[, ydum] <- pred
          }

          # yhat = nu + Lambda eta = mu_y + Lambda (eta - E(eta))
          nu_eff <- mu_y - as.numeric(front %*% alpha_vec)
          yhat <- sweep(tcrossprod(eta_draw, Lambda), 2, nu_eff, "+")

          # Residual noise for ypred
          if (add_noise) {
            yhat <- yhat + draw_residuals(n_obs, Theta, Psi, lavmodel, g)
          }

          out[[g]] <- yhat
        }

        if (nG == 1L) {
          cn <- colnames(y[[1L]])
          if (is.null(cn)) {
            cn <- lavdata@ov.names[[1L]]
          }
          colnames(out[[1L]]) <- cn
          out <- out[[1L]]
        } else {
          out <- do.call(
            rbind,
            Map(function(g, df) data.frame(group = g, df), names(out), out)
          )
          cn <- colnames(y[[1L]])
          if (is.null(cn)) {
            cn <- lavdata@ov.names[[1L]]
          }
          colnames(out)[-1] <- cn
        }
        rownames(out) <- NULL
        out
      }

      msg <- if (add_noise) {
        "Sampling predicted values"
      } else {
        "Sampling fitted values"
      }
      out <- vector("list", nsamp)
      cli_progress_bar(msg, total = nsamp, clear = FALSE)
      for (i in seq_len(nsamp)) {
        out[[i]] <- sample_yhat(x_samp[i, ])
        cli_progress_update()
      }
      cli_progress_done()
    }

    # ---- type = "ymis": Missing data imputation ----
  } else if (type == "ymis") {
    # nocov start
    # For each posterior draw of model parameters, compute the model-implied
    # covariance Sigma(theta) and mean mu(theta), then draw missing values
    # from their conditional distribution given observed values:
    #   y_mis | y_obs, theta ~ N(mu_cond, Sigma_cond)
    nlevels <- lavdata@nlevels

    sample_ymis <- function(xx) {
      lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
      lavimplied <- lavaan::lav_model_implied(lavmodel_x)

      out <- vector("list", nG)
      names(out) <- group_labels

      for (g in seq_len(nG)) {
        yg <- y[[g]]
        p <- ncol(yg)
        n_obs <- nrow(yg)
        outg <- yg

        if (nlevels > 1L) {
          # nocov start
          # Two-level: draw the missing values jointly with each cluster's
          # between values (see draw_ml_group())
          mom <- ml_moments(lavmodel_x, object$lavsamplestats)
          out[[g]] <- draw_ml_group(
            yg,
            lavdata@Lp[[g]],
            lavimplied,
            mom,
            lavmodel_x,
            g
          )$y
          next
        } # nocov end

        Sigma_y <- lavimplied$cov[[g]]
        mu_y <- if (!is.null(lavimplied$mean)) {
          as.numeric(lavimplied$mean[[g]])
        } else {
          # no mean structure: condition on the saturated (sample) means
          # (defensive: missing = "ML" forces a mean structure in lavaan,
          # so this branch is unreachable from a real fit)
          ybar_fit[[g]] # nocov
        }

        # Detect missing values
        na_mat <- is.na(yg)
        if (!any(na_mat)) {
          out[[g]] <- outg
          next
        }

        # Group cases by missing-data pattern for efficiency
        patterns <- apply(na_mat, 1, function(r) {
          paste(which(r), collapse = ",")
        })
        unique_patterns <- unique(patterns[patterns != ""])

        for (pat in unique_patterns) {
          mis_idx <- as.integer(strsplit(pat, ",")[[1]])
          obs_idx <- setdiff(seq_len(p), mis_idx)
          case_rows <- which(patterns == pat)

          Sigma_oo <- Sigma_y[obs_idx, obs_idx, drop = FALSE]
          Sigma_mo <- Sigma_y[mis_idx, obs_idx, drop = FALSE]
          Sigma_mm <- Sigma_y[mis_idx, mis_idx, drop = FALSE]

          A <- Sigma_mo %*% solve(Sigma_oo)
          Sigma_cond <- Sigma_mm - A %*% t(Sigma_mo)
          Sigma_cond <- (Sigma_cond + t(Sigma_cond)) / 2
          chol_cond <- psd_root(Sigma_cond)

          n_mis <- length(mis_idx)
          n_cases <- length(case_rows)

          y_obs_centred <- yg[case_rows, obs_idx, drop = FALSE] -
            matrix(
              mu_y[obs_idx],
              nrow = n_cases,
              ncol = length(obs_idx),
              byrow = TRUE
            )
          mu_cond <- matrix(
            mu_y[mis_idx],
            nrow = n_cases,
            ncol = n_mis,
            byrow = TRUE
          ) +
            y_obs_centred %*% t(A)

          Z <- matrix(rnorm(n_cases * n_mis), nrow = n_mis, ncol = n_cases)
          draws <- mu_cond + t(chol_cond %*% Z)
          outg[case_rows, mis_idx] <- draws
        }

        out[[g]] <- outg
      }

      if (ymis_only) {
        # Return only the imputed cells as a named vector: "varname[rowindex]"
        row_offset <- 0L
        imp_list <- vector("list", nG)
        for (g in seq_len(nG)) {
          yg_orig <- y[[g]]
          var_nms <- colnames(yg_orig)
          if (is.null(var_nms)) {
            var_nms <- lavdata@ov.names[[g]]
          }
          na_pos <- which(is.na(yg_orig), arr.ind = TRUE)
          na_pos <- na_pos[order(na_pos[, 1L], na_pos[, 2L]), , drop = FALSE]
          if (nrow(na_pos) == 0L) {
            imp_list[[g]] <- numeric(0L)
          } else {
            vals <- out[[g]][na_pos]
            nms <- paste0(
              var_nms[na_pos[, 2L]],
              "[",
              na_pos[, 1L] + row_offset,
              "]"
            )
            names(vals) <- nms
            imp_list[[g]] <- vals
          }
          row_offset <- row_offset + nrow(yg_orig)
        }
        return(do.call(c, imp_list))
      }

      if (nG == 1L) {
        cn <- colnames(y[[1L]])
        if (is.null(cn)) {
          cn <- lavdata@ov.names[[1L]]
        }
        colnames(out[[1L]]) <- cn
        out <- out[[1L]]
      } else {
        out <- do.call(
          rbind,
          Map(function(g, df) data.frame(group = g, df), names(out), out)
        )
      }
      rownames(out) <- NULL
      out
    }

    out <- vector("list", nsamp)
    cli_progress_bar("Imputing missing values", total = nsamp, clear = FALSE)
    for (i in seq_len(nsamp)) {
      out[[i]] <- sample_ymis(x_samp[i, ])
      cli_progress_update()
    }
    cli_progress_done()
  } # nocov end

  attr(out, "nobs") <- nobs_out
  attr(out, "type") <- type
  out <- structure(out, class = "predict.inlavaan_internal")
  if (isTRUE(summary)) {
    return(base::summary(out))
  }
  out
}

#' @exportS3Method print predict.inlavaan_internal
#' @keywords internal
print.predict.inlavaan_internal <- function(
  x,
  n = 10L,
  nd = 3L,
  ...
) {
  type <- attr(x, "type")
  cat("Predicted values from inlavaan model")
  if (!is.null(type)) {
    cat(sprintf(" (type = \"%s\")", type))
  }
  cat("\n")
  cat("Number of samples:", length(x), "\n")
  cat("First sample:\n")

  first <- x[[1L]]

  # Named vector (ymis_only output)
  if (is.numeric(first) && !is.matrix(first) && !is.data.frame(first)) {
    # nocov start
    n_total <- length(first)
    if (n_total > n) {
      print(first[seq_len(n)], digits = nd)
      cat(col_grey(paste0(
        "# ",
        symbol$info,
        " ",
        n_total - n,
        " more value",
        if (n_total - n == 1L) "" else "s",
        "\n"
      )))
      cat(col_grey(paste0(
        "# ",
        symbol$info,
        " Use `summary()` to see summary statistics\n"
      )))
    } else {
      print(first, digits = nd)
    }
    return(invisible(x))
  } # nocov end

  # Matrix / data frame output
  nr <- nrow(first)
  if (!is.null(nr) && nr > n) {
    print(as.data.frame(first[seq_len(n), , drop = FALSE]), digits = nd)
    cat(col_grey(paste0(
      "# ",
      symbol$info,
      " ",
      nr - n,
      " more row",
      if (nr - n == 1L) "" else "s",
      "\n"
    )))
    cat(col_grey(paste0(
      "# ",
      symbol$info,
      " Use `summary()` to see summary statistics\n"
    )))
  } else {
    print(as.data.frame(first), digits = nd)
  }
  invisible(x)
}

#' @exportS3Method summary predict.inlavaan_internal
#' @keywords internal
summary.predict.inlavaan_internal <- function(object, ...) {
  is_group <- FALSE
  if (!is.null(names(object[[1]])[1])) {
    is_group <- names(object[[1]])[1] == "group" &
      is.character(object[[1]][, 1])
  }

  if (is_group) {
    # Remove the group column, assuming it's always the first column
    group_id <- object[[1]][, 1]
    object <- lapply(object, function(df) as.matrix(df[-1]))
  } else {
    group_id <- NULL
  }
  arr <- simplify2array(object)

  Mean <- apply(arr, c(1, 2), mean)
  SD <- apply(arr, c(1, 2), sd)
  Q <- apply(arr, c(1, 2), quantile, probs = c(0.025, 0.5, 0.975))
  Mode <- apply(arr, c(1, 2), function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
  })

  res <- list(
    group_id = group_id,
    Mean = Mean,
    SD = SD,
    `2.5%` = Q[1, , ],
    `50%` = Q[2, , ],
    `97.5%` = Q[3, , ],
    Mode = Mode
  )
  structure(res, class = "summary.predict.inlavaan_internal")
}

#' @exportS3Method print summary.predict.inlavaan_internal
#' @keywords internal
print.summary.predict.inlavaan_internal <- function(
  x,
  stat = "Mean",
  n = 10L,
  nd = 3L,
  ...
) {
  cat(paste0(stat, " of predicted values from inlavaan model\n\n"))
  mat <- x[[stat]]
  nr <- nrow(mat)
  if (!is.null(nr) && nr > n) {
    print(as.data.frame(mat[seq_len(n), , drop = FALSE]), digits = nd)
    cat(col_grey(paste0(
      "# ",
      symbol$info,
      " ",
      nr - n,
      " more row",
      if (nr - n == 1L) "" else "s",
      "\n"
    )))
  } else {
    print(as.data.frame(mat), digits = nd)
  }
}

# #' @exportS3Method plot predict.inlavaan_internal
# #' @keywords internal
# plot.predict.inlavaan_internal <- function(x, nrow = NULL, ncol = NULL, ...) {
#   summ <- summary(x)
#   nobs <- attr(x, "nobs")
#   nG <- length(nobs)
#   groups <- rep(seq_len(nG), times = nobs)
#
#   means <- summ$Mean
#   score <- rowSums(means)
#   ranks <- order(score, decreasing = TRUE)
#   summ <- lapply(summ, function(mat) cbind(group = groups, mat))
#
#   plot_df <-
#     bind_rows(lapply(summ, function(x) {
#       as.data.frame(x) |>
#         rownames_to_column("id")
#     }), .id = "statistic") |>
#     pivot_longer(-c(statistic, id, group), names_to = "var", values_to = "val") |>
#     pivot_wider(names_from = statistic, values_from = val) |>
#     mutate(
#       id = factor(id, levels = ranks),
#       group = factor(group, labels = attr(x, "group.label"))
#     )
#
#   if (nG > 1L) {
#     browser()
#     p <-
#       ggplot(plot_df) +
#       geom_pointrange(aes(x = id, y = Mean, ymin = `2.5%`, ymax = `97.5%`, col = group), size = 0)
#   } else {
#     p <-
#       ggplot(plot_df) +
#       geom_pointrange(aes(x = id, y = Mean, ymin = `2.5%`, ymax = `97.5%`), size = 0)
#   }
#
#   p +
#     facet_wrap(~ var, nrow = nrow, ncol = ncol) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5)) +
#     labs(x = "Individual ID", y = "Value")
#
# }

#' Posterior Predictions for INLAvaan Models
#'
#' Compute posterior predictions from a fitted \code{INLAvaan} model,
#' including latent variable scores, predicted observed values, and imputed
#' missing data.
#'
#' @param object An object of class [INLAvaan].
#' @param type Character string specifying the type of prediction:
#'   \describe{
#'     \item{\code{"lv"}}{(default) Posterior draws of latent variable scores
#'       \eqn{\eta | y, \theta}.}
#'     \item{\code{"yhat"}, \code{"ov"}}{Predicted means for observed variables
#'       \eqn{E(y | \eta, \theta) = \nu + \Lambda \eta}; no residual noise. An
#'       observed outcome is predicted from its regressors.}
#'     \item{\code{"ypred"}, \code{"ydist"}}{Predicted observed values including
#'       residual noise \eqn{y = \nu + \Lambda \eta + \varepsilon},
#'       \eqn{\varepsilon \sim N(0, \Theta)}, with the residual variances that
#'       \code{lavInspect(fit, "theta")} reports (observed outcomes included,
#'       observed covariates excluded).}
#'     \item{\code{"ymis"}, \code{"ovmis"}}{Imputed values for missing
#'       observations, drawn from the conditional distribution
#'       \eqn{y_{mis} | y_{obs}, \theta}.}
#'   }
#' @param newdata An optional data frame of new observations. If supplied,
#'   predictions are computed for \code{newdata} rather than the original
#'   training data. Not supported for \code{type = "ymis"}.
#' @param level Integer; for \code{type = "lv"} in two-level models, specifies
#'   whether level 1 or level 2 latent variables are desired (default \code{1L}).
#'   Other types ignore it: for two-level models, \code{"yhat"} and
#'   \code{"ypred"} give the total within plus between prediction.
#' @param nsamp Integer; number of posterior samples to use for prediction.
#'   Defaults to \code{1000}.
#' @param ymis_only Logical; only applies when \code{type = "ymis"}. When
#'   \code{TRUE}, returns only the imputed values as a named numeric vector per
#'   sample (names of the form \code{"varname[rowindex]"}, matching the blavaan
#'   convention). When \code{FALSE} (default), returns the full data matrix with
#'   missing values filled in.
#' @param summary Logical. When \code{TRUE}, collapse the posterior draws with
#'   \code{summary()} and return summary statistics (mean, SD, quantiles, mode)
#'   instead of the raw draws -- equivalent to calling
#'   \code{summary(predict(object, ...))} but without materialising the
#'   intermediate draws object. Default \code{FALSE}.
#' @param ... Currently unused.
#'
#' @returns A list of \code{nsamp} posterior draws, each a matrix (or data
#'   frame, for multiple groups) with rows corresponding to cases and columns
#'   to variables or latent factors. When \code{summary = TRUE}, instead
#'   returns a \code{summary.predict.inlavaan_internal} object with the
#'   posterior mean, SD, quantiles, and mode for each case/variable.
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
#'             test = "none", verbose = FALSE)
#'
#' # Posterior latent variable scores
#' lv_scores <- predict(fit)
#' head(lv_scores)
#'
#' # Predicted observed variable means
#' yhat <- predict(fit, type = "yhat")
#' head(yhat)
#'
#' # Point estimates only, skipping the manual summary() step
#' predict(fit, type = "yhat", summary = TRUE)
#' }
#'
#' @seealso [sampling()], [simulate()], [summary()]
#'
#' @name predict
#' @rdname predict
#' @aliases predict,INLAvaan-method
#' @export
setMethod(
  "predict",
  "INLAvaan",
  function(
    object,
    type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"),
    newdata = NULL,
    level = 1L,
    nsamp = 1000,
    ymis_only = FALSE,
    summary = FALSE,
    ...
  ) {
    type <- match.arg(type)
    predict.inlavaan_internal(
      object@external$inlavaan_internal,
      type = type,
      newdata = newdata,
      level = level,
      nsamp = nsamp,
      ymis_only = ymis_only,
      summary = summary,
      ...
    )
  }
)
