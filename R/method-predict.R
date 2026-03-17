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

# Helper: build data matrices from newdata, reusing metadata from lavdata
build_newdata <- function(newdata, lavdata) {
  nG <- lavdata@ngroups
  grp <- lavdata@group
  has_group <- length(grp) > 0L && nzchar(grp)

  if (has_group) {
    group_labels <- lavdata@group.label
    groups_in_data <- as.character(newdata[[grp]])
    new_X <- vector("list", nG)
    new_nobs <- vector("list", nG)
    for (g in seq_len(nG)) {
      rows_g <- which(groups_in_data == group_labels[g])
      ov_names_g <- lavdata@ov.names[[g]]
      new_X[[g]] <- as.matrix(newdata[rows_g, ov_names_g, drop = FALSE])
      new_nobs[[g]] <- length(rows_g)
    }
  } else {
    ov_names <- lavdata@ov.names[[1L]]
    new_X <- list(as.matrix(newdata[, ov_names, drop = FALSE]))
    new_nobs <- list(nrow(newdata))
  }

  list(
    X = new_X,
    ngroups = nG,
    group.label = if (has_group) lavdata@group.label else character(0),
    nobs = new_nobs
  )
}

# Helper: compute factor scores for multilevel blocks.
# Returns an n x nfac matrix of factor scores.
compute_fs_ml <- function(data_block, VETA_b, LAMBDA_b, Sigma_hat_b,
                          Sigma_inv_b, EETA_b, EY_b) {
  n <- nrow(data_block)
  nfac <- ncol(VETA_b)
  if (nfac == 0L) return(matrix(0, n, nfac))

  Yc <- t(t(data_block) - EY_b)
  FSC <- VETA_b %*% t(LAMBDA_b) %*% Sigma_inv_b
  t(FSC %*% t(Yc) + EETA_b)
}

# Impute missing values in multilevel data by sampling from the conditional
# posterior: y_mis | y_obs, theta ~ N(mu_cond, Sigma_cond).
# Returns the data matrix with NAs replaced by posterior draws.
impute_ml_data <- function(yg, lavimplied, g, nlevels,
                           ov_names_block, lavdata) {
  if (!any(is.na(yg))) return(yg)

  p <- ncol(yg)
  var_names <- colnames(yg)
  if (is.null(var_names)) var_names <- lavdata@ov.names[[g]]

  block_w <- (g - 1) * nlevels + 1
  block_b <- (g - 1) * nlevels + 2

  Sigma_w <- lavimplied$cov[[block_w]]
  Sigma_b <- lavimplied$cov[[block_b]]

  nw <- ov_names_block[[block_w]]
  if (length(nw) == nrow(Sigma_w)) {
    rownames(Sigma_w) <- colnames(Sigma_w) <- nw
  }
  nb <- ov_names_block[[block_b]]
  if (length(nb) == nrow(Sigma_b)) {
    rownames(Sigma_b) <- colnames(Sigma_b) <- nb
  }

  Sigma_y <- matrix(0, p, p, dimnames = list(var_names, var_names))

  vn_w <- rownames(Sigma_w)
  w_in_data <- match(vn_w, var_names)
  w_keep <- !is.na(w_in_data)
  if (any(w_keep)) {
    idx <- w_in_data[w_keep]
    Sigma_y[idx, idx] <- Sigma_y[idx, idx] +
      Sigma_w[w_keep, w_keep, drop = FALSE]
  }

  vn_b <- rownames(Sigma_b)
  b_in_data <- match(vn_b, var_names)
  b_keep <- !is.na(b_in_data)
  if (any(b_keep)) {
    idx <- b_in_data[b_keep]
    Sigma_y[idx, idx] <- Sigma_y[idx, idx] +
      Sigma_b[b_keep, b_keep, drop = FALSE]
  }

  mu_y <- rep(0, p)
  names(mu_y) <- var_names
  if (!is.null(lavimplied$mean)) {
    mu_b <- as.numeric(lavimplied$mean[[block_b]])
    names(mu_b) <- nb
    b_match <- intersect(var_names, nb)
    mu_y[match(b_match, var_names)] <- mu_b[b_match]
  }

  na_mat <- is.na(yg)
  patterns <- apply(na_mat, 1, function(r) paste(which(r), collapse = ","))
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
    chol_cond <- t(chol(Sigma_cond))

    n_mis <- length(mis_idx)
    n_cases <- length(case_rows)

    y_obs_c <- yg[case_rows, obs_idx, drop = FALSE] -
      matrix(mu_y[obs_idx], nrow = n_cases,
             ncol = length(obs_idx), byrow = TRUE)
    mu_cond <- matrix(mu_y[mis_idx], nrow = n_cases,
                      ncol = n_mis, byrow = TRUE) +
      y_obs_c %*% t(A)

    Z <- matrix(rnorm(n_cases * n_mis), nrow = n_mis, ncol = n_cases)
    yg[case_rows, mis_idx] <- mu_cond + t(chol_cond %*% Z)
  }
  yg
}

# Compute cluster random effects (BLUP) directly from (imputed) data,
# bypassing the precomputed YLp which may contain NAs.
compute_ml_ranef <- function(y_g, Lp, decomp) {
  nclusters <- Lp$nclusters[[2]]
  cluster_idx <- Lp$cluster.idx[[2]]
  ov_idx <- Lp$ov.idx[[1]]
  between_idx <- Lp$between.idx[[2]]
  nvar <- length(ov_idx)

  mu_y <- decomp$mu.w + decomp$mu.b
  MB.j <- matrix(0, nclusters, nvar)

  has_between <- length(between_idx) > 0L
  if (has_between) {
    sigma_1 <- cbind(decomp$sigma.yz, decomp$sigma.b)
    mu_all <- c(decomp$mu.z, mu_y)
  } else {
    sigma_1 <- decomp$sigma.b
    mu_all <- mu_y
  }

  for (cl in seq_len(nclusters)) {
    obs_in_cl <- which(cluster_idx == cl)
    nj <- length(obs_in_cl)
    ybar <- colMeans(y_g[obs_in_cl, ov_idx, drop = FALSE])

    if (has_between) {
      b_vals <- y_g[obs_in_cl[1L], between_idx, drop = TRUE]
      b_j <- c(b_vals, ybar)
      sigma_j <- decomp$sigma.w + nj * decomp$sigma.b
      omega_j <- rbind(
        cbind(decomp$sigma.zz, t(decomp$sigma.yz)),
        cbind(decomp$sigma.yz, (1 / nj) * sigma_j)
      )
    } else {
      b_j <- ybar
      omega_j <- (1 / nj) * (decomp$sigma.w + nj * decomp$sigma.b)
    }

    omega_j_inv <- solve(omega_j)
    MB.j[cl, ] <- as.numeric(
      decomp$mu.b + sigma_1 %*% omega_j_inv %*% (b_j - mu_all)
    )
  }
  MB.j
}

#' @exportS3Method predict inlavaan_internal
#' @keywords internal
predict.inlavaan_internal <- function(
  object,
  type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"),
  newdata = NULL,
  level = 1L,
  nsamp = 250,
  ymis_only = FALSE,
  ...
) {
  type <- match.arg(type)
  # Aliases: "ov"/"yhat" -> "yhat"; "ypred"/"ydist" -> "ypred";
  #          "ymis"/"ovmis" -> "ymis"
  if (type == "ov") type <- "yhat"
  if (type == "ydist") type <- "ypred"
  if (type == "ovmis") type <- "ymis"

  theta_star <- object$theta_star
  Sigma_theta <- object$Sigma_theta
  marginal_method <- object$marginal_method
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
  if (nlevels > 1L) {
    if (!is.null(newdata)) {
      cli_abort("{.arg newdata} is not supported for multilevel models.")
    }
    if (!level %in% c(1L, 2L)) {
      cli_abort("{.arg level} must be {.val 1} or {.val 2}.")
    }
  }

  # Handle newdata: rebuild lavdata matrices
  if (!is.null(newdata)) {
    new_ld <- build_newdata(newdata, lavdata)
    y <- new_ld$X
    nG <- new_ld$ngroups
    group_labels <- new_ld$group.label
    nobs_out <- new_ld$nobs
  } else {
    y <- lavdata@X
    nG <- lavdata@ngroups
    group_labels <- lavdata@group.label
    nobs_out <- lavdata@nobs
  }

  samp <- sample_params(
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    method = marginal_method,
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp
  )
  x_samp <- samp$x_samp

  # ---- type = "lv": Posterior draws of latent variable scores ----
  if (type == "lv") {

    if (nlevels > 1L) {
      # ---- Multilevel path: use lavaan internals ----
      lavsamplestats <- object$lavsamplestats

      # Pre-compute per-block ov names for imputation
      ov_names_block <- vector("list", lavmodel@nblocks)
      for (b in seq_len(lavmodel@nblocks)) {
        g_b <- ceiling(b / nlevels)
        ov_all <- lavdata@ov.names[[g_b]]
        ov_names_block[[b]] <- unique(
          pt$lhs[pt$block == b & pt$op == "~~" &
                   pt$lhs == pt$rhs & pt$lhs %in% ov_all]
        )
      }

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

        LAMBDA <- lavaan___lav_model_lambda(
          lavmodel = lavmodel_x, remove.dummy.lv = FALSE
        )
        VETA <- lavaan___lav_model_veta(lavmodel = lavmodel_x)
        EETA <- lavaan___lav_model_eeta(
          lavmodel = lavmodel_x, lavsamplestats = lavsamplestats
        )
        EY <- lavaan___lav_model_ey(
          lavmodel = lavmodel_x, lavsamplestats = lavsamplestats
        )
        Sigma.hat <- lavimplied$cov
        Sigma.inv <- lapply(Sigma.hat, MASS::ginv)

        out <- vector("list", nG)
        names(out) <- group_labels

        for (g in seq_len(nG)) {
          b <- (g - 1) * nlevels + level

          # Impute missing data from conditional posterior, then decompose
          y_g <- impute_ml_data(
            y[[g]], lavimplied, g, nlevels, ov_names_block, lavdata
          )

          Lp <- lavdata@Lp[[g]]
          group.idx <- (g - 1) * nlevels + seq_len(nlevels)
          implied.group <- lapply(lavimplied, function(x) x[group.idx])

          decomp <- lavaan___lav_mvnorm_cluster_implied22l(
            Lp = Lp, implied = implied.group
          )
          MB.j <- compute_ml_ranef(y_g, Lp, decomp)

          ov.idx <- Lp$ov.idx

          if (level == 1L) {
            data.obs.g <- y_g[, ov.idx[[1]], drop = FALSE] -
              MB.j[Lp$cluster.idx[[2]], , drop = FALSE]
          } else {
            # level == 2L
            Data.B <- matrix(0, nrow = nrow(MB.j), ncol = ncol(y_g))
            Data.B[, ov.idx[[1]]] <- MB.j
            between.idx <- Lp$between.idx[[2 * g]]
            if (length(between.idx) > 0L) {
              unique_rows <- match(
                seq_len(Lp$nclusters[[2]]), Lp$cluster.idx[[2]]
              )
              Data.B[, between.idx] <-
                y_g[unique_rows, between.idx, drop = FALSE]
            }
            data.obs.g <- Data.B[, ov.idx[[2]], drop = FALSE]
          }

          VETA.g <- VETA[[b]]
          EETA.g <- EETA[[b]]
          LAMBDA.g <- LAMBDA[[b]]
          EY.g <- EY[[b]]
          Sigma.inv.g <- Sigma.inv[[b]]

          FS.g <- compute_fs_ml(
            data.obs.g, VETA.g, LAMBDA.g, Sigma.hat[[b]],
            Sigma.inv.g, EETA.g, EY.g
          )

          # Replace dummy LV columns with data (level 1 only, per lavaan)
          if (level == 1L) {
            if (length(lavmodel_x@ov.y.dummy.lv.idx[[b]]) > 0L) {
              FS.g[, lavmodel_x@ov.y.dummy.lv.idx[[b]]] <-
                data.obs.g[, lavmodel_x@ov.y.dummy.ov.idx[[b]], drop = FALSE]
            }
            if (length(lavmodel_x@ov.x.dummy.lv.idx[[b]]) > 0L) {
              FS.g[, lavmodel_x@ov.x.dummy.lv.idx[[b]]] <-
                data.obs.g[, lavmodel_x@ov.x.dummy.ov.idx[[b]], drop = FALSE]
            }
          }

          colnames(FS.g) <- get_lv_names(lavmodel_x, b)
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

          if (is.null(alpha)) alpha <- 0

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

          mu_eta <- t(as.numeric(alpha) + PhiLtSinv %*% t(y[[g]]))

          V_eta <- Phi - PhiLtSinv %*% Lambda %*% Phi
          chol_V <- t(chol(V_eta))

          n_obs <- nrow(mu_eta)
          nlv <- ncol(mu_eta)
          Z <- matrix(rnorm(n_obs * nlv), nrow = nlv, ncol = n_obs)
          outg <- mu_eta + t(chol_V %*% Z)

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
      # ---- Multilevel yhat/ypred ----
      lavsamplestats <- object$lavsamplestats

      # Pre-compute per-block ov names for imputation
      ov_names_block <- vector("list", lavmodel@nblocks)
      for (b in seq_len(lavmodel@nblocks)) {
        g_b <- ceiling(b / nlevels)
        ov_all <- lavdata@ov.names[[g_b]]
        ov_names_block[[b]] <- unique(
          pt$lhs[pt$block == b & pt$op == "~~" &
                   pt$lhs == pt$rhs & pt$lhs %in% ov_all]
        )
      }

      sample_yhat_ml <- function(xx) {
        lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, xx)
        lavimplied <- lavaan::lav_model_implied(lavmodel_x)

        LAMBDA <- lavaan___lav_model_lambda(
          lavmodel = lavmodel_x, remove.dummy.lv = FALSE
        )
        VETA <- lavaan___lav_model_veta(lavmodel = lavmodel_x)
        EETA <- lavaan___lav_model_eeta(
          lavmodel = lavmodel_x, lavsamplestats = lavsamplestats
        )
        EY <- lavaan___lav_model_ey(
          lavmodel = lavmodel_x, lavsamplestats = lavsamplestats
        )
        Sigma.hat <- lavimplied$cov
        Sigma.inv <- lapply(Sigma.hat, MASS::ginv)

        nmat <- lavmodel_x@nmat

        out <- vector("list", nG)
        names(out) <- group_labels

        for (g in seq_len(nG)) {
          b_w <- (g - 1) * nlevels + 1  # within block
          b_b <- (g - 1) * nlevels + 2  # between block

          # Impute missing data from conditional posterior, then decompose
          y_g <- impute_ml_data(
            y[[g]], lavimplied, g, nlevels, ov_names_block, lavdata
          )

          Lp <- lavdata@Lp[[g]]
          group.idx <- (g - 1) * nlevels + seq_len(nlevels)
          implied.group <- lapply(lavimplied, function(x) x[group.idx])

          decomp <- lavaan___lav_mvnorm_cluster_implied22l(
            Lp = Lp, implied = implied.group
          )
          MB.j <- compute_ml_ranef(y_g, Lp, decomp)

          ov.idx <- Lp$ov.idx
          n_obs <- nrow(y_g)
          p <- ncol(y_g)

          # --- Within factor scores ---
          data.w <- y_g[, ov.idx[[1]], drop = FALSE] -
            MB.j[Lp$cluster.idx[[2]], , drop = FALSE]
          eta.w <- compute_fs_ml(
            data.w, VETA[[b_w]], LAMBDA[[b_w]], Sigma.hat[[b_w]],
            Sigma.inv[[b_w]], EETA[[b_w]], EY[[b_w]]
          )
          if (length(lavmodel_x@ov.x.dummy.lv.idx[[b_w]]) > 0L) {
            eta.w[, lavmodel_x@ov.x.dummy.lv.idx[[b_w]]] <-
              data.w[, lavmodel_x@ov.x.dummy.ov.idx[[b_w]], drop = FALSE]
          }
          if (length(lavmodel_x@ov.y.dummy.lv.idx[[b_w]]) > 0L) {
            eta.w[, lavmodel_x@ov.y.dummy.lv.idx[[b_w]]] <-
              data.w[, lavmodel_x@ov.y.dummy.ov.idx[[b_w]], drop = FALSE]
          }

          # --- Between factor scores ---
          Data.B <- matrix(0, nrow = nrow(MB.j), ncol = p)
          Data.B[, ov.idx[[1]]] <- MB.j
          between.idx <- Lp$between.idx[[2 * g]]
          if (length(between.idx) > 0L) {
            unique_rows <- match(
              seq_len(Lp$nclusters[[2]]), Lp$cluster.idx[[2]]
            )
            Data.B[, between.idx] <-
              y_g[unique_rows, between.idx, drop = FALSE]
          }
          data.b <- Data.B[, ov.idx[[2]], drop = FALSE]
          eta.b <- compute_fs_ml(
            data.b, VETA[[b_b]], LAMBDA[[b_b]], Sigma.hat[[b_b]],
            Sigma.inv[[b_b]], EETA[[b_b]], EY[[b_b]]
          )

          # --- Predicted values: within + between ---
          eta_w_c <- sweep(eta.w, 2, EETA[[b_w]])
          yhat_w <- t(EY[[b_w]] + LAMBDA[[b_w]] %*% t(eta_w_c))  # n_obs x p_w

          eta_b_c <- sweep(eta.b, 2, EETA[[b_b]])
          yhat_b <- t(EY[[b_b]] + LAMBDA[[b_b]] %*% t(eta_b_c))  # n_clust x p_b

          yhat <- matrix(0, n_obs, p)
          yhat[, ov.idx[[1]]] <- yhat_w
          yhat[, ov.idx[[2]]] <- yhat[, ov.idx[[2]]] +
            yhat_b[Lp$cluster.idx[[2]], , drop = FALSE]

          # --- Residual noise for ypred ---
          if (add_noise) {
            # Endogenous variable indices per block (exclude dummy LVs)
            dummy_w <- c(
              lavmodel_x@ov.x.dummy.ov.idx[[b_w]],
              lavmodel_x@ov.y.dummy.ov.idx[[b_w]]
            )
            endo_w <- setdiff(seq_len(length(ov.idx[[1]])), dummy_w)

            dummy_b <- c(
              lavmodel_x@ov.x.dummy.ov.idx[[b_b]],
              lavmodel_x@ov.y.dummy.ov.idx[[b_b]]
            )
            endo_b <- setdiff(seq_len(length(ov.idx[[2]])), dummy_b)

            # Within residual noise (endogenous only)
            if (length(endo_w) > 0L) {
              mm_w <- seq_len(nmat[b_w]) + cumsum(c(0, nmat))[b_w]
              Theta_w <- lavmodel_x@GLIST[mm_w][["theta"]]
              Theta_w_sub <- Theta_w[endo_w, endo_w, drop = FALSE]
              chol_Tw <- t(chol(Theta_w_sub))
              n_ew <- length(endo_w)
              E_w <- matrix(rnorm(n_obs * n_ew), nrow = n_ew, ncol = n_obs)
              yhat[, ov.idx[[1]][endo_w]] <-
                yhat[, ov.idx[[1]][endo_w]] + t(chol_Tw %*% E_w)
            }

            # Between residual noise (endogenous, cluster-level, expanded)
            if (length(endo_b) > 0L) {
              mm_b <- seq_len(nmat[b_b]) + cumsum(c(0, nmat))[b_b]
              Theta_b <- lavmodel_x@GLIST[mm_b][["theta"]]
              Theta_b_sub <- Theta_b[endo_b, endo_b, drop = FALSE]
              n_clust <- Lp$nclusters[[2]]
              chol_Tb <- t(chol(Theta_b_sub))
              n_eb <- length(endo_b)
              E_b <- matrix(rnorm(n_clust * n_eb), nrow = n_eb, ncol = n_clust)
              eps_b <- t(chol_Tb %*% E_b)
              yhat[, ov.idx[[2]][endo_b]] <-
                yhat[, ov.idx[[2]][endo_b]] +
                eps_b[Lp$cluster.idx[[2]], , drop = FALSE]
            }
          }

          cn <- colnames(y[[g]])
          if (is.null(cn)) cn <- lavdata@ov.names[[g]]
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

          if (is.null(alpha)) alpha <- rep(0, ncol(Lambda))
          if (is.null(nu)) nu <- rep(0, nrow(Lambda))

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

          # Posterior draw of eta | y, theta
          mu_eta <- t(as.numeric(alpha) + PhiLtSinv %*% t(y[[g]]))
          V_eta <- Phi - PhiLtSinv %*% Lambda %*% Phi
          chol_V <- t(chol(V_eta))
          n_obs <- nrow(mu_eta)
          nlv <- ncol(mu_eta)
          Z <- matrix(rnorm(n_obs * nlv), nrow = nlv, ncol = n_obs)
          eta_draw <- mu_eta + t(chol_V %*% Z)

          # yhat = nu + Lambda %*% (I-B)^{-1} %*% eta
          if (is.null(B)) {
            yhat <- sweep(tcrossprod(eta_draw, Lambda), 2, as.numeric(nu), "+")
          } else {
            yhat <- sweep(
              tcrossprod(eta_draw %*% t(IminB_inv), Lambda), 2,
              as.numeric(nu), "+"
            )
          }

          # Add residual noise for ypred
          if (add_noise) {
            p <- ncol(yhat)
            chol_Theta <- t(chol(Theta))
            E <- matrix(rnorm(n_obs * p), nrow = p, ncol = n_obs)
            yhat <- yhat + t(chol_Theta %*% E)
          }

          out[[g]] <- yhat
        }

        if (nG == 1L) {
          cn <- colnames(y[[1L]])
          if (is.null(cn)) cn <- lavdata@ov.names[[1L]]
          colnames(out[[1L]]) <- cn
          out <- out[[1L]]
        } else {
          out <- do.call(
            rbind,
            Map(function(g, df) data.frame(group = g, df), names(out), out)
          )
          cn <- colnames(y[[1L]])
          if (is.null(cn)) cn <- lavdata@ov.names[[1L]]
          colnames(out)[-1] <- cn
        }
        rownames(out) <- NULL
        out
      }

      msg <- if (add_noise) "Sampling predicted values" else "Sampling fitted values"
      out <- vector("list", nsamp)
      cli_progress_bar(msg, total = nsamp, clear = FALSE)
      for (i in seq_len(nsamp)) {
        out[[i]] <- sample_yhat(x_samp[i, ])
        cli_progress_update()
      }
      cli_progress_done()
    }

  # ---- type = "ymis": Missing data imputation ----
  } else if (type == "ymis") { # nocov start
    # For each posterior draw of model parameters, compute the model-implied
    # covariance Sigma(theta) and mean mu(theta), then draw missing values
    # from their conditional distribution given observed values:
    #   y_mis | y_obs, theta ~ N(mu_cond, Sigma_cond)
    nlevels <- lavdata@nlevels

    # Pre-compute per-block ov names for naming model-implied matrices
    ov_names_block <- NULL
    if (nlevels > 1L) {
      nblocks <- lavmodel@nblocks
      ov_names_block <- vector("list", nblocks)
      for (b in seq_len(nblocks)) {
        g_b <- ceiling(b / nlevels)
        ov_all <- lavdata@ov.names[[g_b]]
        ov_names_block[[b]] <- unique(
          pt$lhs[pt$block == b & pt$op == "~~" &
                   pt$lhs == pt$rhs & pt$lhs %in% ov_all]
        )
      }
    }

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

        if (nlevels == 1L) {
          Sigma_y <- lavimplied$cov[[g]]
          mu_y <- if (!is.null(lavimplied$mean)) {
            as.numeric(lavimplied$mean[[g]])
          } else {
            rep(0, p)
          }
        } else {
          # Multilevel: marginal covariance = within + between
          block_w <- (g - 1) * nlevels + 1
          block_b <- (g - 1) * nlevels + 2

          Sigma_w <- lavimplied$cov[[block_w]]
          Sigma_b <- lavimplied$cov[[block_b]]

          # Name the model-implied matrices using per-block ov names
          if (!is.null(ov_names_block)) {
            nw <- ov_names_block[[block_w]]
            if (length(nw) == nrow(Sigma_w)) {
              rownames(Sigma_w) <- colnames(Sigma_w) <- nw
            }
            nb <- ov_names_block[[block_b]]
            if (length(nb) == nrow(Sigma_b)) {
              rownames(Sigma_b) <- colnames(Sigma_b) <- nb
            }
          }

          var_names <- colnames(yg)
          if (is.null(var_names)) var_names <- lavdata@ov.names[[g]]
          Sigma_y <- matrix(0, p, p, dimnames = list(var_names, var_names))

          vn_w <- rownames(Sigma_w)
          w_in_data <- match(vn_w, var_names)
          w_keep <- !is.na(w_in_data)
          if (any(w_keep)) {
            idx <- w_in_data[w_keep]
            Sigma_y[idx, idx] <- Sigma_y[idx, idx] +
              Sigma_w[w_keep, w_keep, drop = FALSE]
          }

          vn_b <- rownames(Sigma_b)
          b_in_data <- match(vn_b, var_names)
          b_keep <- !is.na(b_in_data)
          if (any(b_keep)) {
            idx <- b_in_data[b_keep]
            Sigma_y[idx, idx] <- Sigma_y[idx, idx] +
              Sigma_b[b_keep, b_keep, drop = FALSE]
          }

          mu_y <- rep(0, p)
          names(mu_y) <- var_names
          if (!is.null(lavimplied$mean)) {
            mu_b <- as.numeric(lavimplied$mean[[block_b]])
            names(mu_b) <- vn_b
            b_match <- intersect(var_names, vn_b)
            mu_y[match(b_match, var_names)] <- mu_b[b_match]
          }
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
          chol_cond <- t(chol(Sigma_cond))

          n_mis <- length(mis_idx)
          n_cases <- length(case_rows)

          y_obs_centred <- yg[case_rows, obs_idx, drop = FALSE] -
            matrix(mu_y[obs_idx], nrow = n_cases,
                   ncol = length(obs_idx), byrow = TRUE)
          mu_cond <- matrix(mu_y[mis_idx], nrow = n_cases,
                            ncol = n_mis, byrow = TRUE) +
            y_obs_centred %*% t(A)

          Z <- matrix(rnorm(n_cases * n_mis),
                      nrow = n_mis, ncol = n_cases)
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
          if (is.null(var_nms)) var_nms <- lavdata@ov.names[[g]]
          na_pos <- which(is.na(yg_orig), arr.ind = TRUE)
          na_pos <- na_pos[order(na_pos[, 1L], na_pos[, 2L]), , drop = FALSE]
          if (nrow(na_pos) == 0L) {
            imp_list[[g]] <- numeric(0L)
          } else {
            vals <- out[[g]][na_pos]
            nms  <- paste0(var_nms[na_pos[, 2L]], "[",
                           na_pos[, 1L] + row_offset, "]")
            names(vals) <- nms
            imp_list[[g]] <- vals
          }
          row_offset <- row_offset + nrow(yg_orig)
        }
        return(do.call(c, imp_list))
      }

      if (nG == 1L) {
        cn <- colnames(y[[1L]])
        if (is.null(cn)) cn <- lavdata@ov.names[[1L]]
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
  structure(out, class = "predict.inlavaan_internal")
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
  if (!is.null(type)) cat(sprintf(" (type = \"%s\")", type))
  cat("\n")
  cat("Number of samples:", length(x), "\n")
  cat("First sample:\n")

  first <- x[[1L]]

  # Named vector (ymis_only output)
  if (is.numeric(first) && !is.matrix(first) && !is.data.frame(first)) {
    n_total <- length(first)
    if (n_total > n) {
      print(first[seq_len(n)], digits = nd)
      cat(col_grey(paste0("# ", symbol$info, " ", n_total - n, " more value",
                          if (n_total - n == 1L) "" else "s", "\n")))
      cat(col_grey(paste0("# ", symbol$info,
                          " Use `summary()` to see summary statistics\n")))
    } else {
      print(first, digits = nd)
    }
    return(invisible(x))
  }

  # Matrix / data frame output
  nr <- nrow(first)
  if (!is.null(nr) && nr > n) {
    print(as.data.frame(first[seq_len(n), , drop = FALSE]), digits = nd)
    cat(col_grey(paste0("# ", symbol$info, " ", nr - n, " more row",
                        if (nr - n == 1L) "" else "s", "\n")))
    cat(col_grey(paste0("# ", symbol$info,
                        " Use `summary()` to see summary statistics\n")))
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
    cat(col_grey(paste0("# ", symbol$info, " ", nr - n, " more row",
                        if (nr - n == 1L) "" else "s", "\n")))
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

#' @inheritParams inlavaan
#' @rdname INLAvaan-class
#' @param object An object of class [INLAvaan].
#' @param type Character string specifying the type of prediction:
#'   \describe{
#'     \item{`"lv"`}{(default) Posterior draws of latent variable scores
#'       \eqn{\eta | y, \theta}.}
#'     \item{`"yhat"`, `"ov"`}{Predicted means for observed variables
#'       \eqn{E(y | \eta, \theta) = \nu + \Lambda \eta}; no residual noise.}
#'     \item{`"ypred"`, `"ydist"`}{Predicted observed values including
#'       residual noise \eqn{y = \nu + \Lambda \eta + \varepsilon},
#'       \eqn{\varepsilon \sim N(0, \Theta)}.}
#'     \item{`"ymis"`, `"ovmis"`}{Imputed values for missing observations,
#'       drawn from the conditional distribution
#'       \eqn{y_{mis} | y_{obs}, \theta}.}
#'   }
#' @param newdata An optional data frame of new observations. If supplied,
#'   predictions are computed for `newdata` rather than the original training
#'   data. Not supported for `type = "ymis"`.
#' @param level Integer; for `type = "lv"` in multilevel models, specifies
#'   whether level 1 or level 2 latent variables are desired (default `1L`).
#' @param ymis_only Logical; only applies when `type = "ymis"`. When `TRUE`,
#'   returns only the imputed values as a named numeric vector per sample,
#'   with names of the form `"varname[rowindex]"` (matching the blavaan
#'   convention). When `FALSE` (default), returns the full data matrix with
#'   missing values filled in.
#' @export
setMethod("predict", "INLAvaan", function(
  object,
  type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"),
  newdata = NULL,
  level = 1L,
  nsamp = 1000,
  ymis_only = FALSE,
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
    ...
  )
})
