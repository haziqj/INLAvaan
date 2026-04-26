native_lisrel_backend_supported <- function(fit0) {
  lavmodel <- fit0@Model
  lavsamplestats <- fit0@SampleStats
  lavdata <- fit0@Data

  native_lisrel_backend_supported_components(lavmodel, lavsamplestats, lavdata)
}

native_lisrel_backend_supported_partable <- function(
  lavpartable,
  lavsamplestats,
  lavdata,
  lavoptions
) {
  if (is.null(lavpartable) || is.null(lavsamplestats) ||
      is.null(lavdata) || is.null(lavoptions)) {
    return(FALSE)
  }
  if (!identical(lavoptions$representation, "LISREL")) {
    return(FALSE)
  }
  if (length(lavdata@ordered) > 0L) {
    return(FALSE)
  }
  if (!identical(lavoptions$estimator, "ML")) {
    return(FALSE)
  }

  pt <- as.data.frame(lavpartable)
  bad_ops <- setdiff(unique(pt$op), c("=~", "~~", "~", "~1", ":=", "==", "<", ">"))
  length(bad_ops) == 0L
}

native_lisrel_backend_supported_components <- function(
  lavmodel,
  lavsamplestats,
  lavdata
) {
  if (is.null(lavmodel) || is.null(lavsamplestats) || is.null(lavdata)) {
    return(FALSE)
  }

  if (!identical(lavmodel@representation, "LISREL")) {
    return(FALSE)
  }
  if (length(lavdata@ordered) > 0L) {
    return(FALSE)
  }
  if (lavmodel@estimator != "ML") {
    return(FALSE)
  }

  mats <- names(lavmodel@GLIST)
  allowed_mats <- c("lambda", "theta", "psi", "beta", "nu", "alpha")
  if (!all(mats %in% allowed_mats)) {
    return(FALSE)
  }

  nlevels <- lavdata@nlevels %||% 1L
  if (nlevels == 1L) {
    return(all(c("lambda", "theta", "psi") %in% mats))
  }

  if (nlevels != 2L || isTRUE(lavsamplestats@missing.flag) ||
      isTRUE(lavmodel@conditional.x)) {
    return(FALSE)
  }

  if (length(lavmodel@nmat) != lavmodel@nblocks ||
      lavmodel@nblocks != lavmodel@ngroups * nlevels) {
    return(FALSE)
  }

  block_ok <- vapply(seq_len(lavmodel@nblocks), function(b) {
    mm <- seq_len(lavmodel@nmat[b]) + cumsum(c(0, lavmodel@nmat))[b]
    mats_block <- mats[mm]
    all(c("lambda", "theta", "psi") %in% mats_block)
  }, logical(1))
  all(block_ok)
}

native_lisrel_backend_extract <- function(fit0) {
  native_lisrel_backend_extract_components(
    lavmodel = fit0@Model,
    lavsamplestats = fit0@SampleStats,
    lavdata = fit0@Data
  )
}

native_lisrel_backend_extract_partable <- function(
  lavpartable,
  lavsamplestats,
  lavdata,
  lavoptions
) {
  make_missing_patterns <- function(g) {
    if (!isTRUE(lavsamplestats@missing.flag)) {
      return(NULL)
    }
    lapply(lavsamplestats@missing[[g]], function(rec) {
      p_obs <- sum(rec$var.idx)
      sy_dim <- dim(rec$SY)
      sy_nrow <- if (is.null(sy_dim)) 1L else sy_dim[[1L]]
      sy_ncol <- if (is.null(sy_dim)) 1L else sy_dim[[2L]]
      list(
        SY_values = as.numeric(rec$SY),
        SY_nrow = sy_nrow,
        SY_ncol = sy_ncol,
        MY = as.numeric(rec$MY),
        var_idx = as.logical(rec$var.idx),
        p_obs = as.integer(p_obs),
        freq = as.integer(rec$freq)
      )
    })
  }

  pt <- as.data.frame(lavpartable)
  if (!"group" %in% names(pt)) {
    pt$group <- rep(1L, nrow(pt))
  }
  if (!"start" %in% names(pt)) {
    if ("est" %in% names(pt)) {
      pt$start <- pt$est
    } else if ("ustart" %in% names(pt)) {
      pt$start <- pt$ustart
    } else {
      pt$start <- numeric(nrow(pt))
    }
  }

  ngroups <- max(pt$group, 1L)
  ov_names_all <- lavaan___lav_partable_vnames(lavpartable, type = "ov")
  lv_names_all <- lavaan___lav_partable_vnames(lavpartable, type = "lv")
  if (!is.list(ov_names_all)) {
    ov_names_all <- rep(list(ov_names_all), ngroups)
  }
  if (!is.list(lv_names_all)) {
    lv_names_all <- rep(list(lv_names_all), ngroups)
  }

  matrix_pos <- function(i, j, nrow) {
    as.integer((j - 1L) * nrow + i)
  }

  empty_spec <- function(nrow, ncol) {
    list(
      nrow = as.integer(nrow),
      ncol = as.integer(ncol),
      values = numeric(nrow * ncol),
      free_x_idx = integer(0),
      free_pos_idx = integer(0)
    )
  }

  x_free <- pt$start[pt$free > 0L & !duplicated(pt$free)]

  groups <- lapply(seq_len(ngroups), function(g) {
    ov_names <- ov_names_all[[g]]
    lv_names <- lv_names_all[[g]]
    ptg <- pt[pt$group == g, , drop = FALSE]
    ov_idx <- stats::setNames(seq_along(ov_names), ov_names)
    lv_idx <- stats::setNames(seq_along(lv_names), lv_names)

    build_spec <- function(rows, nrow, ncol, row_map, col_map,
                           row_field = "lhs", col_field = "rhs",
                           symmetric = FALSE) {
      spec <- empty_spec(nrow, ncol)
      if (nrow(rows) == 0L) {
        return(spec)
      }

      i <- unname(row_map[rows[[row_field]]])
      j <- unname(col_map[rows[[col_field]]])
      keep <- !is.na(i) & !is.na(j)
      if (!any(keep)) {
        return(spec)
      }

      rows <- rows[keep, , drop = FALSE]
      i <- i[keep]
      j <- j[keep]
      pos <- matrix_pos(i, j, nrow)
      spec$values[pos] <- rows$start

      free_keep <- rows$free > 0L
      spec$free_x_idx <- as.integer(rows$free[free_keep])
      spec$free_pos_idx <- pos[free_keep]

      if (symmetric) {
        offdiag <- i != j
        if (any(offdiag)) {
          pos2 <- matrix_pos(j[offdiag], i[offdiag], nrow)
          spec$values[pos2] <- rows$start[offdiag]
          free_offdiag <- free_keep[offdiag]
          if (any(free_offdiag)) {
            spec$free_x_idx <- c(spec$free_x_idx, as.integer(rows$free[offdiag][free_offdiag]))
            spec$free_pos_idx <- c(spec$free_pos_idx, pos2[free_offdiag])
          }
        }
      }

      spec
    }

    lambda_rows <- ptg[ptg$op == "=~", c("lhs", "rhs", "free", "start"), drop = FALSE]
    theta_rows <- ptg[
      ptg$op == "~~" & ptg$lhs %in% ov_names & ptg$rhs %in% ov_names,
      c("lhs", "rhs", "free", "start"),
      drop = FALSE
    ]
    psi_rows <- ptg[
      ptg$op == "~~" & ptg$lhs %in% lv_names & ptg$rhs %in% lv_names,
      c("lhs", "rhs", "free", "start"),
      drop = FALSE
    ]
    beta_rows <- ptg[ptg$op == "~", c("lhs", "rhs", "free", "start"), drop = FALSE]
    nu_rows <- ptg[ptg$op == "~1" & ptg$lhs %in% ov_names, c("lhs", "rhs", "free", "start"), drop = FALSE]
    alpha_rows <- ptg[ptg$op == "~1" & ptg$lhs %in% lv_names, c("lhs", "rhs", "free", "start"), drop = FALSE]

    lambda <- build_spec(
      lambda_rows, length(ov_names), length(lv_names),
      ov_idx, lv_idx, row_field = "rhs", col_field = "lhs", symmetric = FALSE
    )
    theta <- build_spec(theta_rows, length(ov_names), length(ov_names), ov_idx, ov_idx, symmetric = TRUE)
    psi <- build_spec(psi_rows, length(lv_names), length(lv_names), lv_idx, lv_idx, symmetric = TRUE)
    beta <- build_spec(beta_rows, length(lv_names), length(lv_names), lv_idx, lv_idx, symmetric = FALSE)
    # nu/alpha rows have rhs = "" (empty); R's named-vector lookup `[""]`
    # returns NA, so we replace the rhs with a sentinel before looking it up.
    # Guard against empty data frames (no mean structure).
    col1 <- stats::setNames(1L, ".intercept.")
    nu_rows_col <- nu_rows
    alpha_rows_col <- alpha_rows
    if (nrow(nu_rows_col) > 0L)    nu_rows_col$rhs    <- ".intercept."
    if (nrow(alpha_rows_col) > 0L) alpha_rows_col$rhs <- ".intercept."
    nu <- build_spec(nu_rows_col, length(ov_names), 1L, ov_idx, col1, row_field = "lhs", col_field = "rhs", symmetric = FALSE)
    alpha <- build_spec(alpha_rows_col, length(lv_names), 1L, lv_idx, col1, row_field = "lhs", col_field = "rhs", symmetric = FALSE)

    has_beta <- length(beta$free_pos_idx) > 0L || any(beta$values != 0)
    has_mean <- any(ptg$op == "~1")
    mats <- list(lambda = lambda, theta = theta, psi = psi)
    if (has_beta) {
      mats$beta <- beta
    }
    if (has_mean) {
      mats$nu <- nu
      mats$alpha <- alpha
    }

    list(
      has_beta = has_beta,
      has_mean = has_mean,
      has_missing = isTRUE(lavsamplestats@missing.flag),
      matrices = mats,
      sample_cov = if (isTRUE(lavsamplestats@missing.flag)) NULL else lavsamplestats@cov[[g]],
      sample_mean = if (isTRUE(lavsamplestats@missing.flag)) {
        NULL
      } else if (has_mean) {
        matrix(lavsamplestats@mean[[g]], ncol = 1L)
      } else {
        NULL
      },
      nobs = if (isTRUE(lavsamplestats@missing.flag)) NULL else lavsamplestats@nobs[[g]],
      missing = make_missing_patterns(g)
    )
  })

  list(
    type = "lisrel_ml_single_group",
    groups = groups,
    x_free_start = x_free
  )
}

native_lisrel_backend_extract_components <- function(
  lavmodel,
  lavsamplestats,
  lavdata
) {
  make_missing_patterns <- function(g) {
    if (!isTRUE(lavsamplestats@missing.flag)) {
      return(NULL)
    }
    lapply(lavsamplestats@missing[[g]], function(rec) {
      p_obs <- sum(rec$var.idx)
      sy_dim <- dim(rec$SY)
      sy_nrow <- if (is.null(sy_dim)) 1L else sy_dim[[1L]]
      sy_ncol <- if (is.null(sy_dim)) 1L else sy_dim[[2L]]
      list(
        SY_values = as.numeric(rec$SY),
        SY_nrow = sy_nrow,
        SY_ncol = sy_ncol,
        MY = as.numeric(rec$MY),
        var_idx = as.logical(rec$var.idx),
        p_obs = as.integer(p_obs),
        freq = as.integer(rec$freq)
      )
    })
  }

  glist <- lavmodel@GLIST
  x_free <- lavaan___lav_model_get_parameters(lavmodel, type = "free")
  nlevels <- lavdata@nlevels %||% 1L
  block_offsets <- cumsum(c(0L, lavmodel@nmat))

  make_matrix_spec <- function(idx) {
    mat <- glist[[idx]]
    list(
      nrow = nrow(mat),
      ncol = ncol(mat),
      values = as.numeric(mat),
      free_x_idx = lavmodel@x.free.idx[[idx]],
      free_pos_idx = lavmodel@m.free.idx[[idx]]
    )
  }

  block_mm <- function(b) {
    seq_len(lavmodel@nmat[b]) + block_offsets[b]
  }

  build_block <- function(idx) {
    block_names <- names(glist)[idx]
    mats <- list(
      lambda = make_matrix_spec(idx[block_names == "lambda"]),
      theta = make_matrix_spec(idx[block_names == "theta"]),
      psi = make_matrix_spec(idx[block_names == "psi"])
    )
    has_beta <- any(block_names == "beta")
    has_mean <- lavmodel@meanstructure &&
      all(c("nu", "alpha") %in% block_names)
    if (has_beta) {
      mats$beta <- make_matrix_spec(idx[block_names == "beta"])
    }
    if (has_mean) {
      mats$nu <- make_matrix_spec(idx[block_names == "nu"])
      mats$alpha <- make_matrix_spec(idx[block_names == "alpha"])
    }

    list(
      has_beta = has_beta,
      has_mean = has_mean,
      has_missing = FALSE,
      matrices = mats,
      sample_cov = NULL,
      sample_mean = NULL,
      nobs = NULL,
      missing = NULL
    )
  }

  make_cluster_stats <- function(g) {
    ylp2 <- lavsamplestats@YLp[[g]][[2L]]
    lp <- lavdata@Lp[[g]]

    list(
      Sigma_W = ylp2$Sigma.W,
      cov_d = lapply(ylp2$cov.d, function(x) unname(as.matrix(x))),
      mean_d = lapply(ylp2$mean.d, function(x) as.numeric(x)),
      lp = list(
        ov_idx = lapply(lp$ov.idx, as.integer),
        both_idx = as.integer(lp$both.idx[[2L]]),
        within_idx = as.integer(lp$within.idx[[2L]]),
        between_idx = as.integer(lp$between.idx[[2L]]),
        nobs = as.integer(lp$nclusters[[1L]]),
        nclusters = as.integer(lp$nclusters[[2L]]),
        cluster_size = as.integer(lp$cluster.size[[2L]]),
        cluster_sizes = as.integer(lp$cluster.sizes[[2L]]),
        ncluster_sizes = as.integer(lp$ncluster.sizes[[2L]]),
        cluster_size_ns = as.integer(lp$cluster.size.ns[[2L]]),
        has_x = length(unlist(lp$ov.x.idx)) > 0L
      ),
      loglik_x = ylp2$loglik.x
    )
  }

  if (nlevels == 2L) {
    groups <- lapply(seq_len(lavmodel@ngroups), function(g) {
      within_block <- (g - 1L) * nlevels + 1L
      between_block <- within_block + 1L
      list(
        within = build_block(block_mm(within_block)),
        between = build_block(block_mm(between_block)),
        cluster_stats = make_cluster_stats(g)
      )
    })

    return(list(
      type = "lisrel_ml_twolevel",
      groups = groups,
      x_free_start = x_free
    ))
  }

  list(
    type = "lisrel_ml_single_group",
    groups = lapply(seq_len(lavmodel@ngroups), function(g) {
      block <- build_block(block_mm(g))
      list(
        has_beta = block$has_beta,
        has_mean = block$has_mean,
        has_missing = isTRUE(lavsamplestats@missing.flag),
        matrices = block$matrices,
        sample_cov = if (isTRUE(lavsamplestats@missing.flag)) NULL else lavsamplestats@cov[[g]],
        sample_mean = if (isTRUE(lavsamplestats@missing.flag)) {
          NULL
        } else if (block$has_mean) {
          matrix(lavsamplestats@mean[[g]], ncol = 1L)
        } else {
          NULL
        },
        nobs = if (isTRUE(lavsamplestats@missing.flag)) NULL else lavsamplestats@nobs[[g]],
        missing = make_missing_patterns(g)
      )
    }),
    x_free_start = x_free
  )
}

native_lisrel_input_has_ordered <- function(data, ordered = NULL) {
  if (!is.null(ordered) && length(ordered) > 0L) {
    return(TRUE)
  }
  if (!is.data.frame(data)) {
    return(FALSE)
  }
  any(vapply(data, is.ordered, logical(1)))
}

native_lisrel_ngroups <- function(data, group = NULL) {
  if (is.null(group) || !nzchar(group)) {
    return(1L)
  }
  if (!is.data.frame(data) || !group %in% names(data)) {
    return(NA_integer_)
  }

  vals <- data[[group]]
  vals <- vals[!is.na(vals)]
  length(unique(vals))
}

native_lisrel_build_options <- function(lavargs, meanstructure = FALSE) {
  opt <- with_safe_detectCores(lavaan::lavOptions())
  opt_fields <- intersect(names(lavargs), names(opt))
  opt[opt_fields] <- lavargs[opt_fields]

  opt$representation <- opt$representation %||% "LISREL"
  if (identical(opt$representation, "default")) {
    opt$representation <- "LISREL"
  }
  opt$estimator <- opt$estimator %||% "ML"
  if (identical(opt$estimator, "default")) {
    opt$estimator <- "ML"
  }
  opt$missing <- opt$missing %||% "listwise"
  if (identical(opt$missing, "default")) {
    opt$missing <- "listwise"
  }
  opt$meanstructure <- isTRUE(meanstructure)
  opt$model.type <- lavargs$model.type %||% "sem"
  opt$test <- lavargs$test %||% "none"
  opt$do.fit <- FALSE
  opt$.categorical <- FALSE
  opt$.multilevel <- FALSE
  opt$.clustered <- FALSE
  opt$sample.cov.rescale <- identical(opt$estimator, "ML")
  opt$sample.cov.robust <- FALSE
  opt$samplestats <- TRUE
  opt$correlation <- FALSE
  opt$fixed.x <- isTRUE(opt$fixed.x)
  opt$conditional.x <- isTRUE(opt$conditional.x)
  opt$sampling.weights.normalization <-
    opt$sampling.weights.normalization %||% "total"

  opt
}

native_lisrel_theta_transforms <- function(pt, ptfreeidx) {
  mats <- pt$mat[ptfreeidx]
  transforms <- integer(length(ptfreeidx))
  transforms[grepl("theta_var|psi_var", mats)] <- 1L
  transforms[grepl("theta_cor|theta_cov|psi_cor|psi_cov", mats)] <- 2L
  gcp_blocks <- attr(pt, "gcp_blocks")
  if (length(gcp_blocks) > 0L) {
    gcp_free <- unique(unlist(lapply(gcp_blocks, function(b) pt$free[b$pt_cor_idx])))
    transforms[pt$free[ptfreeidx] %in% gcp_free] <- 3L
  }
  transforms
}

native_lisrel_apply_model_defaults <- function(lavargs) {
  model_type <- lavargs$model.type %||% "sem"
  is_std_lv <- isTRUE(lavargs$std.lv)

  if (model_type %in% c("cfa", "sem")) {
    defaults <- list(
      int.ov.free = TRUE,
      int.lv.free = FALSE,
      auto.fix.first = !is_std_lv,
      auto.fix.single = TRUE,
      auto.var = TRUE,
      auto.cov.lv.x = TRUE,
      auto.efa = TRUE,
      auto.th = TRUE,
      auto.delta = TRUE,
      auto.cov.y = TRUE
    )
  } else if (model_type == "growth") {
    defaults <- list(
      meanstructure = TRUE,
      int.ov.free = FALSE,
      int.lv.free = TRUE,
      auto.fix.first = !is_std_lv,
      auto.fix.single = TRUE,
      auto.var = TRUE,
      auto.cov.lv.x = TRUE,
      auto.efa = TRUE,
      auto.th = TRUE,
      auto.delta = TRUE,
      auto.cov.y = TRUE
    )
  } else {
    defaults <- list()
  }

  for (nm in names(defaults)) {
    if (!nm %in% names(lavargs)) {
      lavargs[[nm]] <- defaults[[nm]]
    }
  }

  lavargs
}

native_lisrel_parse_supported_model <- function(lavargs) {
  model_type <- lavargs$model.type %||% "sem"
  if (!model_type %in% c("cfa", "sem")) {
    return(NULL)
  }
  if (!is.null(lavargs$group.partial) && length(lavargs$group.partial) > 0L) {
    return(NULL)
  }
  if (!is.character(lavargs$model) || length(lavargs$model) != 1L) {
    return(NULL)
  }

  parsed <- tryCatch(
    lavaan::lavParseModelString(lavargs$model, parser = "old"),
    error = function(e) NULL
  )
  if (is.null(parsed)) return(NULL)
  if (length(attr(parsed, "constraints")) > 0L) {
    return(NULL)
  }
  if (any(parsed$op %in% c("==", "<", ">", ":=", "~*~", "|", "<~"))) {
    return(NULL)
  }
  if (any(nzchar(parsed$fixed)) || any(nzchar(parsed$start)) ||
      any(nzchar(parsed$lower)) || any(nzchar(parsed$upper)) ||
      any(nzchar(parsed$label)) || any(nzchar(parsed$prior)) ||
      any(nzchar(parsed$efa)) || any(nzchar(parsed$rv))) {
    return(NULL)
  }
  if (!is.null(lavargs$group.equal) && length(lavargs$group.equal) > 0L) {
    allowed <- c("loadings", "intercepts")
    if (!all(lavargs$group.equal %in% allowed)) {
      return(NULL)
    }
    if (is.null(lavargs$group) || !nzchar(lavargs$group)) {
      return(NULL)
    }
  }

  parsed
}

native_lisrel_build_partable <- function(lavargs, parsed = NULL) {
  if (is.null(parsed)) {
    parsed <- lavaan::lavParseModelString(lavargs$model)
  }
  parsed <- as.data.frame(parsed, stringsAsFactors = FALSE)
  ngroups <- native_lisrel_ngroups(lavargs$data, lavargs$group %||% NULL)
  std_lv <- isTRUE(lavargs$std.lv)
  int_ov_free <- isTRUE(lavargs$int.ov.free)
  int_lv_free <- isTRUE(lavargs$int.lv.free)
  auto_var <- isTRUE(lavargs$auto.var)
  auto_cov_lv_x <- isTRUE(lavargs$auto.cov.lv.x)
  meanstructure <- isTRUE(lavargs$meanstructure) || any(parsed$op == "~1") || ngroups > 1L
  group_equal <- lavargs$group.equal %||% character(0)
  equal_loadings <- "loadings" %in% group_equal
  equal_intercepts <- "intercepts" %in% group_equal

  lv_names <- unique(parsed$lhs[parsed$op == "=~"])
  ov_names <- unique(parsed$rhs[parsed$op == "=~"])
  reg_lhs <- unique(parsed$lhs[parsed$op == "~"])
  reg_rhs <- unique(parsed$rhs[parsed$op == "~"])
  extra_latent <- setdiff(unique(c(reg_lhs, reg_rhs)), ov_names)
  lv_names <- unique(c(lv_names, extra_latent))
  exo_latent <- setdiff(lv_names, reg_lhs)

  existing_ov_var <- unique(parsed$lhs[parsed$op == "~~" & parsed$lhs == parsed$rhs & parsed$lhs %in% ov_names])
  existing_lv_var <- unique(parsed$lhs[parsed$op == "~~" & parsed$lhs == parsed$rhs & parsed$lhs %in% lv_names])
  existing_means <- unique(parsed$lhs[parsed$op == "~1"])
  existing_cov_pairs <- character(0)
  idx_cov <- which(parsed$op == "~~" & parsed$lhs != parsed$rhs)
  if (length(idx_cov) > 0L) {
    lhs_cov <- parsed$lhs[idx_cov]
    rhs_cov <- parsed$rhs[idx_cov]
    existing_cov_pairs <- paste(pmin(lhs_cov, rhs_cov), pmax(lhs_cov, rhs_cov), sep = "\r")
  }

  chunks <- new.env(parent = emptyenv())
  chunks$lhs <- vector("list", 0L)
  chunks$op <- vector("list", 0L)
  chunks$rhs <- vector("list", 0L)
  chunks$user <- vector("list", 0L)
  chunks$block <- vector("list", 0L)
  chunks$group <- vector("list", 0L)
  chunks$free <- vector("list", 0L)
  chunks$ustart <- vector("list", 0L)
  chunks$exo <- vector("list", 0L)
  chunks$label <- vector("list", 0L)
  chunks$plabel <- vector("list", 0L)
  chunks$start <- vector("list", 0L)
  chunks$est <- vector("list", 0L)
  state <- new.env(parent = emptyenv())
  state$nchunks <- 0L
  append_chunk <- function(lhs, op, rhs = "", user = 0L, block = 1L, group = 1L,
                           free = 0L, ustart = NA_real_, exo = 0L, label = "",
                           plabel = "", start = 0, est = NA_real_) {
    n <- length(lhs)
    if (n == 0L) {
      return(invisible(NULL))
    }
    state$nchunks <- state$nchunks + 1L
    idx <- state$nchunks
    chunks$lhs[[idx]] <- rep(as.character(lhs), length.out = n)
    chunks$op[[idx]] <- rep(as.character(op), length.out = n)
    chunks$rhs[[idx]] <- rep(as.character(rhs), length.out = n)
    chunks$user[[idx]] <- rep(as.integer(user), length.out = n)
    chunks$block[[idx]] <- rep(as.integer(block), length.out = n)
    chunks$group[[idx]] <- rep(as.integer(group), length.out = n)
    chunks$free[[idx]] <- rep(as.integer(free), length.out = n)
    chunks$ustart[[idx]] <- rep(as.numeric(ustart), length.out = n)
    chunks$exo[[idx]] <- rep(as.integer(exo), length.out = n)
    chunks$label[[idx]] <- rep(as.character(label), length.out = n)
    chunks$plabel[[idx]] <- rep(as.character(plabel), length.out = n)
    chunks$start[[idx]] <- rep(as.numeric(start), length.out = n)
    chunks$est[[idx]] <- rep(as.numeric(est), length.out = n)
    invisible(NULL)
  }

  for (g in seq_len(ngroups)) {
    block <- g
    append_chunk(
      lhs = parsed$lhs,
      op = parsed$op,
      rhs = parsed$rhs,
      user = 1L,
      block = block,
      group = g,
      exo = 0L
    )

    if (auto_var) {
      add_ov_var <- setdiff(ov_names, existing_ov_var)
      add_lv_var <- setdiff(lv_names, existing_lv_var)
      append_chunk(add_ov_var, "~~", add_ov_var, block = block, group = g)
      append_chunk(add_lv_var, "~~", add_lv_var, block = block, group = g)
    }

    if (auto_cov_lv_x && length(exo_latent) > 1L) {
      combos <- utils::combn(exo_latent, 2)
      if (ncol(combos) > 0L) {
        pair_keys <- paste(pmin(combos[1L, ], combos[2L, ]), pmax(combos[1L, ], combos[2L, ]), sep = "\r")
        keep <- !(pair_keys %in% existing_cov_pairs)
        if (any(keep)) {
          append_chunk(combos[1L, keep], "~~", combos[2L, keep], block = block, group = g)
        }
      }
    }

    if (meanstructure) {
      append_chunk(setdiff(ov_names, existing_means), "~1", "", block = block, group = g)
      append_chunk(setdiff(lv_names, existing_means), "~1", "", block = block, group = g)
    }
  }

  lhs_all <- unlist(chunks$lhs, use.names = FALSE)
  nrows <- length(lhs_all)
  pt <- data.frame(
    id = rep(NA_integer_, nrows),
    lhs = lhs_all,
    op = unlist(chunks$op, use.names = FALSE),
    rhs = unlist(chunks$rhs, use.names = FALSE),
    user = unlist(chunks$user, use.names = FALSE),
    block = unlist(chunks$block, use.names = FALSE),
    group = unlist(chunks$group, use.names = FALSE),
    free = unlist(chunks$free, use.names = FALSE),
    ustart = unlist(chunks$ustart, use.names = FALSE),
    exo = unlist(chunks$exo, use.names = FALSE),
    label = unlist(chunks$label, use.names = FALSE),
    plabel = unlist(chunks$plabel, use.names = FALSE),
    start = unlist(chunks$start, use.names = FALSE),
    est = unlist(chunks$est, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  pt$id <- seq_len(nrow(pt))
  pt$plabel <- sprintf(".p%d.", pt$id)

  free_counter <- 0L
  eq_free_map <- list()
  eq_label_map <- list()
  # Per-group loading counters: reset for each group so first loading per factor
  # is correctly identified as the marker indicator in every group.
  loading_seen_by_group <- lapply(seq_len(ngroups), function(g)
    setNames(integer(length(lv_names)), lv_names)
  )
  cur_grp <- NA_integer_
  loading_seen <- setNames(integer(length(lv_names)), lv_names)
  for (i in seq_len(nrow(pt))) {
    lhs <- pt$lhs[i]
    op <- pt$op[i]
    rhs <- pt$rhs[i]
    grp <- pt$group[i]
    # Switch the per-group counter when the group changes
    if (!identical(grp, cur_grp)) {
      cur_grp <- grp
      loading_seen <- loading_seen_by_group[[grp]]
    }
    start <- 0
    is_free <- TRUE
    eq_key <- NULL

    if (op == "=~") {
      loading_seen[[lhs]] <- loading_seen[[lhs]] + 1L
      loading_seen_by_group[[grp]][[lhs]] <- loading_seen[[lhs]]
      if (!std_lv && loading_seen[[lhs]] == 1L) {
        is_free <- FALSE
        start <- 1
      } else {
        start <- if (std_lv) 0.7 else 1
      }
      if (equal_loadings && is_free) {
        eq_key <- paste("loadings", lhs, rhs, sep = "::")
      }
    } else if (op == "~~" && lhs == rhs && lhs %in% ov_names) {
      start <- 1
    } else if (op == "~~" && lhs == rhs && lhs %in% lv_names) {
      if (std_lv && equal_loadings && ngroups > 1L && grp > 1L) {
        start <- 0.05
      } else if (std_lv) {
        is_free <- FALSE
        start <- 1
      } else {
        start <- 0.05
      }
    } else if (op == "~~") {
      start <- 0
    } else if (op == "~") {
      start <- 0
    } else if (op == "~1" && lhs %in% ov_names) {
      if (int_ov_free) {
        start <- 0
      } else {
        is_free <- FALSE
        start <- 0
      }
      if (equal_intercepts && is_free) {
        eq_key <- paste("intercepts", lhs, sep = "::")
      }
    } else if (op == "~1" && lhs %in% lv_names) {
      if (equal_intercepts && equal_loadings && ngroups > 1L && grp > 1L) {
        start <- 0
      } else if (int_lv_free) {
        start <- 0
      } else {
        is_free <- FALSE
        start <- 0
      }
    } else {
      is_free <- FALSE
      start <- 0
    }

    pt$start[i] <- start
    pt$ustart[i] <- if (is_free) NA_real_ else start
    if (is_free) {
      if (!is.null(eq_key) && !is.null(eq_free_map[[eq_key]])) {
        pt$free[i] <- eq_free_map[[eq_key]]
        pt$label[i] <- eq_label_map[[eq_key]]
      } else {
        free_counter <- free_counter + 1L
        pt$free[i] <- free_counter
        if (!is.null(eq_key)) {
          eq_free_map[[eq_key]] <- free_counter
          eq_label_map[[eq_key]] <- pt$plabel[i]
          pt$label[i] <- pt$plabel[i]
        }
      }
    } else {
      pt$free[i] <- 0L
    }
  }

  pt
}

native_lisrel_start_values <- function(lavpartable, lavsamplestats, lavoptions) {
  pt <- as.data.frame(lavpartable)
  if (!"group" %in% names(pt)) {
    pt$group <- rep(1L, nrow(pt))
  }

  ngroups <- max(pt$group, 1L)
  ov_names_all <- lavaan___lav_partable_vnames(lavpartable, type = "ov")
  lv_names_all <- lavaan___lav_partable_vnames(lavpartable, type = "lv")
  if (!is.list(ov_names_all)) {
    ov_names_all <- rep(list(ov_names_all), ngroups)
  }
  if (!is.list(lv_names_all)) {
    lv_names_all <- rep(list(lv_names_all), ngroups)
  }

  start <- numeric(nrow(pt))
  load_idx <- which(pt$op == "=~")
  start[load_idx] <- if (isTRUE(lavoptions$std.lv)) 0.7 else 1.0

  ov_var_idx <- which(pt$op == "~~" & pt$lhs == pt$rhs)
  if (length(ov_var_idx) > 0L) {
    lv_names_flat <- unique(unlist(lv_names_all))
    lv_var_idx <- ov_var_idx[pt$lhs[ov_var_idx] %in% lv_names_flat]
    ov_only_var_idx <- setdiff(ov_var_idx, lv_var_idx)
    start[lv_var_idx] <- if (isTRUE(lavoptions$std.lv)) 1.0 else 0.05

    for (g in seq_len(ngroups)) {
      ov_names <- ov_names_all[[g]]
      g_cov <- lavsamplestats@cov[[g]]
      idx_g <- ov_only_var_idx[pt$group[ov_only_var_idx] == g]
      if (length(idx_g) == 0L) {
        next
      }
      samp_idx <- match(pt$lhs[idx_g], ov_names)
      start[idx_g] <- pmax(0.5 * diag(g_cov)[samp_idx], 1e-3)
    }
  }

  lv_cov_idx <- which(
    pt$op == "~~" &
      pt$lhs != pt$rhs &
      pt$lhs %in% unique(unlist(lv_names_all)) &
      pt$rhs %in% unique(unlist(lv_names_all))
  )
  start[lv_cov_idx] <- 0

  ov_cov_idx <- which(
    pt$op == "~~" &
      pt$lhs != pt$rhs &
      pt$lhs %in% unique(unlist(ov_names_all)) &
      pt$rhs %in% unique(unlist(ov_names_all))
  )
  start[ov_cov_idx] <- 0

  beta_idx <- which(pt$op == "~")
  start[beta_idx] <- 0

  nu_idx <- which(pt$op == "~1" & pt$lhs %in% unique(unlist(ov_names_all)))
  if (length(nu_idx) > 0L) {
    for (g in seq_len(ngroups)) {
      idx_g <- nu_idx[pt$group[nu_idx] == g]
      if (length(idx_g) == 0L) {
        next
      }
      ov_names <- ov_names_all[[g]]
      samp_idx <- match(pt$lhs[idx_g], ov_names)
      start[idx_g] <- lavsamplestats@mean[[g]][samp_idx]
    }
  }

  alpha_idx <- which(pt$op == "~1" & pt$lhs %in% unique(unlist(lv_names_all)))
  start[alpha_idx] <- 0

  user_idx <- which(!is.na(pt$ustart))
  if (length(user_idx) > 0L) {
    start[user_idx] <- pt$ustart[user_idx]
  }

  fixed_idx <- which(pt$free == 0L)
  if (length(fixed_idx) > 0L) {
    start[fixed_idx] <- ifelse(is.na(pt$ustart[fixed_idx]), start[fixed_idx], pt$ustart[fixed_idx])
  }

  start
}

native_lisrel_build_lavdata <- function(data, lavpartable, lavoptions, group = NULL) {
  ov_names_all <- lavaan___lav_partable_vnames(lavpartable, type = "ov")
  if (!is.list(ov_names_all)) {
    ov_names_all <- list(ov_names_all)
  }

  if (!is.null(group) && nzchar(group)) {
    group_vals <- data[[group]]
    group_levels <- unique(as.character(stats::na.omit(group_vals)))
    split_idx <- lapply(group_levels, function(gl) which(as.character(group_vals) == gl))
    names(split_idx) <- group_levels
    ngroups <- length(group_levels)
    group_name <- group
    group_label <- group_levels
  } else {
    split_idx <- list(seq_len(nrow(data)))
    ngroups <- 1L
    group_name <- character(0)
    group_label <- character(0)
  }

  ov_names <- lapply(seq_len(ngroups), function(g) ov_names_all[[min(g, length(ov_names_all))]])
  case_idx <- split_idx
  nobs <- lapply(split_idx, length)
  norig <- nobs
  ov_idx <- match(ov_names[[1L]], names(data))
  ov_info <- list(
    name = ov_names[[1L]],
    idx = as.integer(ov_idx),
    nobs = rep(sum(unlist(nobs)), length(ov_names[[1L]])),
    type = rep("numeric", length(ov_names[[1L]])),
    exo = integer(length(ov_names[[1L]])),
    user = integer(length(ov_names[[1L]])),
    mean = colMeans(data[, ov_names[[1L]], drop = FALSE]),
    var = apply(data[, ov_names[[1L]], drop = FALSE], 2, stats::var),
    nlev = integer(length(ov_names[[1L]])),
    lnam = rep("", length(ov_names[[1L]]))
  )

  out <- new("lavData")
  out@data.type <- "full"
  out@group <- group_name
  out@ngroups <- as.integer(ngroups)
  out@group.label <- group_label
  out@block.label <- character(0)
  out@cluster <- character(0)
  out@nlevels <- 1L
  out@level.label <- character(0)
  out@std.ov <- isTRUE(lavoptions$std.ov)
  out@nobs <- nobs
  out@norig <- norig
  out@ov.names <- ov_names
  out@ov.names.x <- rep(list(character(0)), ngroups)
  out@ov.names.l <- list()
  out@ordered <- character(0)
  out@weights <- rep(list(NULL), ngroups)
  out@sampling.weights <- character(0)
  out@ov <- ov_info
  out@case.idx <- case_idx
  out@missing <- "listwise"
  out@Mp <- rep(list(NULL), ngroups)
  out@Rp <- rep(list(NULL), ngroups)
  out@Lp <- rep(list(NULL), ngroups)
  out@eXo <- list()
  out@X <- lapply(split_idx, function(idx) data[idx, ov_names[[1L]], drop = FALSE])
  out
}

native_lisrel_build_lavsamplestats <- function(data, lavdata, lavoptions) {
  ngroups <- lavdata@ngroups
  cov_list <- vector("list", ngroups)
  mean_list <- vector("list", ngroups)
  nobs_list <- lavdata@nobs

  for (g in seq_len(ngroups)) {
    idx <- lavdata@case.idx[[g]]
    ov_names <- lavdata@ov.names[[g]]
    dat_g <- data[idx, ov_names, drop = FALSE]
    cov_list[[g]] <- stats::cov(dat_g)
    mean_list[[g]] <- colMeans(dat_g)
  }

  out <- new("lavSampleStats")
  out@cov <- cov_list
  out@mean <- mean_list
  out@nobs <- nobs_list
  out@ntotal <- sum(unlist(nobs_list))
  out@ngroups <- as.integer(ngroups)
  out@x.idx <- rep(list(integer(0)), ngroups)
  out@mean.x <- rep(list(NULL), ngroups)
  out@cov.x <- rep(list(NULL), ngroups)
  out@missing.flag <- FALSE
  out@missing <- list()
  out@missing.h1 <- list()
  out@YLp <- list()
  out@zero.cell.tables <- list()
  out
}

native_lisrel_light_init <- function(lavargs) {
  lavargs <- native_lisrel_apply_model_defaults(lavargs)

  if (!is.data.frame(lavargs$data)) {
    return(NULL)
  }
  if (anyNA(lavargs$data)) {
    return(NULL)
  }
  if (!is.null(lavargs$cluster)) {
    return(NULL)
  }
  if (!is.null(lavargs$sampling.weights)) {
    return(NULL)
  }
  if (isTRUE(lavargs$fixed.x) || isTRUE(lavargs$conditional.x)) {
    return(NULL)
  }
  if (native_lisrel_input_has_ordered(lavargs$data, lavargs$ordered)) {
    return(NULL)
  }
  if (!is.null(lavargs$estimator) && !identical(lavargs$estimator, "ML")) {
    return(NULL)
  }
  if (!is.null(lavargs$representation) &&
      !identical(lavargs$representation, "LISREL")) {
    return(NULL)
  }
  if (!is.null(lavargs$missing) &&
      !(lavargs$missing %in% c("default", "listwise"))) {
    return(NULL)
  }

  ngroups <- native_lisrel_ngroups(lavargs$data, lavargs$group %||% NULL)
  if (is.na(ngroups) || ngroups < 1L) {
    return(NULL)
  }

  parsed_native <- native_lisrel_parse_supported_model(lavargs)
  if (!is.null(parsed_native)) {
    lavpartable <- native_lisrel_build_partable(lavargs, parsed = parsed_native)
  } else {
    pt_args <- lavargs[intersect(names(lavargs), names(formals(lavaan::lavaanify)))]
    pt_args$ngroups <- ngroups
    lavpartable <- do.call(lavaan::lavaanify, pt_args)
  }

  opt <- native_lisrel_build_options(
    lavargs,
    meanstructure = any(lavpartable$op == "~1")
  )
  ov_names <- lavaan___lav_partable_vnames(lavpartable, type = "ov")
  if (!is.list(ov_names)) {
    ov_names <- rep(list(ov_names), ngroups)
  }

  lavdata <- native_lisrel_build_lavdata(
    data = lavargs$data,
    lavpartable = lavpartable,
    lavoptions = opt,
    group = lavargs$group %||% NULL
  )
  lavsamplestats <- native_lisrel_build_lavsamplestats(
    data = lavargs$data,
    lavdata = lavdata,
    lavoptions = opt
  )
  lavpartable$start <- native_lisrel_start_values(lavpartable, lavsamplestats, opt)
  native_backend <- NULL
  if (native_lisrel_backend_supported_partable(
    lavpartable = lavpartable,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavoptions = opt
  )) {
    native_backend <- native_lisrel_backend_extract_partable(
      lavpartable = lavpartable,
      lavsamplestats = lavsamplestats,
      lavdata = lavdata,
      lavoptions = opt
    )
  }
  lavmodel <- lavaan___lav_model(
    lavpartable = lavpartable,
    lavoptions = opt
  )

  if (!native_lisrel_backend_supported_components(lavmodel, lavsamplestats, lavdata)) {
    return(NULL)
  }

  list(
    lavmodel = lavmodel,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavoptions = opt,
    lavpartable = lavpartable,
    lavcache = NULL,
    native_backend = native_backend %||% native_lisrel_backend_extract_components(
      lavmodel = lavmodel,
      lavsamplestats = lavsamplestats,
      lavdata = lavdata
    )
  )
}

model_loglik_dispatch <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache,
  native_backend = NULL
) {
  if (!is.null(native_backend)) {
    return(native_model_loglik(x, native_backend))
  }

  inlav_model_loglik(
    x,
    lavmodel,
    lavsamplestats,
    lavdata,
    lavoptions,
    lavcache
  )
}

native_model_loglik <- function(x, backend) {
  if (is.null(backend)) {
    cli_abort("Native backend must be provided.")
  }

  if (backend$type == "lisrel_ml_single_group") {
    return(cpp_lisrel_loglik(backend, x))
  }

  if (backend$type == "lisrel_ml_twolevel") {
    return(cpp_lisrel_loglik(backend, x))
  }

  cli_abort("Unsupported native backend type.")
}

native_model_implied_cov_list <- function(x, backend) {
  if (is.null(backend)) {
    cli_abort("Native backend must be provided.")
  }

  if (backend$type == "lisrel_ml_single_group") {
    return(lapply(backend$groups, function(group) {
      cpp_lisrel_implied_cov(
        list(
          type = backend$type,
          groups = list(group),
          x_free_start = backend$x_free_start
        ),
        x
      )
    }))
  }

  if (backend$type == "lisrel_ml_twolevel") {
    implied <- cpp_lisrel_twolevel_implied_blocks(backend, x)
    out <- vector("list", length(backend$groups) * 2L)
    for (g in seq_along(backend$groups)) {
      idx <- (g - 1L) * 2L
      out[[idx + 1L]] <- implied[[g]]$within$cov
      out[[idx + 2L]] <- implied[[g]]$between$cov
    }
    return(out)
  }

  cli_abort("Unsupported native backend type.")
}

native_model_implied_mean_list <- function(x, backend) {
  if (is.null(backend)) {
    cli_abort("Native backend must be provided.")
  }

  if (backend$type == "lisrel_ml_single_group") {
    return(lapply(backend$groups, function(group) {
      if (!isTRUE(group$has_mean)) {
        return(NULL)
      }
      cpp_lisrel_implied_mean(
        list(
          type = backend$type,
          groups = list(group),
          x_free_start = backend$x_free_start
        ),
        x
      )
    }))
  }

  if (backend$type == "lisrel_ml_twolevel") {
    implied <- cpp_lisrel_twolevel_implied_blocks(backend, x)
    out <- vector("list", length(backend$groups) * 2L)
    for (g in seq_along(backend$groups)) {
      idx <- (g - 1L) * 2L
      out[[idx + 1L]] <- implied[[g]]$within$mean
      out[[idx + 2L]] <- implied[[g]]$between$mean
    }
    return(out)
  }

  cli_abort("Unsupported native backend type.")
}

native_model_grad <- function(x, backend) {
  if (is.null(backend)) {
    cli_abort("Native backend must be provided.")
  }

  if (backend$type == "lisrel_ml_single_group") {
    grad <- cpp_lisrel_grad(backend, x)
    if (anyNA(grad) || any(!is.finite(grad))) {
      grad <- cpp_fast_grad(function(z) cpp_lisrel_loglik(backend, z), x)
    }
    return(grad)
  }

  if (backend$type == "lisrel_ml_twolevel") {
    grad <- cpp_lisrel_grad(backend, x)
    if (anyNA(grad) || any(!is.finite(grad))) {
      grad <- cpp_fast_grad(function(z) cpp_lisrel_loglik(backend, z), x)
    }
    return(grad)
  }

  cli_abort("Unsupported native backend type.")
}
