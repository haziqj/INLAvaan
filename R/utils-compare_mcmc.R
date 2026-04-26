# Silence R CMD check notes for ggplot2 pipelines
utils::globalVariables(c(
  "name",
  "method",
  "value",
  "x",
  "y",
  "JS_percent",
  "metric",
  "lhs",
  "op",
  "rhs"
))

compare_mcmc_inlavaan_standardized_draws <- function(fit, nsamp) {
  if (!inherits(fit, "INLAvaan")) {
    cli_abort(
      "Standardized non-Gaussian MCMC comparison currently requires {.cls INLAvaan} fits."
    )
  }

  int <- fit@external$inlavaan_internal
  pt <- int$partable
  samp <- sample_params(
    theta_star = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method = int$marginal_method,
    approx_data = int$approx_data,
    pt = pt,
    lavmodel = int$lavmodel,
    nsamp = nsamp,
    R_star = int$R_star,
    integration_data = if (!is.null(int$inla_integration)) int$inla_integration else NULL,
    native_theta_transforms = int$native_theta_transforms %||% NULL,
    cov_var_idx1 = int$cov_var_idx1 %||% NULL,
    cov_var_idx2 = int$cov_var_idx2 %||% NULL
  )
  x_samp <- samp$x_samp

  out <- vector("list", nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    xi <- x_samp[i, ]
    lavmodel_i <- lavaan::lav_model_set_parameters(fit@Model, xi)

    esti <- pt$est
    esti[pt$free > 0] <- xi[pt$free[pt$free > 0]]
    if (any(pt$op == ":=")) {
      pt_def_rows <- which(pt$op == ":=")
      def_names <- pt$names[pt_def_rows]
      esti[pt_def_rows] <- int$summary[def_names, "Mean"]
    }

    out[[i]] <- lavaan::standardizedSolution(
      object = fit,
      est = esti,
      GLIST = lavmodel_i@GLIST,
      remove.eq = TRUE,
      remove.ineq = TRUE,
      remove.def = FALSE
    )$est.std
  }

  draws <- do.call(rbind, out)
  proto <- lavaan::standardizedSolution(
    object = fit,
    remove.eq = TRUE,
    remove.ineq = TRUE,
    remove.def = FALSE
  )
  colnames(draws) <- with(proto, paste0(lhs, op, rhs))
  draws
}

compare_mcmc_density_data_from_draws <- function(draws) {
  draws_df <- as.data.frame(draws)
  lapply(draws_df, function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2L) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }
    if (stats::sd(x) == 0) {
      eps <- max(abs(x[1L]), 1) * 1e-6
      x <- c(x, x[1L] - eps, x[1L] + eps)
    }
    dens <- stats::density(x)
    data.frame(x = dens$x, y = dens$y)
  })
}

#' Compare INLAvaan Marginals Against an MCMC Reference
#'
#' Aligns posterior marginals from one or more `INLAvaan` fits with posterior
#' draws from a `blavaan` fit and returns comparison plots and discrepancy
#' metrics.
#'
#' @param fit_blavaan A fitted `blavaan` object used as the MCMC reference.
#' @param ... Named `INLAvaan` or `inlavaan_internal` fits to compare.
#' @param params Optional character vector of parameter names to compare.
#' @param show_error Logical; include discrepancy plots.
#' @param truth Optional named vector of true parameter values.
#' @param use_ggplot Logical; use `ggplot2` when available.
#' @param nrow,ncol Optional layout controls for the base-graphics fallback.
#' @param parameterization Compare either the raw posterior parameters, the
#'   standardized posterior parameters, or choose automatically. `"auto"`
#'   switches to standardized comparison when the `blavaan` reference has
#'   ordered variables, which is the safer comparison for non-Gaussian
#'   measurement models.
#'
#' @return A list with comparison plots, aligned densities, and discrepancy
#'   summaries.
#' @export
# nocov start
compare_mcmc <- function(
  fit_blavaan,
  ...,
  params = NULL,
  show_error = TRUE,
  truth = NULL,
  use_ggplot = TRUE,
  parameterization = c("auto", "raw", "standardized"),
  nrow = NULL,
  ncol = NULL
) {
  parameterization <- match.arg(parameterization)

  # Accept blavaan, inlavaan_mcmc, or a plain matrix of draws as the reference.
  is_blavaan   <- inherits(fit_blavaan, "blavaan")
  is_inla_mcmc <- inherits(fit_blavaan, "inlavaan_mcmc")
  is_matrix    <- is.matrix(fit_blavaan) || is.data.frame(fit_blavaan)

  if (!is_blavaan && !is_inla_mcmc && !is_matrix) {
    cli_abort(
      paste0(
        "`fit_blavaan` must be a {.cls blavaan} object, an {.cls inlavaan_mcmc}",
        " object (from {.fn mcmc_from_inlavaan}), or a plain draws matrix."
      )
    )
  }

  # For blavaan, auto-detect standardized; for INLA/matrix, always use raw.
  use_standardized <- if (is_blavaan) {
    identical(parameterization, "standardized") ||
      (identical(parameterization, "auto") &&
         length(if (is.null(fit_blavaan@Data@ordered)) character(0)
                else fit_blavaan@Data@ordered) > 0L)
  } else {
    identical(parameterization, "standardized")
  }

  # Extract draws matrix -------------------------------------------------------
  draws <- if (is_blavaan) {
    if (!requireNamespace("blavaan", quietly = TRUE)) {
      cli_abort("blavaan is required. Please install it.")
    }
    if (use_standardized) {
      blavaan::standardizedPosterior(fit_blavaan)
    } else {
      do.call("rbind", blavaan::blavInspect(fit_blavaan, "mcmc"))
    }
  } else if (is_inla_mcmc) {
    fit_blavaan$draws
  } else {
    as.matrix(fit_blavaan)
  }
  # ---------------------------------------------------------------------------

  parnames <- unique(colnames(draws))
  draws_df <- as.data.frame(draws)
  plot_df_blav <- data.frame(
    name = rep(colnames(draws_df), each = nrow(draws_df)),
    value = unlist(draws_df, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  plot_df_blav$name <- factor(plot_df_blav$name, levels = parnames)

  # INLAvaan Densities
  fit_inlavaan_list <- list(...)
  fit_inlavaan_list <- lapply(fit_inlavaan_list, function(fit) {
    if (use_standardized) {
      return(compare_mcmc_density_data_from_draws(
        compare_mcmc_inlavaan_standardized_draws(fit, nsamp = nrow(draws_df))
      ))
    } else if (inherits(fit, "INLAvaan")) {
      return(fit@external$inlavaan_internal$pdf_data)
    } else if (inherits(fit, "inlavaan_internal")) {
      return(fit$pdf_data)
    } else {
      stop(
        "Unsupported object type. Provide INLAvaan or inlavaan_internal objects."
      )
    }
  })
  fit_inlavaan_list <- lapply(fit_inlavaan_list, function(pdfs) {
    keep <- vapply(pdfs, nrow, integer(1)) > 1L
    pdfs[keep]
  })
  inlav_names <- names(fit_inlavaan_list)
  if (length(fit_inlavaan_list) == 0L) {
    cli_abort("Provide at least one INLAvaan fit to compare.")
  }

  common_names <- Reduce(
    intersect,
    c(list(parnames), lapply(fit_inlavaan_list, names))
  )
  if (length(common_names) == 0L) {
    cli_abort("No common parameter names were found between blavaan and the INLAvaan fits.")
  }
  dropped_names <- setdiff(parnames, common_names)
  if (length(dropped_names) > 0L && is.null(params)) {
    cli::cli_alert_info(
      "Comparing {length(common_names)} common parameter{?s}; dropped {length(dropped_names)} unmatched parameter{?s}."
    )
  }
  parnames <- common_names

  # Subset parameters if requested
  if (!is.null(params)) {
    bad <- setdiff(params, common_names)
    if (length(bad) > 0) {
      cli_abort(c(
        "Unknown parameter(s): {paste(bad, collapse = ', ')}",
        i = "Available common parameters: {paste(common_names, collapse = ', ')}"
      ))
    }
    parnames <- params
    plot_df_blav <- plot_df_blav[
      plot_df_blav$name %in% parnames,
      ,
      drop = FALSE
    ]
    plot_df_blav$name <- factor(
      as.character(plot_df_blav$name),
      levels = parnames
    )
    fit_inlavaan_list <- lapply(fit_inlavaan_list, function(pdfs) {
      pdfs[parnames]
    })
    if (!is.null(truth)) truth <- truth[parnames]
  }

  mycols <- c("#00A6AA", "#F18F00", "#adbf04", "#9C6FAE")
  mycols <- mycols[1:length(inlav_names)]
  names(mycols) <- inlav_names

  # Create plot
  plot_df_parts <- lapply(names(fit_inlavaan_list), function(meth) {
    pdf_list <- fit_inlavaan_list[[meth]]
    part <- do.call(
      rbind,
      Map(
        function(nm, df) {
          df$name <- nm
          df
        },
        names(pdf_list),
        pdf_list
      )
    )
    part$method <- meth
    part
  })
  plot_df <- do.call(rbind, plot_df_parts)
  rownames(plot_df) <- NULL
  plot_df$name <- factor(plot_df$name, levels = parnames)
  plot_df$method <- factor(plot_df$method, levels = inlav_names)
  plot_df <- plot_df[!is.na(plot_df$name), ]

  # Helper function for Integration
  trapz <- function(x, y) {
    if (length(x) < 2) {
      return(0)
    }
    sum((head(y, -1) + tail(y, -1)) * diff(x) / 2, na.rm = TRUE)
  }

  # Align MCMC to Approximation Grid
  plot_df_aligned <- do.call(
    rbind,
    lapply(
      split(plot_df, list(plot_df$name, plot_df$method), drop = TRUE),
      function(data) {
        pnm <- as.character(data$name[1])
        mcmc_vals <- plot_df_blav$value[plot_df_blav$name == pnm]

        # Create KDE of MCMC (gold standard). We constrain the KDE to the range
        # of your approximation to prevent extrapolation
        d <- stats::density(
          mcmc_vals,
          from = min(data$x),
          to = max(data$x),
          n = length(data$x)
        )

        # Interpolate KDE onto exact x-points of your approximation. 'rule = 2'
        # clamps values at the ends if floating point errors occur
        data$f_mcmc <- stats::approx(
          x = d$x,
          y = d$y,
          xout = data$x,
          rule = 2
        )$y
        data
      }
    )
  )
  rownames(plot_df_aligned) <- NULL

  # Calculate Metrics
  metrics_df <- do.call(
    rbind,
    lapply(
      split(
        plot_df_aligned,
        list(plot_df_aligned$name, plot_df_aligned$method),
        drop = TRUE
      ),
      function(chunk) {
        x_val <- chunk$x
        pa <- chunk$y # Approx
        pm <- chunk$f_mcmc # MCMC
        eps <- 1e-12

        # Normalize
        Za <- trapz(x_val, pa)
        pa <- if (!is.na(Za) && Za > 0) pa / Za else pa
        Zm <- trapz(x_val, pm)
        pm <- if (!is.na(Zm) && Zm > 0) pm / Zm else pm

        # --- KL Divergences ---
        kl_fwd <- trapz(x_val, pm * (log(pmax(pm, eps)) - log(pmax(pa, eps))))
        kl_rev <- trapz(x_val, pa * (log(pmax(pa, eps)) - log(pmax(pm, eps))))

        # --- Interpretability Hacks ---
        # 1. Jensen-Shannon % (0 = Identical, 1 = Disjoint)
        m_mix <- 0.5 * (pm + pa)
        kl_pm <- trapz(x_val, pm * (log(pmax(pm, eps)) - log(pmax(m_mix, eps))))
        kl_qm <- trapz(x_val, pa * (log(pmax(pa, eps)) - log(pmax(m_mix, eps))))
        js_val <- 0.5 * kl_pm + 0.5 * kl_qm
        js_pct <- js_val / log(2)

        # 2. Gaussian Bias Equivalent
        bias_equiv <- sqrt(2 * abs(kl_fwd))

        # --- L1 (Total Variation) ---
        l1 <- trapz(x_val, abs(pm - pa))

        data.frame(
          name = as.character(chunk$name[1]),
          method = as.character(chunk$method[1]),
          L1 = l1,
          L1_percent = l1 / 2,
          js = js_val,
          JS_percent = js_pct,
          KL_fwd = kl_fwd,
          KL_rev = kl_rev,
          Bias_SDs = bias_equiv,
          stringsAsFactors = FALSE
        )
      }
    )
  )
  rownames(metrics_df) <- NULL
  metrics_df <- metrics_df[order(metrics_df$method, -metrics_df$Bias_SDs), ]

  # Add overall by averaging
  overall <- aggregate(
    cbind(L1, L1_percent, js, JS_percent, KL_fwd, KL_rev, Bias_SDs) ~
      method,
    data = metrics_df,
    FUN = mean
  )
  overall$name <- "Overall"
  metrics_df <- rbind(metrics_df, overall)
  metrics_df$name <- factor(metrics_df$name, levels = c("Overall", parnames))
  # Average within name+method (collapses duplicates for Overall)
  metrics_df <- aggregate(
    cbind(L1, L1_percent, js, JS_percent, KL_fwd, KL_rev, Bias_SDs) ~
      name + method,
    data = metrics_df,
    FUN = mean
  )
  metrics_df$name <- factor(metrics_df$name, levels = c("Overall", parnames))

  # --- Plotting ---
  use_ggplot <- isTRUE(use_ggplot) &&
    requireNamespace("ggplot2", quietly = TRUE)

  if (use_ggplot) {
    # --- ggplot2 version ---
    # Plot of L1 and JS errors
    err_long <- reshape(
      metrics_df[, c("name", "method", "L1_percent", "JS_percent")],
      direction = "long",
      varying = list(c("L1_percent", "JS_percent")),
      v.names = "value",
      timevar = "metric",
      times = c("L1 Error", "Jensen-Shannon Error"),
      idvar = c("name", "method")
    )
    rownames(err_long) <- NULL
    err_long$name <- factor(
      err_long$name,
      levels = rev(levels(factor(metrics_df$name)))
    )

    p_errors <-
      ggplot2::ggplot(err_long) +
      ggplot2::geom_bar(
        ggplot2::aes(x = name, y = value, fill = method),
        width = 0.7,
        stat = "identity",
        position = ggplot2::position_dodge()
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = function(x) {
        paste0(round(x * 100), "%")
      }) +
      ggplot2::scale_fill_manual(values = mycols) +
      ggplot2::facet_grid(. ~ metric, scales = "free_x") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 4, r = 8, b = 8, l = 8),
        legend.position = "top",
        legend.box.spacing = grid::unit(2, "pt"),
        legend.key.width = grid::unit(1, "cm")
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL)

    p_compare <-
      ggplot2::ggplot() +
      ggplot2::geom_density(
        data = plot_df_blav,
        ggplot2::aes(value, fill = "MCMC"),
        col = NA,
        alpha = 0.38
      ) +
      ggplot2::geom_line(
        data = plot_df,
        ggplot2::aes(x, y, group = method, col = method),
        linewidth = 0.75
      ) +
      ggplot2::facet_wrap(~name, scales = "free", nrow = nrow, ncol = ncol) +
      ggplot2::scale_colour_manual(values = mycols) +
      ggplot2::scale_fill_manual(values = c("MCMC" = "#131516")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 4, r = 8, b = 8, l = 8),
        legend.position = "top",
        legend.box.spacing = grid::unit(2, "pt"),
        legend.key.width = grid::unit(1, "cm")
      ) +
      ggplot2::labs(x = NULL, y = NULL, col = NULL, fill = NULL)

    if (!is.null(truth)) {
      if (length(truth) != length(parnames)) {
        cli_abort("Length of 'truth' must match number of parameters.")
      }

      truth_df <- data.frame(
        name = parnames,
        truth = as.numeric(truth)
      )

      p_compare <-
        p_compare +
        ggplot2::geom_vline(
          data = truth_df,
          ggplot2::aes(xintercept = truth),
          linetype = "dashed",
          color = "red",
          linewidth = 0.5
        )
    }

    if (isTRUE(show_error)) {
      # Pre-compute per-parameter effective right edge from MCMC KDE.
      # Use the rightmost x where density > 1% of peak to ignore sparse outliers.
      mcmc_q95 <- sapply(parnames, function(pnm) {
        vals <- plot_df_blav$value[plot_df_blav$name == pnm]
        d <- stats::density(vals)
        threshold <- 0.0025 * max(d$y)
        max(d$x[d$y >= threshold])
      })
      names(mcmc_q95) <- parnames

      # Summarise plot_df by name + method
      label_df <- do.call(
        rbind,
        lapply(
          split(plot_df, list(plot_df$name, plot_df$method), drop = TRUE),
          function(chunk) {
            pnm <- as.character(chunk$name[1])
            x_approx <- quantile(chunk$x, probs = 0.95)
            data.frame(
              name = chunk$name[1],
              method = chunk$method[1],
              x = max(x_approx, mcmc_q95[[pnm]]),
              panel_max_y = max(chunk$y),
              stringsAsFactors = FALSE
            )
          }
        )
      )
      rownames(label_df) <- NULL
      # Use a common reference height per panel (max across all methods),
      # then stagger each method's label vertically so they don't overlap.
      pmax_y <- tapply(label_df$panel_max_y, label_df$name, max)
      label_df$panel_max_y <- pmax_y[as.character(label_df$name)]
      label_df$method_rank <- match(label_df$method, inlav_names)
      label_df$y <- label_df$panel_max_y *
        (0.925 - 0.15 * (label_df$method_rank - 1))
      label_df$panel_max_y <- NULL
      label_df$method_rank <- NULL
      label_df <- merge(label_df, metrics_df, by = c("name", "method"))
      label_df$JS_percent <- {
        vals <- (1 - label_df$JS_percent) * 100
        ifelse(vals >= 99.95, "100%", sprintf("%.1f%%", vals))
      }

      p_compare <-
        p_compare +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x, y, label = JS_percent, col = method),
          size = 3,
          hjust = 0.7,
          show.legend = FALSE
        )
    }

    return(list(
      p_compare = p_compare,
      p_errors = p_errors,
      metrics_df = metrics_df
    ))
  }

  # --- base R fallback ---
  # Use a temporary null device to build plots, then capture with recordPlot()
  n_params <- length(parnames)
  n_cols <- ceiling(sqrt(n_params))
  n_rows <- ceiling(n_params / n_cols)

  truth_vec <- NULL
  if (!is.null(truth)) {
    if (length(truth) != length(parnames)) {
      cli_abort("Length of 'truth' must match number of parameters.")
    }
    truth_vec <- setNames(as.numeric(truth), parnames)
  }

  # -- p_compare: MCMC density + INLAvaan density lines per parameter --
  # Reserve top row for a horizontal legend
  layout_mat <- matrix(
    seq_len(n_rows * n_cols),
    nrow = n_rows,
    ncol = n_cols,
    byrow = TRUE
  )
  layout_mat <- rbind(rep(n_rows * n_cols + 1, n_cols), layout_mat)
  layout(layout_mat, heights = c(0.8, rep(4, n_rows)))
  op <- par(mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))

  for (pnm in parnames) {
    mcmc_vals <- plot_df_blav$value[plot_df_blav$name == pnm]
    d_mcmc <- stats::density(mcmc_vals)

    sub_df <- plot_df[plot_df$name == pnm, ]
    xlim <- range(c(d_mcmc$x, sub_df$x), na.rm = TRUE)
    ylim <- range(c(d_mcmc$y, sub_df$y), na.rm = TRUE)

    plot(
      d_mcmc,
      main = pnm,
      font.main = 1,
      xlab = "",
      ylab = "",
      bty = "n",
      col = NA,
      xlim = xlim,
      ylim = ylim,
      zero.line = FALSE
    )
    # grid(col = "lightgray", lty = "solid")
    polygon(d_mcmc$x, d_mcmc$y, col = adjustcolor("#131516", 0.25), border = NA)

    for (meth in names(mycols)) {
      mdf <- sub_df[sub_df$method == meth, ]
      if (nrow(mdf) > 0) {
        lines(mdf$x, mdf$y, col = mycols[meth], lwd = 1.5)
      }
    }

    if (!is.null(truth_vec)) {
      abline(v = truth_vec[pnm], lty = 2, col = "red", lwd = 0.8)
    }

    if (isTRUE(show_error)) {
      m_sub <- metrics_df[metrics_df$name == pnm, , drop = FALSE]
      for (i in seq_len(nrow(m_sub))) {
        val <- 100 * (1 - m_sub$JS_percent[i])
        lbl <- if (val >= 99.95) "100%" else sprintf("%.1f%%", val)
        mtext(
          lbl,
          side = 3,
          line = -1.2 - (i - 1) * 1,
          col = mycols[as.character(m_sub$method[i])],
          cex = 0.7,
          adj = 0.95
        )
      }
    }
  }

  # Fill any remaining empty panels
  remaining <- n_rows * n_cols - n_params
  for (i in seq_len(remaining)) {
    plot.new()
  }

  # Top legend panel
  par(mar = c(0, 0, 0, 0))
  plot.new()
  n_leg <- 1 + length(mycols)
  leg_labels <- c("MCMC", names(mycols))
  leg_pch <- c(22, rep(NA, length(mycols)))
  leg_pt_bg <- c(adjustcolor("#131516", 0.25), rep(NA, length(mycols)))
  leg_col <- c("gray40", mycols)
  leg_lwd <- c(NA, rep(1.5, length(mycols)))
  leg_lty <- c(NA, rep(1, length(mycols)))
  legend(
    "center",
    legend = leg_labels,
    pch = leg_pch,
    pt.bg = leg_pt_bg,
    pt.cex = 2,
    col = leg_col,
    lwd = leg_lwd,
    lty = leg_lty,
    horiz = TRUE,
    bty = "n",
    cex = 0.9,
    seg.len = 1.5
  )
  p_compare <- recordPlot()

  # -- p_errors: horizontal bar chart of L1 and JS errors --
  par(mfrow = c(1, 2), mar = c(4, 8, 3, 1), oma = c(0, 0, 2, 0))

  err_long <- metrics_df[, c("name", "method", "L1_percent", "JS_percent")]
  err_methods <- levels(factor(err_long$method))
  err_names <- rev(levels(factor(err_long$name)))
  n_methods <- length(err_methods)

  for (metric in c("L1_percent", "JS_percent")) {
    label <- if (metric == "L1_percent") "L1 Error" else "Jensen-Shannon Error"
    mat <- matrix(
      0,
      nrow = length(err_names),
      ncol = n_methods,
      dimnames = list(err_names, err_methods)
    )
    for (i in seq_len(nrow(err_long))) {
      r <- as.character(err_long$name[i])
      m <- as.character(err_long$method[i])
      if (r %in% err_names) mat[r, m] <- err_long[[metric]][i]
    }

    barplot(
      t(mat) * 100,
      beside = TRUE,
      horiz = TRUE,
      las = 1,
      col = mycols[err_methods],
      border = NA,
      main = label,
      xlab = "%",
      names.arg = err_names
    )
  }
  mtext("Error Metrics", outer = TRUE, cex = 1)
  legend(
    "topright",
    legend = err_methods,
    fill = mycols[err_methods],
    border = NA,
    bty = "n",
    cex = 0.8
  )
  p_errors <- recordPlot()

  par(op)

  list(
    p_compare = p_compare,
    p_errors = p_errors,
    metrics_df = metrics_df
  )
}

#' Run MCMC on the Laplace-Marginalised Posterior of an INLAvaan Fit
#'
#' Uses random-walk Metropolis–Hastings (with adaptive scaling during burn-in)
#' to draw exact samples from the same Laplace-marginalised posterior that the
#' ESLA/simplified-Laplace approximations target.  The resulting draws can be
#' passed as the first argument to [compare_mcmc()] as an MCMC reference.
#'
#' @param fit A fitted `INLAvaan` object.
#' @param n_samples Number of post-burn-in samples to return.
#' @param burnin Number of burn-in iterations (discarded).
#' @param thin Thinning interval (keep every `thin`-th draw).
#' @param seed Optional integer random seed.
#' @param verbose Print progress and diagnostics.
#'
#' @return An `inlavaan_mcmc` object with elements `$draws` (matrix of
#'   named parameter samples in the lavaan scale) and `$acceptance_rate`.
#' @export
# nocov start
mcmc_from_inlavaan <- function(
  fit,
  n_samples = 2000L,
  burnin    = 1000L,
  thin      = 1L,
  seed      = NULL,
  verbose   = TRUE
) {
  if (!is.null(seed)) set.seed(seed)

  int <- fit@external$inlavaan_internal
  if (is.null(int$joint_lp)) {
    cli_abort(c(
      "The stored fit does not contain a {.field joint_lp} function.",
      i = "Re-fit the model with the current version of INLAvaan."
    ))
  }

  joint_lp    <- int$joint_lp
  theta_star  <- int$theta_star
  Sigma_theta <- int$Sigma_theta
  pt          <- int$partable
  m           <- length(theta_star)

  # Recover free-parameter names from the partable (theta_star is stored as.numeric)
  parnames_mcmc <- pt$names[pt$free > 0L & !duplicated(pt$free)]

  # Multivariate normal proposal: 2.38^2/m * Sigma_theta (optimal RWMH scaling)
  scale <- 2.38 / sqrt(m)
  L     <- tryCatch(
    t(chol(Sigma_theta)),
    error = function(e) diag(sqrt(pmax(diag(Sigma_theta), 1e-12)))
  )

  theta_cur <- theta_star
  lp_cur    <- joint_lp(theta_cur)
  n_total   <- burnin + n_samples * thin

  out_theta <- matrix(NA_real_, n_samples, m)
  colnames(out_theta) <- parnames_mcmc

  n_accept <- 0L
  s_idx    <- 0L

  if (isTRUE(verbose)) {
    cli::cli_progress_bar(
      "MCMC sampling",
      total = n_total,
      format = "{cli::pb_bar} {cli::pb_percent} | iter {cli::pb_current}/{cli::pb_total} | acc {round(n_accept/max(iter,1)*100,1)}%"
    )
  }

  for (iter in seq_len(n_total)) {
    theta_prop <- theta_cur + scale * as.numeric(L %*% stats::rnorm(m))
    lp_prop    <- joint_lp(theta_prop)

    if (is.finite(lp_prop) && log(stats::runif(1L)) < lp_prop - lp_cur) {
      theta_cur <- theta_prop
      lp_cur    <- lp_prop
      n_accept  <- n_accept + 1L
    }

    # Adaptive scaling during burn-in (target 23.4% acceptance)
    if (iter <= burnin && iter %% 100L == 0L) {
      acc_rate <- n_accept / iter
      scale    <- scale * exp(0.5 * (acc_rate - 0.234))
      scale    <- max(1e-8, min(scale, 20.0))
    }

    if (iter > burnin && (iter - burnin) %% thin == 0L) {
      s_idx <- s_idx + 1L
      out_theta[s_idx, ] <- theta_cur
    }

    if (isTRUE(verbose)) cli::cli_progress_update()
  }
  if (isTRUE(verbose)) cli::cli_progress_done()

  acc_rate_final <- n_accept / n_total
  if (isTRUE(verbose)) {
    cli::cli_alert_info(
      "Acceptance rate: {round(acc_rate_final * 100, 1)}%  ({n_samples} samples after {burnin} burn-in)"
    )
  }

  # Transform theta → lavaan-scale x, keeping names
  x_list <- lapply(seq_len(n_samples), function(i) {
    tryCatch(
      as.numeric(pars_to_x(out_theta[i, ], pt = pt, compute_jac = FALSE)),
      error = function(e) rep(NA_real_, m)
    )
  })
  out_x <- do.call(rbind, x_list)
  colnames(out_x) <- parnames_mcmc

  ok <- apply(out_x, 1L, function(r) all(is.finite(r)))
  out_x     <- out_x[ok, , drop = FALSE]
  out_theta <- out_theta[ok, , drop = FALSE]

  structure(
    list(
      draws          = out_x,
      theta          = out_theta,
      acceptance_rate = acc_rate_final,
      n_samples      = nrow(out_x),
      burnin         = burnin,
      thin           = thin
    ),
    class = "inlavaan_mcmc"
  )
}
# nocov end


# Internal: extract a matrix of named draws from blavaan, inlavaan_mcmc, or a
# plain matrix; also returns the parameterization used.
.extract_mcmc_draws <- function(fit_ref, use_standardized, fit_inlavaan_ref) {
  if (inherits(fit_ref, "blavaan")) {
    if (!requireNamespace("blavaan", quietly = TRUE)) {
      cli_abort("blavaan is required. Please install it.")
    }
    draws <- if (use_standardized) {
      blavaan::standardizedPosterior(fit_ref)
    } else {
      do.call("rbind", blavaan::blavInspect(fit_ref, "mcmc"))
    }
    return(draws)
  }

  if (inherits(fit_ref, "inlavaan_mcmc")) {
    draws <- fit_ref$draws
    return(draws)
  }

  if (is.matrix(fit_ref) || is.data.frame(fit_ref)) {
    return(as.matrix(fit_ref))
  }

  cli_abort(
    paste0(
      "`fit_ref` must be a {.cls blavaan} object, an {.cls inlavaan_mcmc} object",
      " (from {.fn mcmc_from_inlavaan}), or a plain draws matrix."
    )
  )
}
# nocov end
