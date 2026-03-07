# Silence R CMD check notes for ggplot2 pipelines
utils::globalVariables(c(
  "name",
  "method",
  "value",
  "x",
  "y",
  "JS_percent",
  "metric"
))

# nocov start
compare_mcmc <- function(fit_blavaan, ..., show_error = TRUE, truth = NULL,
                         use_ggplot = TRUE) {
  parnames <- unique(names(coef(fit_blavaan)))

  if (requireNamespace("blavaan", quietly = TRUE) == FALSE) {
    cli_abort("blavaan is required for plotting. Please install it.")
  }

  # MCMC Histograms
  draws <- do.call("rbind", blavaan::blavInspect(fit_blavaan, "mcmc"))
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
    if (inherits(fit, "INLAvaan")) {
      return(fit@external$inlavaan_internal$pdf_data)
    } else if (inherits(fit, "inlavaan_internal")) {
      return(fit$pdf_data)
    } else {
      stop(
        "Unsupported object type. Provide INLAvaan or inlavaan_internal objects."
      )
    }
  })
  inlav_names <- names(fit_inlavaan_list)

  mycols <- c("#00A6AA", "#F18F00", "#adbf04", "#9C6FAE")
  mycols <- mycols[1:length(inlav_names)]
  names(mycols) <- inlav_names

  # Create plot
  plot_df_parts <- lapply(names(fit_inlavaan_list), function(meth) {
    pdf_list <- fit_inlavaan_list[[meth]]
    part <- do.call(rbind, Map(
      function(nm, df) { df$name <- nm; df },
      names(pdf_list), pdf_list
    ))
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
    sum((head(y, -1) + tail(y, 1)) * diff(x) / 2)
  }

  # Align MCMC to Approximation Grid
  plot_df_aligned <- do.call(rbind, lapply(
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
      data$f_mcmc <- stats::approx(x = d$x, y = d$y, xout = data$x, rule = 2)$y
      data
    }
  ))
  rownames(plot_df_aligned) <- NULL

  # Calculate Metrics
  metrics_df <- do.call(rbind, lapply(
    split(plot_df_aligned, list(plot_df_aligned$name, plot_df_aligned$method),
          drop = TRUE),
    function(chunk) {
      x_val <- chunk$x
      pa <- chunk$y      # Approx
      pm <- chunk$f_mcmc  # MCMC
      eps <- 1e-12

      # Normalize
      Za <- trapz(x_val, pa)
      pa <- if (Za > 0) pa / Za else pa
      Zm <- trapz(x_val, pm)
      pm <- if (Zm > 0) pm / Zm else pm

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
  ))
  rownames(metrics_df) <- NULL
  metrics_df <- metrics_df[order(metrics_df$method, -metrics_df$Bias_SDs), ]

  # Add overall by averaging
  overall <- aggregate(
    cbind(L1, L1_percent, js, JS_percent, KL_fwd, KL_rev, Bias_SDs) ~
      method,
    data = metrics_df, FUN = mean
  )
  overall$name <- "Overall"
  metrics_df <- rbind(metrics_df, overall)
  metrics_df$name <- factor(metrics_df$name, levels = c("Overall", parnames))
  # Average within name+method (collapses duplicates for Overall)
  metrics_df <- aggregate(
    cbind(L1, L1_percent, js, JS_percent, KL_fwd, KL_rev, Bias_SDs) ~
      name + method,
    data = metrics_df, FUN = mean
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
    err_long$name <- factor(err_long$name, levels = rev(levels(factor(metrics_df$name))))

    p_errors <-
      ggplot2::ggplot(err_long) +
      ggplot2::geom_bar(
        ggplot2::aes(x = name, y = value, fill = method),
        width = 0.7,
        stat = "identity",
        position = ggplot2::position_dodge()
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
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
      ggplot2::facet_wrap(~name, scales = "free") +
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
      # Summarise plot_df by name + method
      label_df <- do.call(rbind, lapply(
        split(plot_df, list(plot_df$name, plot_df$method), drop = TRUE),
        function(chunk) {
          data.frame(
            name = chunk$name[1],
            method = chunk$method[1],
            x = quantile(chunk$x, probs = 0.95),
            y = 0.75 * max(chunk$y),
            stringsAsFactors = FALSE
          )
        }
      ))
      rownames(label_df) <- NULL
      label_df <- merge(label_df, metrics_df, by = c("name", "method"))
      label_df$JS_percent <- paste0(round((1 - label_df$JS_percent) * 100, 1), "%")

      p_compare <-
        p_compare +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x, y, label = JS_percent, col = method),
          size = 3,
          position = "identity",
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
  layout_mat <- matrix(seq_len(n_rows * n_cols), nrow = n_rows, ncol = n_cols,
                       byrow = TRUE)
  layout_mat <- rbind(rep(n_rows * n_cols + 1, n_cols), layout_mat)
  layout(layout_mat, heights = c(0.8, rep(4, n_rows)))
  op <- par(mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))

  for (pnm in parnames) {
    mcmc_vals <- plot_df_blav$value[plot_df_blav$name == pnm]
    d_mcmc <- stats::density(mcmc_vals)

    sub_df <- plot_df[plot_df$name == pnm, ]
    xlim <- range(c(d_mcmc$x, sub_df$x), na.rm = TRUE)
    ylim <- range(c(d_mcmc$y, sub_df$y), na.rm = TRUE)

    plot(d_mcmc, main = pnm, font.main = 1, xlab = "", ylab = "", bty = "n",
         col = NA, xlim = xlim, ylim = ylim, zero.line = FALSE)
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
        lbl <- paste0(round(100 * (1 - m_sub$JS_percent[i]), 1), "%")
        mtext(lbl, side = 3, line = -1.2 - (i - 1) * 1,
              col = mycols[as.character(m_sub$method[i])], cex = 0.7, adj = 0.95)
      }
    }
  }

  # Fill any remaining empty panels
  remaining <- n_rows * n_cols - n_params
  for (i in seq_len(remaining)) plot.new()

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
  legend("center",
         legend = leg_labels,
         pch = leg_pch,
         pt.bg = leg_pt_bg,
         pt.cex = 2,
         col = leg_col,
         lwd = leg_lwd,
         lty = leg_lty,
         horiz = TRUE, bty = "n", cex = 0.9, seg.len = 1.5)
  p_compare <- recordPlot()

  # -- p_errors: horizontal bar chart of L1 and JS errors --
  par(mfrow = c(1, 2), mar = c(4, 8, 3, 1), oma = c(0, 0, 2, 0))

  err_long <- metrics_df[, c("name", "method", "L1_percent", "JS_percent")]
  err_methods <- levels(factor(err_long$method))
  err_names <- rev(levels(factor(err_long$name)))
  n_methods <- length(err_methods)

  for (metric in c("L1_percent", "JS_percent")) {
    label <- if (metric == "L1_percent") "L1 Error" else "Jensen-Shannon Error"
    mat <- matrix(0, nrow = length(err_names), ncol = n_methods,
                  dimnames = list(err_names, err_methods))
    for (i in seq_len(nrow(err_long))) {
      r <- as.character(err_long$name[i])
      m <- as.character(err_long$method[i])
      if (r %in% err_names) mat[r, m] <- err_long[[metric]][i]
    }

    barplot(t(mat) * 100, beside = TRUE, horiz = TRUE, las = 1,
            col = mycols[err_methods], border = NA,
            main = label, xlab = "%", names.arg = err_names)
  }
  mtext("Error Metrics", outer = TRUE, cex = 1)
  legend("topright", legend = err_methods, fill = mycols[err_methods],
         border = NA, bty = "n", cex = 0.8)
  p_errors <- recordPlot()

  par(op)

  list(
    p_compare = p_compare,
    p_errors = p_errors,
    metrics_df = metrics_df
  )
}
# nocov end
