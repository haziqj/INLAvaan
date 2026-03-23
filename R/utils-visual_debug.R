# nocov start
visual_debug <- function(object, params, logscale = FALSE, use_ggplot = TRUE,
                         points = FALSE) {
  if (inherits(object, "INLAvaan")) {
    dat_list <- object@external$inlavaan_internal$visual_debug
  } else if (inherits(object, "inlavaan_internal")) {
    dat_list <- object$visual_debug
  }

  all_names <- names(coef(object))
  if (!missing(params) && !is.null(params)) {
    if (is.numeric(params)) {
      keep_names <- all_names[params]
    } else {
      keep_names <- params
    }
    dat_list <- dat_list[names(dat_list) %in% keep_names]
    all_names <- keep_names
  }

  type_labels <- c(
    Original = "Raw", Corrected = "Corrected",
    SN_Fit = "Skew-normal fit"
  )
  type_cols <- c(
    Original = "gray60", Corrected = "black", SN_Fit = "red2"
  )
  type_lty <- c(Original = 1, Corrected = 1)
  type_lwd <- c(Original = 1.3, Corrected = 1.3)

  use_ggplot <- isTRUE(use_ggplot) &&
    requireNamespace("ggplot2", quietly = TRUE)

  if (use_ggplot) {
    # --- ggplot2 version ---
    plot_df <- do.call(rbind, Map(
      function(nm, df) { df$name <- nm; df },
      names(dat_list), dat_list
    ))
    rownames(plot_df) <- NULL
    plot_df <- reshape(
      plot_df, direction = "long",
      varying = list(c("Original", "Corrected", "SN_Fit")),
      v.names = "value",
      timevar = "type",
      times = c("Original", "Corrected", "SN_Fit"),
      idvar = c("name", "x")
    )
    rownames(plot_df) <- NULL
    plot_df$name <- factor(plot_df$name, levels = all_names)
    plot_df$type <- factor(
      plot_df$type,
      levels = c("Original", "Corrected", "SN_Fit"),
      labels = c("Raw", "Corrected", "Skew-normal fit")
    )
    if (isTRUE(logscale)) {
      plot_df$value[plot_df$value <= 0] <- NA
      plot_df <- plot_df[!is.na(plot_df$value), ]
      plot_df$value <- log10(plot_df$value)
    }

    x <- type <- value <- NULL # no visible binding NOTE
    df_sn <- subset(plot_df, type == "Skew-normal fit")

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x, value, col = type,
                                                linetype = type,
                                                linewidth = type)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(. ~ name) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "top",
        legend.key.width = grid::unit(1.2, "cm")
      ) +
      ggplot2::labs(col = NULL, x = NULL, y = NULL,
                    linetype = NULL, linewidth = NULL) +
      ggplot2::scale_colour_manual(
        values = c("Raw" = "gray60", "Corrected" = "black",
                   "Skew-normal fit" = "red2")
      ) +
      ggplot2::scale_linetype_manual(
        values = c("Raw" = "solid", "Corrected" = "solid",
                   "Skew-normal fit" = "dashed")
      ) +
      ggplot2::scale_linewidth_manual(
        values = c("Raw" = 0.65, "Corrected" = 0.65, "Skew-normal fit" = 0.45)
      )

    if (isTRUE(points)) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(shape = type)) +
        ggplot2::scale_shape_manual(
          values = c("Raw" = 16, "Corrected" = 16, "Skew-normal fit" = 4),
          name = NULL
        )
    }

    return(p)
  }

  # --- base R fallback ---
  n_params <- length(dat_list)
  n_cols <- ceiling(sqrt(n_params))
  n_rows <- ceiling(n_params / n_cols)

  # Reserve top row for a horizontal legend
  layout_mat <- matrix(seq_len(n_rows * n_cols), nrow = n_rows, ncol = n_cols,
                       byrow = TRUE)
  layout_mat <- rbind(rep(n_rows * n_cols + 1, n_cols), layout_mat)
  layout(layout_mat, heights = c(0.8, rep(4, n_rows)))
  op <- par(mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))
  on.exit(par(op))

  for (nm in names(dat_list)) {
    dd <- dat_list[[nm]]
    xvals <- dd$x
    ycols <- c("Original", "Corrected", "SN_Fit")

    ylim_vals <- unlist(dd[, ycols, drop = FALSE])
    if (isTRUE(logscale)) {
      ylim_vals[ylim_vals <= 0] <- NA
      ylim_vals <- log10(ylim_vals[!is.na(ylim_vals)])
    }

    plot(NULL, xlim = range(xvals), ylim = range(ylim_vals, na.rm = TRUE),
         main = nm, font.main = 1, xlab = "", ylab = "", bty = "n",
         axes = TRUE)
    # grid(col = "lightgray", lty = "solid")

    for (tp in ycols) {
      yvals <- dd[[tp]]
      if (isTRUE(logscale)) {
        yvals[yvals <= 0] <- NA
        yvals <- log10(yvals)
      }
      if (tp == "SN_Fit") {
        lines(xvals, yvals, col = type_cols[tp], lty = 2, lwd = 0.9)
        if (isTRUE(points)) {
          points(xvals, yvals, col = type_cols[tp], pch = 4, cex = 0.8)
        }
      } else {
        lines(xvals, yvals, col = type_cols[tp], lty = type_lty[tp],
              lwd = type_lwd[tp])
        if (isTRUE(points)) {
          points(xvals, yvals, col = type_cols[tp], pch = 16, cex = 0.8)
        }
      }
    }
  }

  # Fill remaining empty panels
  remaining <- n_rows * n_cols - n_params
  for (i in seq_len(remaining)) plot.new()

  # Top legend panel
  par(mar = c(0, 0, 0, 0))
  plot.new()
  if (isTRUE(points)) {
    legend_pch <- c(16L, 16L, 4L)
  } else {
    legend_pch <- c(NA_integer_, NA_integer_, NA_integer_)
  }
  legend(
    "center",
    legend = c("Raw", "Corrected", "Skew-normal fit"),
    col    = c(type_cols["Original"], type_cols["Corrected"], type_cols["SN_Fit"]),
    lty    = c(1L, 1L, 2L),
    pch    = legend_pch,
    lwd    = c(1.3, 1.3, 0.9),
    horiz  = TRUE, bty = "n", cex = 0.9, seg.len = 1.5
  )

  invisible(NULL)
}
# nocov end
