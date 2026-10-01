# nocov start
visual_debug <- function(object, params, logscale = FALSE, use_ggplot = TRUE,
                         points = FALSE, raw = TRUE) {
  if (inherits(object, "INLAvaan")) {
    dat_list <- object@external$inlavaan_internal$visual_debug
  } else if (inherits(object, "inlavaan_internal")) {
    dat_list <- object$visual_debug
  }
  dat_list <- Filter(Negate(is.null), dat_list)  # fast-path params have none

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

  # "Raw" vs "Corrected" when both are shown; otherwise the corrected curve is
  # simply the evaluated grid
  type_labels <- c(
    Original = "Raw",
    Corrected = if (isTRUE(raw)) "Corrected" else "Evaluated",
    SN_Fit = "Skew-normal fit"
  )
  type_cols <- c(
    Original = "gray60", Corrected = "black", SN_Fit = "red2"
  )
  type_lty <- c(Original = 1, Corrected = 1, SN_Fit = 2)
  type_lwd <- c(Original = 1.3, Corrected = 1.3, SN_Fit = 1.3)

  # Grid evaluations are drawn by connecting the ordinates (drop the raw
  # uncorrected curve if raw = FALSE); the skew-normal fit is drawn smooth
  grid_cols <- c("Original", "Corrected")
  if (!isTRUE(raw)) grid_cols <- setdiff(grid_cols, "Original")
  ycols <- c(grid_cols, "SN_Fit")

  # Smooth skew-normal density from the fitted z-space parameters. Objects
  # fitted before these were stored fall back to the grid ordinates.
  sn_curve <- function(dd, n = 201) {
    snp <- attr(dd, "sn_params")
    if (is.null(snp)) return(data.frame(x = dd$x, value = dd$SN_Fit))
    xx <- seq(min(dd$x), max(dd$x), length.out = n)
    data.frame(
      x = xx,
      value = dsnorm(xx, xi = snp[["xi"]], omega = snp[["omega"]],
                     alpha = snp[["alpha"]], logC = snp[["logC"]])
    )
  }
  to_log10 <- function(v) {
    v[v <= 0] <- NA
    log10(v)
  }

  use_ggplot <- isTRUE(use_ggplot) &&
    requireNamespace("ggplot2", quietly = TRUE)

  if (use_ggplot) {
    # --- ggplot2 version ---
    grid_df <- do.call(rbind, Map(
      function(nm, dd) {
        do.call(rbind, lapply(grid_cols, function(tp) {
          data.frame(name = nm, x = dd$x, value = dd[[tp]], type = tp)
        }))
      },
      names(dat_list), dat_list
    ))
    sn_df <- do.call(rbind, Map(
      function(nm, dd) data.frame(name = nm, sn_curve(dd), type = "SN_Fit"),
      names(dat_list), dat_list
    ))
    plot_df <- rbind(grid_df, sn_df)
    rownames(plot_df) <- NULL
    plot_df$name <- factor(plot_df$name, levels = all_names)
    plot_df$type <- factor(
      plot_df$type,
      levels = ycols,
      labels = type_labels[ycols]
    )
    if (isTRUE(logscale)) {
      plot_df$value <- to_log10(plot_df$value)
      plot_df <- plot_df[!is.na(plot_df$value), ]
    }

    x <- type <- value <- NULL # no visible binding NOTE
    is_sn <- plot_df$type == type_labels[["SN_Fit"]]
    by_label <- function(v) setNames(v, type_labels[names(v)])

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x, value, col = type,
                                                linetype = type,
                                                linewidth = type)) +
      ggplot2::geom_line(data = plot_df[!is_sn, ])

    if (isTRUE(points)) {
      # Points only at the grid ordinates, not on the smooth SN curve
      p <- p +
        ggplot2::geom_point(ggplot2::aes(shape = type), na.rm = TRUE) +
        ggplot2::scale_shape_manual(
          values = by_label(c(Original = 16, Corrected = 16, SN_Fit = NA)),
          name = NULL
        )
    }

    # SN curve drawn last so it sits on top of the grid lines and points
    p <- p +
      ggplot2::geom_line(data = plot_df[is_sn, ]) +
      ggplot2::facet_wrap(. ~ name) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "top",
        legend.key.width = grid::unit(1.2, "cm")
      ) +
      ggplot2::labs(col = NULL, x = NULL, y = NULL,
                    linetype = NULL, linewidth = NULL) +
      ggplot2::scale_colour_manual(values = by_label(type_cols)) +
      ggplot2::scale_linetype_manual(
        values = by_label(c(Original = "solid", Corrected = "solid",
                            SN_Fit = "dashed"))
      ) +
      ggplot2::scale_linewidth_manual(
        values = by_label(c(Original = 0.65, Corrected = 0.65, SN_Fit = 0.65))
      )

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
    sn <- sn_curve(dd)
    grid_y <- dd[grid_cols]
    if (isTRUE(logscale)) {
      grid_y[] <- lapply(grid_y, to_log10)
      sn$value <- to_log10(sn$value)
    }

    plot(NULL, xlim = range(dd$x),
         ylim = range(unlist(grid_y), sn$value, na.rm = TRUE),
         main = nm, font.main = 1, xlab = "", ylab = "", bty = "n",
         axes = TRUE)
    # grid(col = "lightgray", lty = "solid")

    for (tp in grid_cols) {
      lines(dd$x, grid_y[[tp]], col = type_cols[tp], lty = type_lty[tp],
            lwd = type_lwd[tp])
      if (isTRUE(points)) {
        points(dd$x, grid_y[[tp]], col = type_cols[tp], pch = 16, cex = 0.8)
      }
    }
    lines(sn$x, sn$value, col = type_cols["SN_Fit"], lty = type_lty["SN_Fit"],
          lwd = type_lwd["SN_Fit"])
  }

  # Fill remaining empty panels
  remaining <- n_rows * n_cols - n_params
  for (i in seq_len(remaining)) plot.new()

  # Top legend panel
  par(mar = c(0, 0, 0, 0))
  plot.new()
  if (isTRUE(points)) {
    legend_pch <- c(Original = 16L, Corrected = 16L, SN_Fit = NA_integer_)
  } else {
    legend_pch <- c(Original = NA_integer_, Corrected = NA_integer_,
                    SN_Fit = NA_integer_)
  }
  legend(
    "center",
    legend = unname(type_labels[ycols]),
    col    = unname(type_cols[ycols]),
    lty    = type_lty[ycols],
    pch    = legend_pch[ycols],
    lwd    = type_lwd[ycols],
    horiz  = TRUE, bty = "n", cex = 0.9, seg.len = 1.5
  )

  invisible(NULL)
}
# nocov end
