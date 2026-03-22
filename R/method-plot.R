#' @exportS3Method plot inlavaan_internal
#' @keywords internal
plot.inlavaan_internal <- function(x, truth,
                                   type = c("marg_pdf", "sn_fit", "sn_fit_log"),
                                   params = "all",
                                   nrow = NULL, ncol = NULL,
                                   use_ggplot = TRUE, points = FALSE, ...) {
  type <- match.arg(type)

  # Dispatch to visual_debug for sn_fit types
  if (type %in% c("sn_fit", "sn_fit_log")) {
    logscale <- type == "sn_fit_log"
    if (identical(params, "all")) params <- NULL
    return(visual_debug(
      x, params = params, logscale = logscale,
      use_ggplot = use_ggplot, points = points
    ))
  }

  all_names <- names(x$pdf_data)
  postmode <- setNames(x$summary[, "Mode"], rownames(x$summary))

  # Resolve which parameters to plot
  if (identical(params, "all")) {
    param_names <- all_names
  } else {
    bad <- setdiff(params, all_names)
    if (length(bad) > 0) { # nocov start
      stop(
        "Unknown parameter(s): ", paste(bad, collapse = ", "),
        "\nAvailable: ", paste(all_names, collapse = ", ")
      )
    } # nocov end
    param_names <- params
  }
  n_plot <- length(param_names)

  use_ggplot <- isTRUE(use_ggplot) &&
    requireNamespace("ggplot2", quietly = TRUE)

  if (use_ggplot) {
    # --- ggplot2 version (facet_wrap, no extra dependency) ---
    plot_df <- do.call(rbind, Map(
      function(nm, df) { df$name <- nm; df },
      param_names, x$pdf_data[param_names]
    ))
    rownames(plot_df) <- NULL
    plot_df$name <- factor(plot_df$name, levels = param_names)

    modes <- postmode[param_names]
    if (missing(truth)) {
      vline_df <- data.frame(
        name = factor(param_names, levels = param_names),
        xint = modes
      )
    } else {
      n_truth <- min(length(truth), length(param_names))
      vline_df <- data.frame(
        name = factor(param_names[seq_len(n_truth)], levels = param_names),
        xint = truth[seq_len(n_truth)]
      )
    }

    x <- xint <- NULL # no visible binding NOTE
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x, y)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = "free", nrow = nrow, ncol = ncol) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = NULL)

    if (missing(truth)) {
      p <- p + ggplot2::geom_vline(
        data = vline_df,
        ggplot2::aes(xintercept = xint),
        linetype = "dashed", color = "red3"
      )
    } else {
      p <- p + ggplot2::geom_vline(
        data = vline_df,
        ggplot2::aes(xintercept = xint),
        linetype = "dotted", color = "steelblue3"
      )
    }

    return(p)
  }

  # --- base R fallback ---
  if (!is.null(ncol) && !is.null(nrow)) {
    n_cols <- ncol; n_rows <- nrow
  } else if (!is.null(ncol)) { # nocov start
    n_cols <- ncol; n_rows <- ceiling(n_plot / n_cols)
  } else if (!is.null(nrow)) {
    n_rows <- nrow; n_cols <- ceiling(n_plot / n_rows)
  } else { # nocov end
    n_cols <- ceiling(sqrt(n_plot))
    n_rows <- ceiling(n_plot / n_cols)
  }

  layout_mat <- matrix(seq_len(n_rows * n_cols), nrow = n_rows, ncol = n_cols,
                       byrow = TRUE)
  layout_mat <- rbind(rep(n_rows * n_cols + 1, n_cols), layout_mat)
  layout(layout_mat, heights = c(0.8, rep(4, n_rows)))
  op <- par(mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))
  on.exit(par(op))

  for (j in seq_len(n_plot)) {
    param <- param_names[j]
    plot_df <- x$pdf_data[[param]]

    plot(
      plot_df$x,
      plot_df$y,
      type = "l",
      main = param,
      font.main = 1,
      xlab = "",
      ylab = "",
      bty = "n",
      axes = TRUE,
      col = "black",
      ...
    )

    if (missing(truth)) {
      abline(v = postmode[param], lty = 2, col = "red3")
    } else {
      abline(v = truth[j], lty = 3, col = "steelblue3")
    }
  }

  remaining <- n_rows * n_cols - n_plot
  for (i in seq_len(remaining)) plot.new()

  par(mar = c(0, 0, 0, 0))
  plot.new()
  if (missing(truth)) {
    legend("center", legend = "Mode", col = "red3", lty = 2,
           horiz = TRUE, bty = "n", cex = 0.9, seg.len = 1.5)
  } else {
    legend("center", legend = "Truth", col = "steelblue3", lty = 3,
           horiz = TRUE, bty = "n", cex = 0.9, seg.len = 1.5)
  }

  invisible(NULL)
}

#' @param x An object of class [INLAvaan].
#' @param y Not used.
#' @param type Character. One of \code{"marg_pdf"} (default; posterior marginal
#'   densities), \code{"sn_fit"} (skew-normal fit diagnostic on natural scale),
#'   or \code{"sn_fit_log"} (same on log scale).
#' @param points Logical. If \code{TRUE}, overlay data points on the curves
#'   (applies to \code{sn_fit} and \code{sn_fit_log} types).
#' @param ... Additional arguments passed to the plot function.
#'
#' @rdname INLAvaan-class
#' @export
setMethod("plot", "INLAvaan", function(x, y, ...) {
  plot.inlavaan_internal(x@external$inlavaan_internal, ...)
})
