#' @exportS3Method plot inlavaan_internal
#' @keywords internal
plot.inlavaan_internal <- function(x, truth, use_ggplot = TRUE, ...) {
  n_params <- max(x$partable$free)
  param_names <- names(x$pdf_data)[seq_len(n_params)]
  postmode <- x$summary[, "Mode"]

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

    vline_df <- data.frame(
      name = factor(param_names, levels = param_names),
      xint = if (missing(truth)) postmode[seq_len(n_params)] else truth
    )

    x <- xint <- NULL # no visible binding NOTE
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x, y)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = "free") +
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
  n_cols <- ceiling(sqrt(n_params))
  n_rows <- ceiling(n_params / n_cols)

  layout_mat <- matrix(seq_len(n_rows * n_cols), nrow = n_rows, ncol = n_cols,
                       byrow = TRUE)
  layout_mat <- rbind(rep(n_rows * n_cols + 1, n_cols), layout_mat)
  layout(layout_mat, heights = c(0.8, rep(4, n_rows)))
  op <- par(mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))
  on.exit(par(op))

  for (j in seq_len(n_params)) {
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

    # grid(col = "lightgray", lty = "solid")

    if (missing(truth)) {
      abline(v = postmode[j], lty = 2, col = "red3")
    } else {
      abline(v = truth[j], lty = 3, col = "steelblue3")
    }
  }

  remaining <- n_rows * n_cols - n_params
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
#' @param ... Not used.
#'
#' @rdname INLAvaan-class
#' @export
setMethod("plot", "INLAvaan", function(x, y, ...) {
  plot.inlavaan_internal(x@external$inlavaan_internal, ...)
})
