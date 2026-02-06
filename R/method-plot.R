#' @exportS3Method plot inlavaan_internal
#' @keywords internal
plot.inlavaan_internal <- function(x, truth, ...) {
  # 1. Calculate Grid Dimensions
  #    Determine how many rows/cols needed based on number of plots
  n_params <- max(x$partable$free)
  n_cols <- ceiling(sqrt(n_params))
  n_rows <- ceiling(n_params / n_cols)

  # 2. Setup Graphics Parameters
  #    Save current settings (op) and restore them on exit (Crucial for packages!)
  #    mfrow: sets the grid layout
  #    mar: sets margins (bottom, left, top, right) to be "minimal"
  op <- par(mfrow = c(n_rows, n_cols), mar = c(2, 2, 3, 1))
  on.exit(par(op))

  postmode <- x$summary[, "Mode"]

  # 3. Plot Loop
  for (j in seq_len(n_params)) {
    param <- names(x$pdf_data)[j]
    plot_df <- x$pdf_data[[param]]

    # Base plot equivalent to geom_line + theme_minimal
    plot(
      plot_df$x,
      plot_df$y,
      type = "l", # Line plot
      main = param, # Title (subtitle in ggplot)
      xlab = "",
      ylab = "", # Remove axis labels
      bty = "n", # "n" = No box (mimics theme_minimal)
      axes = TRUE, # Keep numbers
      col = "black",
      ...
    )

    # Optional: Add grid lines to mimic theme_minimal
    grid(col = "lightgray", lty = "dotted")

    # Add Vertical Lines
    if (missing(truth)) {
      abline(v = postmode[j], lty = 2, col = "red3") # lty 2 = dashed
    } else {
      abline(v = truth[j], lty = 3, col = "steelblue3") # lty 3 = dotted
    }
  }

  # Base R plots directly to the device, so we don't return an object
  invisible(NULL)
}

# plot.inlavaan_internal <- function(x, truth, ...) {
#   all_plots <- list()
#   postmode <- x$summary[, "Mode"]
#
#   for (j in seq_len(max(x$partable$free))) {
#     param <- names(x$pdf_data)[j]
#     plot_df <- x$pdf_data[[param]]
#
#     p_dens <-
#       ggplot(plot_df, aes(x, y)) +
#       geom_line() +
#       theme_minimal() +
#       labs(x = NULL, y = NULL, subtitle = param)
#
#     if (missing(truth)) {
#       p_dens <- p_dens +
#         geom_vline(
#           xintercept = postmode[j],
#           linetype = "dashed",
#           color = "red3"
#         )
#     } else {
#       p_dens <- p_dens +
#         geom_vline(
#           xintercept = truth[j],
#           linetype = "dotted",
#           color = "steelblue3"
#         )
#     }
#
#     all_plots[[param]] <- p_dens
#   }
#
#   cowplot::plot_grid(plotlist = all_plots)
# }

#' @param x An object of class [INLAvaan].
#' @param y Not used.
#' @param ... Not used.
#'
#' @rdname INLAvaan-class
#' @export
setMethod("plot", "INLAvaan", function(x, y, ...) {
  plot.inlavaan_internal(x@external$inlavaan_internal, ...)
})
