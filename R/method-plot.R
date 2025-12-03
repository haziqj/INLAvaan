#' @exportS3Method plot inlavaan_internal
#' @keywords internal
plot.inlavaan_internal <- function(x, ...) {
  all_plots <- list()
  postmode <- x$summary[, "Mode"]

  for (j in seq_along(x$pdf_data)) {
    param <- names(x$pdf_data)[j]
    plot_df <- x$pdf_data[[param]]

    p_dens <-
      ggplot(plot_df, aes(x, y)) +
      geom_line() +
      geom_vline(xintercept = postmode[j], linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(x = NULL, y = NULL, subtitle = param)

    all_plots[[param]] <- p_dens
  }

  cowplot::plot_grid(plotlist = all_plots)
}

#' Plot for INLAvaan objects
#'
#' @param x An `INLAvaan` object
#' @param y Not used
#' @param ... Not used
#'
#' @export
#' @rdname INLAvaan-methods
setMethod("plot", "INLAvaan", function(x, y, ...) {
  plot.inlavaan_internal(x@external$inlavaan_internal, ...)
})
