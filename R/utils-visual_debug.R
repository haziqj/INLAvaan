# nocov start
visual_debug <- function(object, params, logscale = FALSE) {
  if (inherits(object, "INLAvaan")) {
    dat_list <- object@external$inlavaan_internal$visual_debug
  } else if (inherits(object, "inlavaan_internal")) {
    dat_list <- object$visual_debug
  }

  plot_df <- dplyr::bind_rows(dat_list, .id = "name")
  plot_df <- tidyr::pivot_longer(
    plot_df,
    cols = 3:5,
    names_to = "type",
    values_to = "value"
  )
  plot_df$name <- factor(plot_df$name, levels = names(coef(object)))

  if (missing(params)) {
    # do nothing
  } else {
    if (is.numeric(params)) {
      keep_names <- names(coef(object))[params]
    } else {
      keep_names <- params
    }
    plot_df <- plot_df[plot_df$name %in% keep_names, ]
  }

  plot_df$type <- factor(
    plot_df$type,
    levels = c("Original", "Corrected", "SN_Fit"),
    labels = c("Original", "Corrected", "Skew normal fit")
  )
  if (isTRUE(logscale)) {
    plot_df <- plot_df %>%
      dplyr::mutate(value = ifelse(value <= 0, NA, value))
    plot_df <- plot_df %>%
      dplyr::filter(!is.na(value))
    plot_df$value <- log10(plot_df$value)
  }

  x <- type <- NULL # no visible binding NOTE
  ggplot2::ggplot(
    plot_df,
    aes(x, value, col = type, linetype = type, linewidth = type)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(. ~ name) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      legend.key.width = unit(1.2, "cm")
    ) +
    ggplot2::labs(
      col = NULL,
      x = NULL,
      y = NULL,
      linetype = NULL,
      linewidth = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Original" = "gray60",
        "Corrected" = "black",
        "Skew normal fit" = "red2"
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Original" = "solid",
        "Corrected" = "solid",
        "Skew normal fit" = "dashed"
      )
    ) +
    ggplot2::scale_linewidth_manual(
      values = c(
        "Original" = 0.65,
        "Corrected" = 0.65,
        "Skew normal fit" = 0.45
      )
    )
}
# nocov end
