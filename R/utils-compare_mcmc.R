# Silence R CMD check notes for dplyr/ggplot2 pipelines
utils::globalVariables(c(
  "name",
  "method",
  "value",
  "y",
  "f_mcmc",
  "results",
  "Bias_SDs",
  "L1",
  "L1_percent",
  "JS_percent",
  "metric"
))

# nocov start
compare_mcmc <- function(fit_blavaan, ..., show_error = TRUE) {
  parnames <- unique(names(coef(fit_blavaan)))

  # MCMC Histograms
  draws <- do.call("rbind", blavaan::blavInspect(fit_blavaan, "mcmc"))
  plot_df_blav <-
    as.data.frame(draws) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::mutate(name = factor(name, levels = parnames))

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
  plot_df <-
    fit_inlavaan_list %>%
    purrr::map(function(plot_df_list) {
      plot_df <-
        purrr::imap(plot_df_list, function(x, idx) {
          dplyr::mutate(x, name = idx)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(name = factor(name, levels = parnames))
    }) %>%
    dplyr::bind_rows(.id = "method") %>%
    dplyr::mutate(method = factor(method, inlav_names))

  # Helper function for Integration
  trapz <- function(x, y) {
    if (length(x) < 2) {
      return(0)
    }
    sum((head(y, -1) + tail(y, 1)) * diff(x) / 2)
  }

  # Align MCMC to Approximation Grid
  plot_df_aligned <-
    plot_df %>%
    dplyr::group_by(name, method) %>%
    dplyr::group_modify(function(data, keys) {
      # Get matching MCMC samples
      mcmc_vals <- dplyr::filter(plot_df_blav, name == keys$name)$value

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
      f_mcmc <- stats::approx(x = d$x, y = d$y, xout = data$x, rule = 2)$y

      dplyr::mutate(data, f_mcmc = f_mcmc)
    }) %>%
    dplyr::ungroup()

  # Calculate Metrics
  metrics_df <-
    plot_df_aligned %>%
    dplyr::group_by(name, method) %>%
    dplyr::summarise(
      results = {
        x <- x
        pa <- y # Approx
        pm <- f_mcmc # MCMC
        eps <- 1e-12

        # Normalize
        Za <- trapz(x, pa)
        pa <- if (Za > 0) pa / Za else pa
        Zm <- trapz(x, pm)
        pm <- if (Zm > 0) pm / Zm else pm

        # --- KL Divergences ---
        # Forward KL: MCMC || Approx (How much truth did we miss?)
        kl_fwd <- trapz(x, pm * (log(pmax(pm, eps)) - log(pmax(pa, eps))))

        # Reverse KL: Approx || MCMC (Did we hallucinate probability?)
        kl_rev <- trapz(x, pa * (log(pmax(pa, eps)) - log(pmax(pm, eps))))

        # --- Interpretability Hacks ---

        # 1. Jensen-Shannon % (0 = Identical, 1 = Disjoint)
        m_mix <- 0.5 * (pm + pa)
        kl_pm <- trapz(x, pm * (log(pmax(pm, eps)) - log(pmax(m_mix, eps))))
        kl_qm <- trapz(x, pa * (log(pmax(pa, eps)) - log(pmax(m_mix, eps))))
        js_val <- 0.5 * kl_pm + 0.5 * kl_qm
        js_pct <- js_val / log(2)

        # 2. Gaussian Bias Equivalent
        # "How many SDs apart are the means to generate this much divergence?"
        # If KL_fwd is 0.02, it's like saying the mean is biased by 0.2 SDs.
        bias_equiv <- sqrt(2 * abs(kl_fwd))

        # --- L1 (Total Variation) ---
        l1 <- trapz(x, abs(pm - pa))

        dplyr::tibble(
          L1 = l1,
          L1_percent = l1 / 2, # 0 to 1 scale (Area difference)
          js = js_val,
          JS_percent = js_pct, # 0 to 1 scale (Information diff)
          KL_fwd = kl_fwd, # Raw KL (Forward)
          KL_rev = kl_rev, # Raw KL (Reverse)
          Bias_SDs = bias_equiv # "Z-score bias" equivalent
        )
      },
      .groups = "drop"
    ) %>%
    tidyr::unpack(results) %>%
    dplyr::arrange(method, dplyr::desc(Bias_SDs))

  # Add overall by averaging
  metrics_df <-
    metrics_df %>%
    dplyr::bind_rows(dplyr::mutate(metrics_df, name = "Overall")) %>%
    dplyr::mutate(name = factor(name, levels = c("Overall", parnames))) %>%
    dplyr::summarise(dplyr::across(L1:Bias_SDs, mean), .by = c(name, method))

  # Plot of L1 and JS errors
  p_errors <-
    metrics_df %>%
    tidyr::pivot_longer(
      cols = c(L1_percent, JS_percent),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      name = forcats::fct_rev(name),
      metric = dplyr::recode(
        metric,
        L1_percent = "L1 Error",
        JS_percent = "Jensen-Shannon Error"
      )
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      aes(x = name, y = value, fill = method),
      width = 0.7,
      stat = "identity",
      position = position_dodge()
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = mycols) +
    ggplot2::facet_grid(. ~ metric, scales = "free_x") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = margin(t = 4, r = 8, b = 8, l = 8),
      legend.position = "top",
      legend.box.spacing = unit(2, "pt"),
      legend.key.width = unit(1, "cm")
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL)

  p_compare <-
    ggplot2::ggplot() +
    ggplot2::geom_density(
      data = plot_df_blav,
      aes(value, fill = "MCMC"),
      col = NA,
      alpha = 0.38
    ) +
    # ggplot2::geom_histogram(
    #   data = plot_df_blav,
    #   aes(value, y = ggplot2::after_stat(density), fill = "MCMC"),
    #   alpha = 0.4,
    #   bins = 50,
    #   col = NA,
    #   position = "identity"
    # ) +
    ggplot2::geom_line(
      data = plot_df,
      aes(x, y, group = method, col = method),
      linewidth = 0.75
    ) +
    ggplot2::facet_wrap(~name, scales = "free") +
    ggplot2::scale_colour_manual(values = mycols) +
    ggplot2::scale_fill_manual(values = c("MCMC" = "#131516")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = margin(t = 4, r = 8, b = 8, l = 8),
      legend.position = "top",
      legend.box.spacing = unit(2, "pt"),
      legend.key.width = unit(1, "cm")
    ) +
    ggplot2::labs(x = NULL, y = NULL, col = NULL, fill = NULL)

  if (isTRUE(show_error)) {
    pos <- if (length(mycols) == 1) "identity" else ggplot2::position_dodge()
    p_compare <-
      p_compare +
      ggplot2::geom_text(
        data = plot_df %>%
          dplyr::summarise(
            x = quantile(x, probs = 0.95),
            y = 0.75 * max(y),
            .by = c(name, method)
          ) %>%
          dplyr::left_join(metrics_df, by = c("name", "method")) %>%
          dplyr::mutate(JS_percent = scales::percent(1 - JS_percent, 0.1)),
        aes(x, y, label = JS_percent, col = method),
        size = 3,
        position = "identity",
        show.legend = FALSE
      )
  }

  list(
    p_compare = p_compare,
    p_errors = p_errors,
    metrics_df = metrics_df
  )
}
# nocov end
