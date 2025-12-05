compare_mcmc <- function(fit_blavaan, ...) {
  parnames <- unique(names(coef(fit_blavaan)))

  # MCMC Histograms
  draws <- do.call("rbind", blavInspect(fit_blavaan, "mcmc"))
  plot_df_blav <-
    as.data.frame(draws) |>
    pivot_longer(everything()) |>
    mutate(name = factor(name, levels = parnames))

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

  # Create plot
  plot_df <-
    fit_inlavaan_list |>
    map(function(plot_df_list) {
      plot_df <-
        imap(plot_df_list, \(x, idx) mutate(x, name = idx)) |>
        bind_rows() |>
        mutate(name = factor(name, levels = parnames))
    }) |>
    bind_rows(.id = "method") |>
    mutate(
      method = factor(
        method,
        levels = c("skewnorm", "asymgaus", "marggaus", "sampling")
      )
    )

  p_compare <-
    ggplot() +
    geom_density(
      data = plot_df_blav,
      aes(value, fill = "MCMC"),
      col = NA,
      alpha = 0.38
    ) +
    geom_line(
      data = plot_df,
      aes(x, y, group = method, col = method),
      linewidth = 0.75
    ) +
    facet_wrap(~name, scales = "free") +
    scale_colour_manual(
      values = c(
        "skewnorm" = "#00A6AA",
        "asymgaus" = "#F18F00",
        "marggaus" = "#adbf04",
        "sampling" = "#9C6FAE"
      )
    ) +
    scale_fill_manual(
      values = c("MCMC" = "#131516")
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 4, r = 8, b = 8, l = 8),
      legend.position = "top",
      legend.box.spacing = unit(2, "pt"),
      legend.key.width = unit(1, "cm")
    ) +
    labs(x = NULL, y = NULL, col = NULL, fill = NULL)

  # Helper function for Integration
  trapz <- function(x, y) {
    if (length(x) < 2) {
      return(0)
    }
    sum((head(y, -1) + tail(y, 1)) * diff(x) / 2)
  }

  # Align MCMC to Approximation Grid
  plot_df_aligned <-
    plot_df |>
    group_by(name, method) |>
    group_modify(function(data, keys) {
      # Get matching MCMC samples
      mcmc_vals <- filter(plot_df_blav, name == keys$name)$value

      # Create KDE of MCMC (gold standard). We constrain the KDE to the range
      # of your approximation to prevent extrapolation
      d <- density(
        mcmc_vals,
        from = min(data$x),
        to = max(data$x),
        n = length(data$x)
      )

      # Interpolate KDE onto exact x-points of your approximation. 'rule = 2'
      # clamps values at the ends if floating point errors occur
      f_mcmc <- approx(x = d$x, y = d$y, xout = data$x, rule = 2)$y

      mutate(data, f_mcmc = f_mcmc)
    }) |>
    ungroup()

  # Calculate Metrics
  metrics_df <-
    plot_df_aligned |>
    group_by(name, method) |>
    summarise(
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

        tibble(
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
    ) |>
    unpack(results) |>
    arrange(method, desc(Bias_SDs))

  # Add overall by averaging
  metrics_df <-
    metrics_df |>
    bind_rows(mutate(metrics_df, name = "Overall")) |>
    mutate(name = factor(name, levels = c("Overall", parnames))) |>
    summarise(across(L1:Bias_SDs, mean), .by = c(name, method))

  # Plot of L1 and JS errors
  p_errors <-
    metrics_df |>
    pivot_longer(
      cols = c(L1_percent, JS_percent),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      name = fct_rev(name),
      metric = recode(
        metric,
        L1_percent = "L1 Error",
        JS_percent = "Jensen-Shannon Error"
      )
    ) |>
    ggplot() +
    geom_bar(
      aes(x = name, y = value, fill = method),
      width = 0.7,
      stat = "identity",
      position = position_dodge()
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
      values = c(
        "skewnorm" = "#00A6AA",
        "asymgaus" = "#F18F00",
        "marggaus" = "#adbf04",
        "sampling" = "#9C6FAE"
      )
    ) +
    facet_grid(. ~ metric, scales = "free_x") +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 4, r = 8, b = 8, l = 8),
      legend.position = "top",
      legend.box.spacing = unit(2, "pt"),
      legend.key.width = unit(1, "cm")
    ) +
    labs(x = NULL, y = NULL, fill = NULL)

  list(
    p_compare = p_compare,
    p_errors = p_errors,
    metrics_df = metrics_df
  )
}
