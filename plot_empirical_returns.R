
plot_empirical_returns <-
  function(market_data, crypto_colours) {
    
    # plot 1 - log returns line plot over time
    p1 <-
      market_data |>
      ggplot(mapping = aes(x = date, y = log_returns, color = crypto, data_id = crypto)) +
      geom_line_interactive(size = 0.8) +
      geom_hline(yintercept = 0) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans")
      ) +
      scale_x_date(breaks = "1 years", name = "") +
      scale_y_continuous(name = "Annual Log Returns", labels = scales::label_percent()) +
      scale_color_manual(values = crypto_colours)

    # plot 2 - log returns histogram
    p2 <-
      market_data |>
      ggplot(mapping = aes(x = log_returns, fill = crypto, data_id = crypto)) +
      geom_histogram_interactive(size = 1, bins = 10) +
      geom_text_interactive(
        data = market_data |> slice_max(by = crypto, n = 1, order_by = date),
        mapping = aes(label = crypto, color = crypto, x = 0, y = Inf),
        hjust = 1,
        vjust = 1.5,
        size = 4,
        angle = 90,
        fontface = "bold"
      ) +
      geom_vline(xintercept = 0) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      ) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans")
      ) +
      scale_fill_manual(values = crypto_colours) +
      scale_color_manual(values = crypto_colours) +
      scale_y_continuous(name = "") +
      scale_x_continuous(labels = scales::label_percent(), name = "") +
      facet_wrap(vars(crypto), nrow = 1) +
      theme(
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank()
      ) +
      coord_cartesian(ylim = as.Date(c(0, NA)))

    # plot 3 - log returns violin (not currently used)
    p3 <-
      market_data |>
      ggplot(mapping = aes(x = crypto, y = log_returns, fill = crypto, color = crypto, data_id = crypto)) +
      geom_violin_interactive() +
      scale_fill_manual(values = crypto_colours) +
      scale_color_manual(values = crypto_colours) +
      geom_text_interactive(
        data = data.frame(crypto = unique(market_data$crypto), y_label = market_data$log_returns |> min(na.rm = T)),
        mapping = aes(label = crypto, color = crypto, x = crypto, y = y_label),
        hjust = 0.5,
        vjust = 1,
        size = 4,
        nudge_y = -0.5,
        fontface = "bold"
      ) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()
      ) +
      scale_y_continuous(name = "Annual Log Returns", labels = scales::label_percent()) +
      scale_x_discrete(name = "") +
      coord_cartesian(ylim = c(market_data$log_returns |> min(na.rm = T) - 2, NA))

    patchwork_plot <- (p1 / p2)

    output <-
      girafe(
        ggobj = patchwork_plot,
        options = list(
          opts_hover(css = ""),
          opts_hover_inv(css = "opacity:0.15;"),
          opts_sizing(rescale = T)
        )
      )

    return(output)
  }
