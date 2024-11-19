plot_open_over_time <-
  function(data) {                 
    p <-
      market_data |> 
      ggplot(aes(x = date, y = open, color = crypto, data_id = crypto)) +
      geom_label_interactive(
        data = market_data |> dplyr::slice_max(order_by = open, n=1, by=crypto),
        aes(y = open * 2.2, label = scales::dollar(open, accuracy = 0.01)),
        size = 3,
        fontface = "bold"
      ) +
      geom_label_interactive(
        data = market_data |> dplyr::slice_min(order_by = open, n=1, by=crypto) |> dplyr::filter(open >= 0.01),
        aes(y = open / 2.2, label = scales::dollar(open, accuracy = 0.01)),
        size = 3,
        fontface = "bold"
      ) +
      geom_label_interactive(
        data = market_data |> dplyr::slice_min(order_by = open, n=1, by=crypto) |> dplyr::filter(open < 0.01),
        aes(y = open / 2.2, label = scales::dollar(open, accuracy = 0.001)),
        size = 3,
        fontface = "bold"
      ) +
      geom_point_interactive(
        data = market_data |> dplyr::slice_max(order_by = open, n=1, by=crypto),
        size = 3
      ) +
      geom_point_interactive(
        data = market_data |> dplyr::slice_min(order_by = open, n=1, by=crypto),
        size = 3
      ) +
      geom_line_interactive(size = 1) +
      scale_x_date(breaks = "2 years", name = "Date") +
      scale_y_log10(breaks = 10^seq(-2, 6, by=1), 
                    label = scales::label_currency(accuracy = 0.01),
                    name = "Opening Price") +
      geom_text_interactive(
        data = market_data |> slice_max(by = crypto, n = 1, order_by = date),
        mapping = aes(label = crypto),
        hjust = 0,
        size = 4,
        nudge_x = 60,
        fontface = "bold"
      ) +
      theme_light() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, family = "sans")) +
      coord_cartesian(xlim = as.Date(c(NA, "2025-07-01")), ylim = c(NA, 100000)) +
      scale_color_manual(values = crypto_colours) 
    
    interactive_p <-
      girafe(ggobj = p, 
             options = list(
               opts_hover(css = ""),
               opts_hover_inv(css = "opacity: 0;"),
               opts_sizing(rescale = FALSE)
             )
      )
    
    return(interactive_p)
  }