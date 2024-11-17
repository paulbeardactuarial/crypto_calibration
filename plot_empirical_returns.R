
plot_empirical_returns <-
function(market_data) {
p1 <-
  market_data |> 
  ggplot(mapping = aes(x = date, y = log_returns, color = crypto, data_id = crypto)) +
  geom_line_interactive(size = 1) +
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
  scale_x_date(breaks = "2 years", name = "Date") +
  scale_y_continuous(name = "Annual Log Returns", labels = scales::label_percent()) +
  scale_color_manual(values = crypto_colours) +
  coord_cartesian(xlim = as.Date(c(NA, "2025-01-01")))


p2 <-
  market_data |> 
  ggplot(mapping = aes(x = log_returns, fill = crypto, data_id = crypto)) +
  geom_histogram_interactive(size = 1, bins = 10) +
  geom_text_interactive(
    data = market_data |> slice_max(by = crypto, n = 1, order_by = date),
    mapping = aes(label = crypto, color = crypto, x = 0, y= 0),
    hjust = 0.5,
    vjust = 1,
    size = 4,
    nudge_y = -1,
    fontface = "bold"
  ) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans")) +
  scale_fill_manual(values = crypto_colours) +
  scale_color_manual(values = crypto_colours) +
  scale_y_continuous(name = "") +
  scale_x_continuous(labels = scales::label_percent(), name = "") +
  facet_wrap(vars(crypto), nrow = 1) +
  theme(  strip.text = element_blank(),
          strip.background = element_blank(),
          axis.text.y = element_blank())



patchwork_plot <- p1/p2
girafe(ggobj = patchwork_plot, 
       options = list(
         opts_hover(css = ""),
         opts_hover_inv(css = "opacity: 0;"),
         opts_sizing(rescale = FALSE)
       )
)

}
