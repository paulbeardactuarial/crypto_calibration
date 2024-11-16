
library(ggplot2)
library(tidyverse)
library(ggiraph)

data_folder <- "./Data"
crypto_names <- dir(data_folder, full.names = FALSE) |> str_remove(" Historical Data.csv")

tn_folder <- "./Thumbnails"

dir(tn_folder, full.names = TRUE)


market_data <-
dir(data_folder, full.names = TRUE) |> 
  map(\(x) read_csv(x, col_types = c(Date = "c", Open = "n", `Vol.` = "c"))) |> 
  setNames(crypto_names) |> 
  map(\(x) select(x, Date, Open, Vol.)) |> 
  bind_rows(.id = "crypto") |> 
  mutate(Date = as.Date(Date, tryFormats = c("%m/%d/%Y"))) |> 
  janitor::clean_names()
                 
p <-
market_data |> 
  ggplot(aes(x = date, y = open, color = crypto, data_id = date)) +
  geom_segment_interactive(
    aes(xend = date , y = 0.001, yend = 1000000),
    color = "white",
    alpha = 1
  ) +
  geom_text_interactive(
    aes(y = open * 2.2, 
        label = scales::dollar(open, accuracy = 0.01)
        ),
    color = "white",
    hover_nearest = TRUE,
    size = 3,
    alpha = 1
  ) +
  geom_line(size = 1) +
  scale_x_date(breaks = "2 years") +
  scale_y_log10(breaks = 10^seq(-2, 6, by=1), 
                label = scales::label_currency(accuracy = 0.01)) +
  geom_text(
    data = market_data |> slice_max(by = crypto, n = 1, order_by = date),
    mapping = aes(label = crypto),
    hjust = 0,
    size = 4,
    nudge_x = 60,
    fontface = "bold"
    ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(xlim = as.Date(c(NA, "2028-01-01")), ylim = c(NA, 100000))


girafe(ggobj = p, 
       options = list(
         opts_hover(css = "fill:black;"),
         opts_hover_inv(css = "opacity: 0;"),
         opts_sizing(rescale = FALSE)
         )
       )


