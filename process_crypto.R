
library(ggplot2)
library(tidyverse)

data_folder <- "./Data"
crypto_names <- dir(data_folder, full.names = FALSE) |> str_remove(" Historical Data.csv")


market_data <-
dir(data_folder, full.names = TRUE) |> 
  map(\(x) read_csv(x, col_types = c(Date = "c", Open = "n", `Vol.` = "c"))) |> 
  setNames(crypto_names) |> 
  map(\(x) select(x, Date, Open, Vol.)) |> 
  bind_rows(.id = "crypto") |> 
  mutate(Date = as.Date(Date, tryFormats = c("%m/%d/%Y"))) |> 
  janitor::clean_names()
                 

  
market_data |> 
  ggplot(aes(x = date, y = open, color = crypto)) +
  geom_line(size = 1) +
  scale_x_date(breaks = "2 years") +
  scale_y_log10(breaks = 10^seq(-2, 6, by=1), 
                label = scales::label_dollar(accuracy = 0.01)) +
  geom_text(
    data = market_data |> slice_max(by = crypto, n = 1, order_by = date),
    mapping = aes(label = crypto),
    hjust = -.5
    ) +
  ggthemes::theme_pander() +
  theme(legend.position = "none")
  


