
library(ggplot2)
library(tidyverse)
library(ggiraph)
library(patchwork)

data_folder <- "./Data"
crypto_names <- dir(data_folder, full.names = FALSE) |> str_remove(" Historical Data.csv")

tn_folder <- "./Thumbnails"

dir(tn_folder, full.names = TRUE)

source("./plot_open_over_time.R")
source("./plot_empirical_returns.R")

crypto_colours = c("BTC" = "#f7931a", "ETH" = "#116797", "XRP" = "#2f2c56", "Dogecoin" = "#987F50")
crypto_levels <-names(crypto_colours)

market_data <-
dir(data_folder, full.names = TRUE) |> 
  map(\(x) read_csv(x, col_types = c(Date = "c", Open = "n", `Vol.` = "c"))) |> 
  setNames(crypto_names) |> 
  map(\(x) select(x, Date, Open, Vol.)) |> 
  bind_rows(.id = "crypto") |> 
  mutate(Date = as.Date(Date, tryFormats = c("%m/%d/%Y"))) |> 
  janitor::clean_names() |> 
  filter(crypto != "BNB") |> 
  dplyr::mutate(crypto = factor(crypto, levels = crypto_levels)) |> 
  dplyr::filter(date <= as.Date("2023-05-01")) |> 
  arrange(crypto, date) |> 
  mutate(simple_returns_raw = dplyr::if_else(crypto == lag(crypto, 12), (open / lag(open, 12)) - 1, NA)) |> 
  mutate(simple_returns_neutral = (1 + simple_returns_raw) / mean(1 + simple_returns_raw, na.rm = TRUE) - 1, .by = crypto) |> 
  mutate(log_returns = log(1+simple_returns_neutral))

market_data |> plot_open_over_time()

market_data |> plot_empirical_returns()

p <-
market_data |> 
  ggplot(mapping = aes(x = crypto, y=log_returns, fill=crypto, color = crypto, data_id = crypto)) +
  geom_violin_interactive() +
  scale_fill_manual(values = crypto_colours) +
scale_color_manual(values = crypto_colours)


girafe(ggobj = p,
       options = 
         list(
           opts_hover(css = ""),
           opts_hover_inv(css = "opacity:0.1;"),
           opts_sizing(rescale = FALSE)
         ))
