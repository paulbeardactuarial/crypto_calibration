

market_data |> plot_open_over_time()

market_data |> plot_empirical_returns()



ggplot2::theme_set(theme_void())

girafe(ggobj = p,
       options = 
         list(
           opts_hover(css =   "filter: brightness(120%);
                      transition: all 0.8s ease;"),
           opts_hover_inv(css = "opacity:0.1;transition: all 0.8s ease;"),
           opts_sizing(rescale = FALSE)
         ))
