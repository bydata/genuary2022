pacman::p_load("tidyverse", "here", "ggforce")

max_value <- 10000
plot_margin <- 12

df <- tibble(
  x = runif(10000, 1, max_value),
  y = runif(10000, 1, max_value),
  r = rnorm(10000)
)

df %>% 
  ggplot(aes(x, y)) +
  geom_voronoi_tile(aes(fill = r), show.legend = FALSE) +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("Ingres")) +
  geom_voronoi_segment(color = "white", size = 0.1) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_equal(xlim = c(0, max_value), ylim = c(0, max_value), expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(t = plot_margin, l = plot_margin, b = plot_margin, r = plot_margin),
        plot.background = element_rect(color = "grey89", fill = "grey94"))
ggsave(here("plots", "day01-voronoi.png"), dpi = 600, width = 4, height = 4)
