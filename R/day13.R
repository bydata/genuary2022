pacman::p_load("tidyverse", "here", "gganimate")


max_value <- 100
plot_margin <- 0
n <- 200

random_data <- function(n, max_value) {
  tibble(
    x = runif(n, 0, max_value),
    y = runif(n, 0, max_value),
    r = rnorm(n)
  )
}

df <- bind_rows(random_data(n, max_value), 
                random_data(n, max_value), 
                random_data(n, max_value), 
                random_data(n, max_value), 
                random_data(n, max_value), 
                .id = "frame")

# ragg::agg_png(here("plots", "day13.png"), res = 72, width = 800, height = 80)
p <- df %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(fill = r, size = r), shape = 21, color = "white", alpha = 0.9, 
             stroke = 0.1, show.legend = FALSE) +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("Ingres")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_size_area(max_size = 10) +
  coord_cartesian(xlim = c(0, max_value), ylim = c(0, max_value), expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(t = plot_margin, l = plot_margin, b = plot_margin, r = plot_margin),
        plot.background = element_rect(color = "grey89", fill = "grey94")) +
  transition_states(frame)
animate(p, res = 200, width = 800, height = 80, duration = 20, fps = 20, rewind = TRUE)
anim_save(here("plots", "day13.gif"))  

# invisible(dev.off())
