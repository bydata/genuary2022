pacman::p_load("dplyr", "ggplot2", "tidyr", "magick")


spaceship <- matrix(
  c(
    0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1,
    1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1,
    0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0
    ),
  byrow = TRUE,
  ncol = 11
)


draw_spaceship <- function(spaceship, color = "#06EFED", n_stars = 500) {
  
  stars <- data.frame(
    x = runif(n_stars, -10, 21),
    y = runif(n_stars, -10, 10),
    alpha = rnorm(n_stars)
  )
  
  as_tibble(spaceship) %>%
    mutate(y = row_number()) %>% 
    pivot_longer(cols = V1:V11, names_to = "x", values_to = "filled",
                 names_transform = list(x = function(x) as.integer(gsub("V", "", x)))) %>% 
    ggplot(aes(x, y)) +
    geom_point(data = stars,
               aes(x, y, alpha = alpha), color = "white", size = 0.2) + 
    geom_tile(aes(fill = factor(filled)), show.legend = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(expand = c(0, 0)) +
    scale_fill_manual(values = c("grey1", color)) +
    scale_alpha_identity() +
    coord_equal(xlim = c(-10, 21), ylim = c(10, -10), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(color = NA, fill = "grey1"),
          plot.margin = margin(0, 0, 0, 0))
}


spaceship_colors <- c("#06EFED", "#FCFF68", "#3EFD58", "#7E90F4")

i <- 1
for (color in rep(spaceship_colors, 4)[order(rep(spaceship_colors, 4))]) {
  p <- draw_spaceship(spaceship, color)
  ggsave(here::here("plots", "day03", glue::glue("day03-spaceship-{i}.png")),
         dpi = 150, width = 5, height = 3.3)
  i <- i + 1
}

filenames <- here::here("plots", "day03", glue::glue("day03-spaceship-{1:16}.png"))
images <- purrr::map(filenames, image_read) %>% 
  purrr::reduce(c)

animated <- image_animate(images, optimize = TRUE)
image_write(animated, here::here("plots", "day03_animated.gif"),
            format = "gif", quality = 80, density = 150)
