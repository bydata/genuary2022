pacman::p_load("tidyverse")

#' WALL DRAWING 289
#' 
#' https://massmoca.org/event/walldrawing289/
#' 
#' A 6-inch (15 cm) grid covering each of the four black walls. 
#' White lines to points on the grids. 
#' Fourth wall: twenty-four lines from the center, twelve lines from the midpoint 
#' of each of the sides, twelve lines from each corner. 
#' (The length of the lines and their placement are determined by the drafter.) 
#' (Detail: 4th wall only)
#' July 1976
#' White crayon lines and black pencil grid on black wall
#' Whitney Museum of American Art, New York, Purchase with funds from the Gilman Foundation, Inc. 78.1.1-4

24 + 4 * 12 + 4 * 12

grid_size <- 1
dots_horizontally <- 15
dots_vertically <- 8

x <- seq(grid_size, dots_horizontally * grid_size, grid_size) 
y <- seq(grid_size, dots_vertically * grid_size, grid_size)
grid <- expand.grid(x = x, y = y) 

center <- c(x= grid_size * (dots_horizontally + 1)  / 2,
            y = grid_size * (dots_vertically + 1)  / 2)

corners <- list(
  c(0, 0),
  c(0, dots_vertically + grid_size),
  c(dots_horizontally + grid_size, 0),
  c(dots_horizontally + grid_size, dots_vertically + grid_size)
  )
corners <- map(corners, ~set_names(.x, c("x", "y"))) %>% 
  bind_rows()


midpoints <- list(
  c(0, center["y"]),
  c(dots_horizontally + grid_size, center["y"]),
  c(center["x"], 0),
  c(center["x"], dots_vertically + grid_size)
)
midpoints <- map(midpoints, ~set_names(.x, c("x", "y"))) %>% 
  bind_rows()

grid %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  geom_point(data = corners,
             col = "red", size = 3) +
  geom_point(data = midpoints,
             col = "green", size = 3) +
  geom_point(data = tibble(x = center["x"], y = center["y"]),
             col = "blue", size = 3) +
  coord_equal(xlim = c(0, (dots_horizontally + 1) * grid_size), 
              ylim = c(0, (dots_vertically + 1) * grid_size), 
              expand = FALSE) +
  theme_bw()


# Create a tibble with the coordinates of the center, corners, and midpoints and 
# expand it by the number of lines to be drawn (120)
segment_starts <- bind_rows(
  tibble(corners, rep = 12), 
  tibble(x = center["x"], y = center["y"], rep = 24), 
  tibble(midpoints, rep = 12)
)
segment_starts <- segment_starts %>% uncount(rep) 

draw_lines <- function(seed = NULL) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  # shuffle the rows
  segment_starts <- segment_starts[sample(seq_len(nrow(segment_starts)), size = 120, replace = FALSE), ]
  # combine
  lines <- segment_starts %>% 
    bind_cols(data.frame(xend = grid$x, yend = grid$y)) 
  lines
}

draw_lines() %>% 
  ggplot() +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               col = "white", alpha = 0.99, size = 0.38) +
  coord_equal(xlim = c(0, (dots_horizontally + 1) * grid_size), 
              ylim = c(0, (dots_vertically + 1) * grid_size), 
              expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "black"),
        plot.margin = margin(0, 0, 0, 0))
ggsave(here("plots", "day07.png"), dpi = 300, width = 5, height = 5 * 9/16 )


            