#' Code by Will Chase
#' https://www.williamrchase.com/post/flow-fields-12-months-of-art-september/
#' 
#' Adaptations made:
#'   * Simplex noise instead of Perlin noise
#'   * Color palette
#'   * Added title in plot

library(tidyverse)
library(ambient)
library(particles)
library(tidygraph)
library(ggtext)

seed <- sample(1:2000, 1)
seed <- 1331


grid <-
  long_grid(x = seq(0, 10, length.out = 1000),
            y = seq(0, 10, length.out = 1000)) %>%
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 2, seed = seed),
    y1 = y + gen_perlin(x = x, y = y, frequency = 0.5, seed = seed)
  )

curl <- curl_noise(gen_simplex, seed = seed, x = grid$x1, y = grid$y1)

grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)

field <- as.matrix(grid, x, value = angle)

sim <- create_ring(10000) %>%
  simulate(alpha_decay = 0, setup = petridish_genesis(vel_max = 0, max_radius = 1)) %>%
  wield(reset_force, xvel = 0, yvel = 0) %>%
  wield(field_force, angle = field, vel = 0.15, xlim = c(-50, 40), ylim = c(-50, 40)) %>%
  evolve(100, record)

traces <- data.frame(do.call(rbind, lapply(sim$history, position)))
names(traces) <- c("x", "y")
traces$particle <- rep(1:10000, 100)

bl_yl <- c("#fcba03", "#32a852", "#4287f5")
bl_yl_bg <- "#ebe6da"

traces2 <-
  traces %>%
  group_by(particle) %>%
  mutate(color = sample(bl_yl, 1, replace = TRUE))

ggplot(traces2) +
  geom_path(aes(x, y, group = particle, color = color), size = 0.06, alpha = 0.6) +
  annotate("richtext", x = 1.15, y = 0.67,
           label = "<span style='font-size:24pt;color:#4287f5'>THE NEXT NEXT</span><br>FIDENZA",
           color = "#32a852", label.colour = NA, fill = NA, size = 16, hjust = 0,
           lineheight = 0.8,
           family = "Bebas Neue") + 
  scale_color_identity(guide = "none") +
  theme_void() +
  theme(legend.position = "none", panel.background = element_rect(fill = bl_yl_bg))
ggsave(here::here("plots", "day04.png"), dpi = 600, width = 5, height = 4)
