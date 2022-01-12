library(ggplot)
library(dyplr)

# generate random sizes for squares
a <- rnorm(1000, 10)
hist(a)

data.frame(a = a) %>% 
  arrange(-a) %>% 
  ggplot() +
  geom_tile(aes(x = 1, y = 1, width = a, height = a, fill = abs(a)),
            alpha = 0.2, col = "black", show.legend = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "grey80"))
ggsave(here::here("plots", "day12.png"), dpi = 600, width = 4, height = 4)
