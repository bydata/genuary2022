pacman::p_load("ggplot2", "dplyr", "ggtext")

# states <- setdiff(state.name, c("Alaska", "Hawaii"))
states <- "Texas"
counties <- tigris::counties(states, cb = TRUE, resolution = "5m")

df <- bind_cols(
  counties,
  value = runif(nrow(counties), 0, 1)
)

wilke_quote <- "
\"[The rainbow scale] runs through all possible colors in the color spectrum. 
This means the scale is effectively circular; the colors at the beginning and 
the end are nearly the same (dark red). 
If these two colors end up next to each other in a plot, 
we do not instinctively perceive them as representing data values that are maximally apart.
In addition, the scale is highly non-monotonic. It has regions where colors 
change very slowly and others when colors change rapidly.\"
<span style='color:grey34'>(Wilke, 2019: *Fundamentals of Data Visualization*)</span>
"

ggplot(df) +
  geom_sf(aes(fill = value), 
          color = "white", size = 0.1) +
  scale_fill_gradientn(colors = rainbow(20), labels = scales::percent_format()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Introducing the Rainbow Color Scale",
       subtitle = wilke_quote,
       fill = "Random % value") +
  cowplot::theme_map(font_family = "Lato", font_size = 8) +
  theme(
    legend.position = c(0.01, 0.15),
    legend.direction = "horizontal",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.width = unit(7, "mm"),
    legend.key.height = unit(2.5, "mm"),
    plot.background = element_rect(color = NA, fill = "grey89"),
    plot.subtitle = ggtext::element_textbox_simple(
      margin = margin(t = 8)
    )
  )
ggsave(here::here("plots", "day16.png"), dpi = 300, width = 4, height = 4)
