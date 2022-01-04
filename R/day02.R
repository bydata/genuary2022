
#' Code adapted from 
#' https://github.com/gkaramanis/aRtist/blob/main/genuary/2022/2022-02/2022-02.R
#' https://coolbutuseless.github.io/2018/02/05/dithr-package-for-error-diffusion-dithering-in-r/

# Install required packages
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("EBImage")
# devtools::install_github('coolbutuseless/dithr')

pacman::p_load("tidyverse", "imager", "here", "glue", "magick", "dithr")

# img_path <- "~/Documents/Repositories/30DayMapChallenge-2021/plots/day28_earth-not-flat-with_lines.png"

#' Image source:
#' https://de.wikipedia.org/wiki/K%C3%B6lner_Dom#/media/Datei:K%C3%B6lner_Dom_und_Hohenzollernbr%C3%BCcke_Abendd%C3%A4mmerung_(9706_7_8).jpg
#' Raimond Spekking / CC BY-SA 4.0
img_path <- here("plots", "1094px-Kölner_Dom_und_Hohenzollernbrücke_Abenddämmerung_(9706_7_8).jpg")

original_image <- image_read(img_path) 

grayscale_image <- original_image %>%
  image_convert(type = "grayscale")

plot(grayscale_image)

image_info(grayscale_image)
width <- image_info(grayscale_image)$width
height <- image_info(grayscale_image)$height

m <- as_EBImage(grayscale_image)@.Data

stages <- 10
plot_dir <- here("plots", "day02")
for (i in seq_len(stages)) {
  i_fmt <- str_pad(i, 2, side = "left", pad = "0")
  dithered_matrix <- dither(m, edm = diffusion_matrix$two_row_sierra, threshold = i / stages)
  ragg::agg_png(here(plot_dir, glue("day02-{i_fmt}.png")), 
                width = width, height = height, res = 200)
  plot_matrix(dithered_matrix)
  invisible(dev.off())
}

# Morphing -----------------------

dithered_image <- image_read(here(plot_dir, "day02-03.png"))

images <- c(original_image, grayscale_image, dithered_image)

morphed <- image_morph(images, frames = 12)
image_write(morphed, path = here("plots", "day02-animated-morphed.gif"), 
            format = "gif", quality = 100, density = 100)


