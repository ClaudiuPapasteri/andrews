library(ggplot2)
library(cowplot)
library(magick)
library(here)


logo_file <- here::here("add_image", "logo.png") %>%
  magick::image_read() %>%
  magick::image_resize("400x400")   

my_plot <- 
  ggplot(iris, aes(x = Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7)

my_plot_2 <- 
  cowplot::ggdraw() +
  cowplot::draw_plot(my_plot) +
  cowplot::draw_image(logo_file,  x = 0.3, y = 0.4, scale = 0.2) 
  
my_plot_2

