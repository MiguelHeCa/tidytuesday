library(mathart)
library(ggart)
library(ggforce)
library(Rcpp)
library(tidyverse)

# Art ---------------------------------------------------------------------

set.seed(14)
df <- lissajous(a = runif(1, 0, 2), b = runif(1, 0, 2), A = runif(1, 0, 2), B = runif(1, 0, 2), d = 200) %>%
  sample_n(1001) %>%
  k_nearest_neighbour_graph(40)

p <- ggplot() +
  geom_segment(aes(x = y, y = x, xend = yend, yend = xend, colour = dist), df, size = 0.03) +
  scale_color_viridis_c(option = "cividis") +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0)

ggsave("knn_lissajous_014.png", p, width = 25, height = 25, units = "cm")
