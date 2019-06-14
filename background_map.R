
# Tidy Tuesday | Week 24
# Meteorites!
# Source: "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-06-11"

library(tidyverse)
library(ggmap)

# Read data ---------------------------------------------------------------

world <- map_data("world")

# Prepare data ------------------------------------------------------------


# Create static plot ------------------------------------------------------

map_colors <-  c("#d5d5d5", "#fdfdfd", "#1b1b1b")

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = map_colors[1],
    color = map_colors[2],
    size = .2
  ) +
  theme_void()  +
  theme(
    panel.background = element_rect(fill = map_colors[3])
  ) +
  coord_map("mollweide", orientation = c(90, 0, 0))


