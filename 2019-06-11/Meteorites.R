# Tidy Tuesday | Week 24
# Meteorites!
# Source: "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-06-11"

library(tidyverse)
library(gganimate)

# Read data ---------------------------------------------------------------

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

world <- map_data("world")

# Prepare data ------------------------------------------------------------

meteo <- meteorites %>%
  drop_na() %>% 
  filter(mass > 0, year > 1500) %>% 
  mutate(year = if_else(name == "Northwest Africa 7701", 2010, year),
         year = as.integer(year),
         calc_mass = if_else(mass > 10000, 10000, mass))

fallen <- meteo %>% filter(fall == "Fell")
found <- meteo %>% filter(fall == "Found")


# trial -------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "#e6e6e9",
    size = 0.1
  ) +
  geom_point(
    data = fallen,
    aes(
      x = long,
      y = lat,
      size = calc_mass,
      color = calc_mass
    ),
    alpha = 0.5
  ) +
  scale_color_distiller(
    palette = "Reds",
    direction = 1,
    labels = c("0", "2.5", "5.0", "7.5", "10.0+"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  geom_point(
    data = found,
    aes(
      x = long,
      y = lat,
      size = calc_mass,
      fill = calc_mass
    ),
    shape = 21,
    alpha = 0.5
  ) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    labels = c("0", "2.5", "5.0", "7.5", "10.0+"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  scale_size(guide = "none") +
  annotate(
    "text",
    x = -134,
    y = -30,
    family = "B612 Mono",
    label = "Year: {frame_time}",
    size = 7,
    color = "gray50",
    fontface = "bold"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.15, 0.1),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    text = element_text(family = "B612 Mono")
  ) +
  labs(
    color = "Fallen mass in kilograms",
    fill = "Found mass in kilograms",
    title = "Fallen and found meteorites on Earth",
    caption = "Source: NASA"
  ) +
  coord_map("mollweide", orientation = c(90, 0, 0))


# Create static plot ------------------------------------------------------

map <- ggplot() + 
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               fill = "#e6e6e9",
               size = 0.1) +
  geom_point(data = fallen,
             aes(x = long, y = lat, size = calc_mass, color = calc_mass),
             alpha = 0.5) +
  scale_color_distiller(palette = "Reds",
                        direction = 1,
                        labels = c("1", "2,500", "5,000", "7,500", "10,000+"),
                        guide = guide_colorbar(
                          direction = "horizontal",
                          barheight = unit(2, units = "mm"),
                          barwidth = unit(50, units = "mm"),
                          title.position = "top",
                          title.hjust = 0.5,
                          label.hjust = 0.5
                        )) +
  geom_point(data = found,
             aes(x = long, y = lat, size = calc_mass, fill = calc_mass),
             shape = 21,
             alpha = 0.5) +
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       labels = c("1", "2,500", "5,000", "7,500", "10,000+"),
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         title.position = "top",
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       )) +
  scale_size(guide = "none") +
  labs(color = "Fallen mass in grams",
       fill = "Found mass in grams",
       title = "Meteorites on Earth",
       subtitle = "Year: {frame_time}",
       caption = "Source: NASA") +
  theme_void() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#000019"),
        plot.title = element_text(hjust = 0.5, family = "Open Sans"),
        plot.subtitle = element_text(hjust = 0.5, family = "Open Sans")) +
  coord_map("mollweide", orientation = c(90, 0, 0))


# Create GIF --------------------------------------------------------------

map_gif <- map +
  transition_events(start = year,
                    end = year + 5L,
                    enter_length = 6L,
                    exit_length = 4L) +
  enter_grow() +
  exit_fade()

animate(map_gif, nframes = 160, duration = 40, height = 1000, width = 1000)

anim_save("2019-06-11/Meteorites.gif")
