library(tidyverse)
library(gganimate)

# Create world map --------------------------------------------------------

world <- map_data("world")
map <- ggplot() + 
  geom_polygon(data = world,
                    aes(x = long, y = lat, group = group),
                    fill = "#e6e6e9",
                    size = 0.1) +
  coord_map("mollweide", orientation = c(90, 0, 0)) +
  theme_void()

# Read data ---------------------------------------------------------------


meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

recent <- meteorites %>% 
  filter(long != max(long, na.rm = T) & year < 2020 & !is.na(mass)) %>% 
  filter(year > 1900) %>% 
  mutate(masskg = mass/1000) %>% 
  mutate(masskg = ifelse(masskg > 5000, 5000, masskg))

# Plot metoerite falls ----------------------------------------------------

quartz()

map +
  geom_point(data = recent,
             aes(x = long, y = lat, size = masskg),
             color = "orange",
             alpha = 0.5) +
  labs(size = "Mass (kg)",
       caption = "Data: Meteoritical Society, via NASA: tinyurl.com/yxkrhykk",
       title = "Meteorites found on Earth",
       subtitle = "Year found: {frame_time}") +
  theme(legend.position = c(0.15, 0.36),
        panel.background = element_rect(fill = "#000019")) +
  scale_size(labels = c("0", "1000", "2000", "3000", "4000", "5000+"),
             guide = "legend") +
  transition_events(start = year, end = year + 5L, enter_length = 6, exit_length = 4) +
  enter_grow() +
  exit_fade()

anim_save("Meteorites.gif")


# Looking data ------------------------------------------------------------

meteorites %>% 
  ggplot(aes(x = mass)) +
  geom_histogram() +
  scale_x_log10()

quantile(meteorites$mass, na.rm = T)

meteorites %>% 
  ggplot(aes(x = mass)) +
  geom_boxplot()

meteorites %>% 
  filter(fall == "Fell") %>% 
  select(year, mass) %>% 
  arrange(-mass)





