# Tidytuesday week 33

# Roman Emperors

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(emoGG)
library(ggthemes)

# Get and prepare data ----------------------------------------------------

emperors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

imperatorum <- emperors %>% 
  mutate(
    NOMINE = str_replace_all(toupper(name), "U", "V"),
    ANNUS_NATALIS = as.integer(year(birth)),
    ANNO_MORTIS = as.integer(year(death)),
    PRIMO_REGNI = as.integer(year(reign_start)),
    FINE_REGNABUNT = as.integer(year(reign_end))
  ) %>%
  mutate(
    ANNUS_NATALIS = if_else(
      name %in% c("Augustus", "Tiberius", "Claudius"),-ANNUS_NATALIS, ANNUS_NATALIS
    ),
    PRIMO_REGNI = if_else(name == "Augustus",-PRIMO_REGNI, PRIMO_REGNI)
  ) %>% 
  mutate(
    AETATE = if_else(
      name %in% c("Augustus", "Tiberius", "Claudius"),
      -time_length(ymd("0000-1-1") - birth, unit = "years") -1 +
        time_length(death - ymd("0000-1-1"), unit = "years"),
      time_length(death - birth, unit = "year")
    ),
    REGNABIT_DURATIONEM = if_else(
      name == "Augustus",
      -time_length(ymd("0000-1-1") - reign_start, unit = "years") -1 +
        time_length(reign_end - ymd("0000-1-1"), unit = "years"),
      time_length(reign_end - reign_start, unit = "year")
    )
  ) %>% 
  mutate(DOMUS = dynasty) %>% 
  select(index, NOMINE:DOMUS)

index <- emperors %>% 
  group_by(dynasty) %>% 
  summarise(min = min(index),
            max = max(index)) %>% 
  ungroup() %>% 
  mutate(middle = (min + max)/2,
         ymin = min - .5,
         ymax = max + .5) %>% 
  arrange(min)

ggplot(
  imperatorum,
  aes(
    x = ANNUS_NATALIS,
    xend = ANNO_MORTIS,
    y = fct_reorder(NOMINE, index),
    yend = fct_reorder(NOMINE, index)
  )
) +
  geom_segment(size = 2, lineend = "round", alpha = .2) +
  geom_segment(
    aes(x = PRIMO_REGNI, xend = FINE_REGNABUNT),
    color = "#073642",
    size = 1,
    lineend = "round"
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = index$ymax[1],
    alpha = 0.2,
    fill = "#80dbb9"
  ) +
  annotate(
    "text",
    x = 400,
    y = index$middle[1],
    label = index$dynasty[1],
    family = "Inknut Antiqua",
    hjust = 1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[2],
    ymax = index$ymax[2],
    alpha = 0.2,
    fill = "#c6e299"
  ) +
  annotate(
    "text",
    x = 400,
    y = index$middle[2],
    label = index$dynasty[2],
    family = "Inknut Antiqua",
    hjust = 1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[3],
    ymax = index$ymax[3],
    alpha = 0.2,
    fill = "#ffb483"
  ) +
  annotate(
    "text",
    x = 400,
    y = index$middle[3],
    label = index$dynasty[3],
    family = "Inknut Antiqua",
    hjust = 1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[4],
    ymax = index$ymax[4],
    alpha = 0.2,
    fill = "#ff7a72"
  ) +
  annotate(
    "text",
    x = 400,
    y = index$middle[4],
    label = index$dynasty[4],
    family = "Inknut Antiqua",
    hjust = 1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[5],
    ymax = index$ymax[5],
    alpha = 0.2,
    fill = "#cd2660"
  ) +
  annotate(
    "text",
    x = 400,
    y = index$middle[5],
    label = index$dynasty[5],
    family = "Inknut Antiqua",
    hjust = 1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[6],
    ymax = index$ymax[6],
    alpha = 0.2,
    fill = "#cfda42"
  ) +
  annotate(
    "text",
    x = -100,
    y = index$middle[6],
    label = index$dynasty[6],
    family = "Inknut Antiqua",
    hjust = 0
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[7],
    ymax = index$ymax[7],
    alpha = 0.2,
    fill = "#84c1ca"
  ) +
  annotate(
    "text",
    x = -100,
    y = index$middle[7],
    label = index$dynasty[7],
    family = "Inknut Antiqua",
    hjust = 0
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = index$ymin[8],
    ymax = Inf,
    alpha = 0.2,
    fill = "#467582"
  ) +
  annotate(
    "text",
    x = -100,
    y = index$middle[8],
    label = index$dynasty[8],
    family = "Inknut Antiqua",
    hjust = 0
  ) +
  labs(x = "Year",
       title = "Roman Emperors: \nRise and Fall",
       caption = "#TidyTuesday") +
  theme_solarized(base_size = 10, base_family = "Inknut Antiqua") +
  theme(
    axis.text.y = element_text(family = "Inknut Antiqua Light", size = 6),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 14)
  ) +
  coord_cartesian(xlim = c(-100, 400))

ggsave("2019-08-14/emperors.png", width = 5, height = 15)










