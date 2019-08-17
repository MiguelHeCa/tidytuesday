# Tidytuesday week 33

# Roman Emperors

# References --------------------------------------------------------------

# Inspired in https://www.pmassicotte.com/post/tidytuesday-2019-08-13/
# Thanks to https://github.com/rfordatascience/tidytuesday

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tvthemes)

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
  mutate(DOMUS = dynasty,
         INCOGNITA = if_else(is.na(ANNUS_NATALIS), ANNO_MORTIS - 35, NA_real_),
         NOMINE_ET_DIEM = if_else(
           !is.na(INCOGNITA),
           glue::glue("{NOMINE} (???, {round(REGNABIT_DURATIONEM, digits = 1)}) "),
           glue::glue("{NOMINE} ({round(AETATE)}, {round(REGNABIT_DURATIONEM, digits = 1)}) "),
         )) %>% 
  select(index, NOMINE:NOMINE_ET_DIEM) %>% 
  arrange(desc(index))

index_domus <- emperors %>% 
  mutate(index = 69 - index) %>% 
  group_by(dynasty) %>% 
  summarise(min = min(index),
            max = max(index)) %>% 
  ungroup() %>% 
  mutate(dynasty = case_when(
    dynasty == "Nerva-Antonine" ~ "Nerva-\nAntonine",
    TRUE ~ dynasty
  )) %>% 
  mutate(middle = (min + max)/2,
         ymin = min - .5,
         ymax = max + .5) %>% 
  arrange(min)

figure <- ggplot(
  imperatorum,
  aes(
    x = ANNUS_NATALIS,
    xend = ANNO_MORTIS,
    y = fct_reorder(NOMINE, -index),
    yend = fct_reorder(NOMINE, -index)
  )
) +
  geom_segment(aes(color = "Age"), size = 1, alpha = .5) +
  geom_segment(aes(x = INCOGNITA, xend = ANNO_MORTIS, color = "Age unkown"), size = 1, alpha = .5) +
  geom_segment(
    aes(x = PRIMO_REGNI, xend = FINE_REGNABUNT, color = "Reign"),
    size = 1.5,
    lineend = "square"
  ) +
  geom_text(aes(x = ANNUS_NATALIS, label = NOMINE_ET_DIEM),
            hjust = 1,
            family = "Inknut Antiqua",
            size = 2.5) +
  geom_text(aes(x = INCOGNITA, label = NOMINE_ET_DIEM),
            hjust = 1,
            family = "Inknut Antiqua",
            size = 2.5) +
  scale_color_manual(values = c("#449f8b", "#ecb100", "#9f4458")) +
  scale_x_continuous(sec.axis = dup_axis(), name = "Year", breaks = c(-100, 0, 100, 200, 300, 400)) +
  theme_theLastAirbender(text.font = "Inknut Antiqua", ) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x.bottom = element_blank(),
    panel.background = element_rect(colour = "black"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key = element_rect(fill = "#ece5d3", size = 0.5, colour = "#ece5d3"),
    legend.background = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  labs(
    title = "The Chronicles of the Caesars",
    caption = "#TidyTuesday | MywellHernan"
  ) +
  coord_cartesian(xlim = c(-170, 400), ylim = c(-1, nrow(imperatorum) + 2))

figure <- figure +
  annotate("text", label = "Caesar", x = -150, y = 70, size = 3, family = "Inknut Antiqua") +
  geom_curve(aes(x = -150, y = 69.5, xend = -150, yend = 68.5), arrow = arrow(angle = 45, length = unit(0.1, "inches")), curvature = 0.2, size = 0.3, colour = "#beafae") +
  annotate("text", label = "Age", x = -95, y = 70, size = 3, family = "Inknut Antiqua") +
  geom_curve(aes(x = -95, y = 69.5, xend = -100, yend = 68.5), arrow = arrow(angle = 45, length = unit(0.1, "inches")), curvature = 0.2, size = 0.3, colour = "#beafae") +
  annotate("text", label = "Reign duration", x = -30, y = 70, size = 3, family = "Inknut Antiqua") +
  geom_curve(aes(x = -60, y = 69.5, xend = -76, yend = 68.5), arrow = arrow(angle = 45, length = unit(0.1, "inches")), curvature = 0.3, size = 0.3, colour = "#beafae") +
  annotate("text", label = "The first emperor: Pax romana begins", x = 30, y = 68, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Birth of Christianity", x = 50, y = 67, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Great Fire of Rome: Christianity becomes illegal", x = 80, y = 64, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Colosseum is built", x = 100, y = 60, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Pompeii is destroyed", x = 100, y = 59, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Wall erected in Britannia", x = 160, y = 55, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Wall erected in Caledonia", x = 190, y = 54, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Pax romana ends", x = 210, y = 51, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Empire's maximum extension", x = 230, y = 48, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Granted citizenship for all free men", x = -145, y = 47, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Beginning of the Imperial crisis", x = -140, y = 43, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Year of the Six Emperors begins", x = -140, y = 42, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Captured by the Sassanid Empire", x = -120, y = 31, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Uprise of the Gallic Roman Empire", x = -110, y = 30, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Uprise of the Palmynere Empire", x = -150, y = 29, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Reconquest of the rebel Empires", x = -90, y = 27, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Administration divided in East and West,\nChristians no longer persecuted", x = 110, y = 20, size = 3, family = "Inknut Antiqua", hjust = 1, vjust = .8) +
  annotate("text", label = "Edict of Milan: Christianity is legalized", x = -160, y = 15, size = 3, family = "Inknut Antiqua", hjust = 0) +
  annotate("text", label = "Co-ruling: Valentinian I in Rome,\nValens in Constantinople", x = -20, y = 5, size = 3, family = "Inknut Antiqua", hjust = 0, vjust = .8) +
  annotate("text", label = "Christianity becomes the state religion,\nEmpire divided: Birth of Western and Eastern Roman Empires", x = -50, y = 1, size = 3, family = "Inknut Antiqua", hjust = 0, vjust = .8) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index_domus$ymin[7], ymax = index_domus$ymax[7]), color = "black", alpha = 0, size = 0.1) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index_domus$ymin[6], ymax = index_domus$ymax[6]), color = "black", alpha = 0, size = 0.1) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index_domus$ymin[5], ymax = index_domus$ymax[5]), color = "black", alpha = 0, size = 0.1) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index_domus$ymin[4], ymax = index_domus$ymax[4]), color = "black", alpha = 0, size = 0.1) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index_domus$ymin[3], ymax = index_domus$ymax[3]), color = "black", alpha = 0, size = 0.1) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index_domus$ymin[2], ymax = index_domus$ymax[2]), color = "black", alpha = 0, size = 0.1)

figure +
  geom_text(aes(x = -Inf, y = 67, label = index_domus$dynasty[8]), angle = 90, family = "Inknut Antiqua", vjust = 1.2, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = index_domus$middle[7], label = index_domus$dynasty[7]), angle = 90, family = "Inknut Antiqua", vjust = 1.2, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = index_domus$middle[6], label = index_domus$dynasty[6]), angle = 90, family = "Inknut Antiqua", vjust = 1.2, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = index_domus$middle[5], label = index_domus$dynasty[5]), angle = 90, family = "Inknut Antiqua", vjust = 1.2, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = index_domus$middle[4], label = index_domus$dynasty[4]), angle = 90, family = "Inknut Antiqua", vjust = 1.2, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = index_domus$middle[3], label = index_domus$dynasty[3]), angle = 90, family = "Inknut Antiqua", vjust = 1.2, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = index_domus$middle[2], label = index_domus$dynasty[2]), family = "Inknut Antiqua", hjust = 0, size = 4, color = "#7E605E") +
  geom_text(aes(x = -Inf, y = 0, label = index_domus$dynasty[1]), family = "Inknut Antiqua", hjust = 0, size = 4, color = "#7E605E")

ggsave("2019-08-14/emperors.png", width = 7.3, height = 14.6)

