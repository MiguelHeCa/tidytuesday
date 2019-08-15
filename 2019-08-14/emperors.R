# Tidytuesday week 33

# Roman Emperors

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(emoGG)
library(ggthemes)



library(emojifont)
library(gridSVG)

load.emojifont("EmojiOne.ttf")


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
  mutate(ORTUS = case_when(
    rise %in% c("Appointment by Army", "Appointment by Praetorian Guard") ~ paste("\U0001F482", "\U0001F451"),
    rise == "Appointment by Emperor" ~ paste("\U0001f934", "\U0001F451"),
    rise == "Appointment by Senate" ~ paste("\U0001F474", "\U0001F451"),
    rise == "Birthright" ~ "\U0001F476",
    rise == "Election" ~ "\U0001F5F3",
    rise == "Purchase" ~ "\U0001F4B8",
    rise == "Seized Power" ~ "\U0001F608"
  )) %>% 
  mutate(CAUSAM_ET_OCCISOR = case_when(
    cause == "Assassination" & killer == "Wife" ~ paste("\U0001F616", "\U0001f5E1", "\U0001F64B"),
    cause == "Assassination" & killer %in% c("Other Emperor", "Usurper") ~ paste("\U0001F616", "\U0001f5E1", "\U0001f934"),
    cause == "Assassination" & killer == "Senate" ~ paste("\U0001F616", "\U0001f5E1", "\U0001F474"),
    cause == "Assassination" & !killer %in% c("Other Emperor", "Usurper", "Wife", "Senate") ~ paste("\U0001F616", "\U0001f5E1", "\U0001F482"),
    cause == "Captivity" ~ "\U00026D3",
    cause == "Died in Battle" ~ "\U00002694",
    cause == "Execution" & killer == "Other Emperor" ~ paste("\U0001f934", "\U0001F44E", "\U0001F635"),
    cause == "Execution" & killer == "Senate" ~ paste("\U0001F474", "\U0001F44E", "\U0001F635"),
    cause == "Natural Causes" & killer == "Aneurism" ~ "\U0001F92F",
    cause == "Natural Causes" & killer == "Disease" ~ "\U0001F637",
    cause == "Natural Causes" & killer == "Fire" ~ "\U0001F525",
    cause == "Natural Causes" & killer == "Heart Failure" ~ "\U0001F494",
    cause == "Natural Causes" & killer == "Lightning" ~ "\U00026A1",
    cause == "Natural Causes" & killer == "Unknown" ~ "\U0001F9D0",
    cause == "Suicide" & killer == "Other Emperor" ~ paste("\U0001f934", "\U0001F449","\U0001F622"),
    cause == "Suicide" & killer == "Senate" ~ paste("\U0001F474", "\U0001F449","\U0001F622"),
    cause == "Suicide" & killer == "Unknown" ~ "\U0001F622",
    cause == "Unknown" ~ "\U0001f937",
  )) %>% 
  mutate(DOMUS = dynasty) %>% 
  select(index, NOMINE:DOMUS)


quartz()
ps = grid.export("emoji.svg", addClasses = TRUE)
ggplot(imperatorum, aes(
  x = ANNUS_NATALIS,
  xend = ANNO_MORTIS,
  y = fct_reorder(NOMINE, index),
  yend = fct_reorder(NOMINE, index)
)) +
  geom_segment(size = 2, lineend = "square", aes(color = DOMUS), alpha = 0.2) +
  geom_segment(
    aes(x = PRIMO_REGNI, xend = FINE_REGNABUNT),
    color = "darkgray",
    size = 1,
    lineend = "square"
  ) +
  geom_text(aes(
    x = ANNUS_NATALIS,
    y = NOMINE,
    label = ORTUS
  ),
  family = "EmojiOne",
  size = 3,
  hjust = 1.4,
  vjust = 0.3
  ) +
  geom_text(aes(
    x = ANNO_MORTIS,
    y = NOMINE,
    label = CAUSAM_ET_OCCISOR
  ),
  family = "EmojiOne",
  size = 3,
  hjust = -0.3,
  vjust = 0.3
  ) +
  coord_cartesian(xlim = c(-100, 450))


ggplot(imperatorum, aes(
  x = ANNUS_NATALIS,
  xend = ANNO_MORTIS,
  y = fct_reorder(NOMINE, index),
  yend = fct_reorder(NOMINE, index)
)) +
  geom_segment(size = 2, lineend = "round", aes(color = DOMUS)) +
  geom_segment(
    aes(x = PRIMO_REGNI, xend = FINE_REGNABUNT),
    color = "black",
    size = 1,
    lineend = "round"
  ) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 5.5, alpha = 0.2, color = "#2aa198") + 
  annotate("text", )
  labs(x = "Year",
       title = "Rise and fall of the Roman Emperors") +
  theme_solarized(base_size = 10, base_family = "Inknut Antiqua") + 
  theme(axis.text.y = element_text(family = "Inknut Antiqua Light", size = 7),
        axis.title = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(-100, 450))

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_emoji(emoji="1f5e1")

emperors %>% 
  select(index, name, dynasty) %>% 
  print(n = Inf)

places <- emperors %>% 
  group_by(dynasty) %>% 
  summarise(min = min(index),
            max = max(index)) %>% 
  arrange(min)

ggsave("emperors.png", width = 12 , height = 19)



utf8::utf8_print(paste("\U0001F474", "\U0001F44E", "\U0001F635"))

utf8::utf8_print("\U0001f937")

causa_asesino <- emperors %>% 
  mutate(CAUSAM_ET_OCCISOR = case_when(
    cause == "Assassination" & killer == "Wife" ~ paste("\U0001F616", "\U0001f5E1", "\U0001F64B"),
    cause == "Assassination" & killer %in% c("Other Emperor", "Usurper") ~ paste("\U0001F616", "\U0001f5E1", "\U0001f934"),
    cause == "Assassination" & killer == "Senate" ~ paste("\U0001F616", "\U0001f5E1", "\U0001F474"),
    cause == "Assassination" & !killer %in% c("Other Emperor", "Usurper", "Wife", "Senate") ~ paste("\U0001F616", "\U0001f5E1", "\U0001F482"),
    cause == "Captivity" ~ "\U00026D3",
    cause == "Died in Battle" ~ "\U00002694",
    cause == "Execution" & killer == "Other Emperor" ~ paste("\U0001f934", "\U0001F44E", "\U0001F635"),
    cause == "Execution" & killer == "Senate" ~ paste("\U0001F474", "\U0001F44E", "\U0001F635"),
    cause == "Natural Causes" & killer == "Aneurism" ~ "\U0001F92F",
    cause == "Natural Causes" & killer == "Disease" ~ "\U0001F637",
    cause == "Natural Causes" & killer == "Fire" ~ "\U0001F525",
    cause == "Natural Causes" & killer == "Heart Failure" ~ "\U0001F494",
    cause == "Natural Causes" & killer == "Lightning" ~ "\U00026A1",
    cause == "Natural Causes" & killer == "Unknown" ~ "\U0001F9D0",
    cause == "Suicide" & killer == "Other Emperor" ~ paste("\U0001f934", "\U0001F449","\U0001F622"),
    cause == "Suicide" & killer == "Senate" ~ paste("\U0001F474", "\U0001F449","\U0001F622"),
    cause == "Suicide" & killer == "Unknown" ~ "\U0001F622",
    cause == "Unknown" ~ "\U0001f937",
  )) %>% 
  select(cause, killer, CAUSAM_ET_OCCISOR) %>% 
  distinct() %>% 
  arrange(cause, killer)

ascenso <- emperors %>% 
  mutate(ORTUS = case_when(
    rise %in% c("Appointment by Army", "Appointment by Praetorian Guard") ~ paste("\U0001F482", "\U0001F451"),
    rise == "Appointment by Emperor" ~ paste("\U0001f934", "\U0001F451"),
    rise == "Appointment by Senate" ~ paste("\U0001F474", "\U0001F451"),
    rise == "Birthright" ~ "\U0001F451",
    rise == "Election" ~ "\U0001F5F3",
    rise == "Purchase" ~ "\U0001F4B8",
    rise == "Seized Power" ~ "\U0001F608"
  )) %>% 
  select(rise, ORTUS) %>% 
  distinct() %>% 
  arrange(rise) %>% 
  print(n = Inf)



