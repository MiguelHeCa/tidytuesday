library(tidyverse)
library(cowplot)
library(ggrepel)
library(gapminder)
library(janitor)
library(countrycode)

# Download data from TidyTuesday's repo
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

# Check what years have any kind of data
map(list(coast_vs_waste, mismanaged_vs_gdp, waste_vs_gdp),
    ~ .x %>% drop_na() %>% distinct(Year))
# Turns out we only have 2010

# Save old names for future texts
old_column_names <- map(list(coast_vs_waste, mismanaged_vs_gdp, waste_vs_gdp),
                        ~ colnames(.x))
names(old_column_names) <- ls(pattern = "_vs_")

# Check if gdp columns are the same
m_gdp <- mismanaged_vs_gdp %>% select(gdp = 5) %>% drop_na()

w_gdp <- waste_vs_gdp %>% select(gdp = 5) %>% drop_na()

setequal(m_gdp, w_gdp)
# indeed they are

# Merge plastic and mismanagement waste, adding continents
plastic_waste <- waste_vs_gdp %>% 
  left_join(mismanaged_vs_gdp) %>% 
  left_join(coast_vs_waste) %>% 
  drop_na() %>% 
  clean_names() %>% 
  mutate(continent = countrycode(code, "iso3c", "continent")) %>% 
  select(entity,
         continent,
         gdp_pc_ppp = ends_with("international"),
         m_pw = ends_with("tonnes"),
         pw_pc = matches("capita_pl"),
         m_pw_pc = matches("capita_mi")) %>% 
  mutate(annual_waste = pw_pc * 365 * 1000 / 10^6,
         waste_ratio = m_pw_pc / pw_pc * 100)


# plot --------------------------------------------------------------------


# Facets continents
plot.plastic <- ggplot(plastic_waste,
             aes(x = gdp_pc_ppp, y = pw_pc, size = m_pw)) +
  geom_point(aes(color = continent), alpha = 0.7) +
  stat_smooth(geom = "line", method = "lm", color = "#896900", alpha = 0.3) +
  geom_smooth(method = "lm", color = NA, alpha = 0.1) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  facet_grid(. ~ continent) +
  background_grid(major = "xy", minor = "none") +
  ylab("Per capita plastic waste\n(kilograms per person per day)")
#plot.plastic

plot.mismanage <- ggplot(plastic_waste,
             aes(x = gdp_pc_ppp, y = m_pw_pc, size = m_pw)) +
  geom_point(aes(color = continent), alpha = 0.7) +
  stat_smooth(geom = "line", method = "loess", color = "#892500", alpha = 0.3) +
  geom_smooth(method = "loess", color = NA, alpha = 0.1) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  facet_grid(. ~ continent) +
  background_grid(major = "xy", minor = "none") +
  xlab("GDP per capita, PPP (constant 2011)") +
  ylab("Per capita mismanaged plastic waste\n(kilograms per person per day)")
#plot.mismanage

plot.waste <- plot_grid(plot.plastic +
            theme(legend.position = "none",
                  axis.title.x = element_text(color = "white"),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  strip.background.x = element_blank()),
          plot.mismanage +
            theme(legend.position = "none",
                  strip.text.x = element_blank()),
          nrow = 2,
          align = "v")
plot.waste

ggsave("plot_waste.png", width = 15, height = 10, units = "in")

# test code ---------------------------------------------------------------


# Facets continents
plot.plastic <- ggplot(plastic_waste,
                       aes(x = gdp_pc_ppp, y = pw_pc, size = m_pw)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", alpha = 0.2) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  facet_grid(. ~ continent) +
  background_grid(major = "xy", minor = "none") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
plot.plastic


# # more test code --------------------------------------------------------

plot.mismanage <- ggplot(plastic_waste,
                         aes(x = gdp_pc_ppp, y = m_pw_pc, size = m_pw)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.2) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  facet_grid(. ~ continent) +
  background_grid(major = "xy", minor = "none") +
  theme(legend.position = "none",
        strip.text.x = element_blank()) 
plot.mismanage

























