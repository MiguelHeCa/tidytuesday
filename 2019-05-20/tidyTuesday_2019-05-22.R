
# TidyTuesdays guidelines
"https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-21"

# Load packages
library(tidyverse)
library(cowplot)
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

# Check if gdp columns are the same
m_gdp <- mismanaged_vs_gdp %>% select(gdp = 5) %>% drop_na()

w_gdp <- waste_vs_gdp %>% select(gdp = 5) %>% drop_na()

setequal(m_gdp, w_gdp)

rm(list = c("m_gdp", "w_gdp"))

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
         m_pw_pc = matches("capita_mi"))

# plot --------------------------------------------------------------------

# Set colors for size and model lines
p_col <- c("#0080cc", "#cc0080", "#80cc00")

# Plot for plastic waste per capita
plot.plastic <- ggplot(plastic_waste,
                       aes(x = gdp_pc_ppp, y = pw_pc, size = m_pw)) +
  geom_point(color = p_col[1], alpha = 0.7) +
  stat_smooth(
    geom = "line",
    method = "lm",
    color = p_col[2],
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_smooth(method = "lm",
              color = NA,
              alpha = 0.1,
              show.legend = FALSE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  scale_size(name = "Mismanaged plastic waste (tonnes)",
             labels = scales::comma) +
  facet_grid(. ~ continent) +
  background_grid(major = "xy", minor = "none") +
  panel_border() +
  labs(title = "Plastic waste per capita against GDP per capita in 2010, by continent",
       x = "GDP per capita, PPP (constant 2011, log scale)",
       y = "Plastic waste\n(kg per person per day, log scale)",
       caption = "The richer the country the higher the plastic waste in all continents.")

# Plot for plastic wage mismanagement per capita
plot.mismanage <- ggplot(plastic_waste,
                         aes(x = gdp_pc_ppp, y = m_pw_pc, size = m_pw)) +
  geom_point(color = p_col[1], alpha = 0.7) +
  stat_smooth(
    geom = "line",
    method = "loess",
    color = p_col[3],
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_smooth(method = "loess",
              color = NA,
              alpha = 0.1,
              show.legend = FALSE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  facet_grid(. ~ continent) +
  background_grid(major = "xy", minor = "none") +
  panel_border() +
  labs(title = "Plastic waste mismanagement per capita against GDP per capita in 2010 by continent",
       x = "GDP per capita, PPP (constant 2011, log scale)",
       y = "Mismanaged plastic waste\n(kg per person per day, log scale)",
       caption = "Middle income countries struggle the most with waste management. However, it gets better in richer countries.")

# Inner grid that contains the two plots
plot.inner <- plot_grid(
  plot.plastic +
    theme(
      legend.position = "none",
      strip.background.x = element_blank(),
      plot.caption = element_text(hjust = 0.5)
    ),
  plot.mismanage +
    theme(
      legend.position = "none",
      strip.background.x = element_blank(),
      plot.caption = element_text(hjust = 0.5)
    ),
  nrow = 2,
  align = "v"
)

# Extraction of legend for the outer plot
plot.legend <- get_legend( plot.plastic + theme(legend.position = "bottom"))

plot.outer <- plot_grid(
  plot.inner,
  plot.legend,
  ncol = 1,
  nrow = 2,
  rel_heights = c(1, .1)
)

# Gather inner and outer layers
printed.plot <- add_sub(plot.outer,
                        "Source: Our World in Data and R4DS' TidyTuesdays.",
                        x = 1,
                        hjust = 1,
                        size = 11)

ggdraw(printed.plot)

ggsave("plot_waste.png", width = 20, height = 12, units = "in")

gc()























