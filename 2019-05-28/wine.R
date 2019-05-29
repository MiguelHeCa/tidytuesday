# TidyTuesday Week 22


# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(wordcloud2)

# Prepare data ------------------------------------------------------------

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wr <- wine_ratings %>% drop_na(points, price)

# Select only latinamerican countries
latinamerica <- c("Argentina", "Brazil",  "Chile", "Mexico", "Peru", "Uruguay")

wr_lat <- wr %>% 
  filter(country %in% latinamerica) %>% 
  select(country, description, points, price)

# It was easier to include this words in the stop_words data frame than
# using regex. I excluded this words because I thought they might not give 
# give any useful insight about wine description.
additional_stop_words <- tibble(
  word = c(
    "flavor",
    "flavors",
    "aroma",
    "aromas",
    "drink",
    "drinks"
  ),
  lexicon = "Wine"
)

all_stop_words <- stop_words %>% 
  bind_rows(additional_stop_words)

# Select only the observations whose points are ranked in the top ten.
# Then I prepare data for the word clouds
wine_lat <- wr_lat %>%
  group_by(country) %>% 
  mutate(ranking = dense_rank(points)) %>% 
  arrange(ranking) %>% 
  ungroup() %>% 
  filter(ranking >= 10) %>% 
  mutate(description = str_replace_all(description, "\\d", "")) %>% 
  group_by(country) %>% 
  unnest_tokens(word, description) %>% 
  anti_join(all_stop_words, by = c("word" = "word")) %>% 
  count(word, sort = T, name = "freq") %>% 
  ungroup()

# Get a vector of the countries that are left from the criteria. Sorry Peru!
rated_countries <- wine_lat %>% 
  distinct(country) %>% 
  arrange(country) %>% 
  unlist() %>% 
  unname()

# Word cloud --------------------------------------------------------------

wine_colors <-  c("#5b0b0b", "#790000", "#8f8023", "#9e934d", "#bcb37b")

create_wordcloud <- function(COUNTRY){
  wine_lat %>% 
    filter(country == COUNTRY) %>% 
    select(word, freq) %>% 
    wordcloud2(size = 1,
               color = rep_len(wine_colors, nrow(.)),
               fontFamily = "Open Sans")
}  

wc_a <- create_wordcloud(rated_countries[1])
wc_b <- create_wordcloud(rated_countries[2])
wc_c <- create_wordcloud(rated_countries[3])
wc_m <- create_wordcloud(rated_countries[4])
wc_u <- create_wordcloud(rated_countries[5])

# Saved word clouds manually through RStudio given that saving it by code
# is too convoluted.
# I tried ggwordcloud and wordcloud also, but rendering took too long and the 
# cloud is not as aesthetically pleasing as in wordcloud2.
# Caveat is that making grids with wordcloud2 are a real pain, though,
# so I cheated a little bit with the final plot (edited it elsewhere).
# If you know any realiable method to make grids with wordcloud or wordcloud2
# please let me know.


