#===============================================================================
# This script will do initial text analysis of grey's anatomy scripts
#
# Tyler Bradley
# 2018-02-02
#===============================================================================

library(tidyverse)
library(tidytext)

episode_scripts <- readRDS("data/episode-scripts.rds")
data("stop_words")

episode_words <- episode_scripts %>% 
  unnest_tokens(word, text)


tidy_words <- episode_words %>% 
  anti_join(stop_words)

tidy_words %>% 
  count(word, sort = TRUE) %>% 
  top_n(30) %>% 
  mutate(word = factor(word) %>% forcats::fct_reorder(n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip()


ncjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "trust")

tidy_words %>% 
  inner_join(ncjoy) %>% 
  # group_by(season) %>% 
  count(word, sort = TRUE)


tidy_words %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(season, episode_num) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative,
         ep = str_c(season, "-", episode_num)) %>%
  ggplot(aes(episode_num, sentiment, fill = season)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~season, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, size = 6))
  
