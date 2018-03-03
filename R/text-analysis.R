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


sents <- get_sentiments("nrc") 

# bar charts for word frequency broken up by sentiments from the 
# nrc data set 
# It looks like time is the most common word from these sentimenets 
# followed by surgery. Time is denoted by "anticipation" and 
# surgery is denoted by both "fear" and "sadness"
tidy_words %>% 
  inner_join(sents) %>% 
  group_by(sentiment) %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word = factor(word) %>% fct_reorder(n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  facet_wrap(~ sentiment, scales = "free_y") + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 7)) +
  coord_flip()


# this graph looks at the sentiment score for each epsiode of 
# grey's anatomy
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

# This figure looks at the frequency of the 20 most common positive and
# negative words in all of the seasons of grey's anatomy included in this 
# analysis. 
tidy_words %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(20) %>%
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  facet_wrap(~ sentiment, scales = "free_y") + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip()

# this graph shows a general count of positive and negative words per season
# it looks like every seaons has more negative words than postive
# this is not necessarily surprising since Shonda Rhymes loves to build up 
# the drama until the end of episodes (or sequence of episodes) before 
# there is a happy ending (if there is one at all)
tidy_words %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(season, sentiment) %>% 
  arrange(season) %>% 
  ggplot(aes(season, n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw()



#
ep_words <- tidy_words %>% 
  group_by(season) %>% 
  mutate(total_n = n()) %>% 
  ungroup() %>% 
  group_by(word, season, total_n) %>% 
  summarize(word_count = n()) %>% 
  arrange(desc(word_count)) %>% 
  ungroup() %>% 
  filter(!stringr::str_detect(word, "_"))


ep_words %>% 
  ggplot(aes(word_count/total_n, fill = season)) + 
  geom_histogram(show.legend = FALSE) + 
  facet_wrap(~season, scales = "free_y") + 
  theme_bw()

word_freq <- ep_words %>% 
  group_by(season) %>% 
  mutate(rank = row_number(),
         term_freq = word_count/total_n)

ggplot(word_freq, aes(rank, term_freq, color = season)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_log10() + 
  scale_y_log10()


imp_words <- ep_words %>% 
  bind_tf_idf(word, season, word_count) %>% 
  select(-total_n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(season) %>% 
  top_n(15) %>% 
  # mutate(word = fct_relevel(word, tf_idf)) %>% 
  ungroup()

imp_words %>% 
  ggplot(aes(word, tf_idf, fill = season)) + 
  facet_wrap(~season, scales = "free_y") + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_bw(base_size = 10)
  