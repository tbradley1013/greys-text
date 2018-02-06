library(rvest)
library(httr)
library(tidyverse)

url <- "http://transcripts.foreverdreaming.org/viewtopic.php?f=11&t=6456"



ep_links <- read_html(url) %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  filter(str_detect(value, "viewtopic.php"),
         str_detect(value, "11&t"),
         str_detect(value, "[0-9]$"))


ep_title <- ep_links %>% 
  pull(value) %>% 
  map_df(function(x){
    site <- read_html(x) 
    
    # out <- site %>% 
    #   html_nodes("p") %>% 
    #   html_text() %>% 
    #   as_tibble() %>% 
    #   rownames_to_column()
    
    title <- site %>% 
      html_nodes("h2") %>% 
      html_text() %>% 
      as_tibble()
    
    # cat("number of lines:", nrow(out), "\n",
    #     "episode title:", title, "\n")
    
    return(title)
    
  })


ep_scripts <- ep_links %>% 
  pull(value) %>% 
  map_df(function(x){
    site <- read_html(x) 
    
    out <- site %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      as_tibble() 
    
    title <- site %>% 
      html_nodes("h2") %>% 
      html_text()
    
    out <- out %>% 
      mutate(character = ifelse(str_detect(value, ":"),
                                str_extract(value, "^[^:]+"),
                                NA),
             text = ifelse(str_detect(value, ":"),
                           str_extract(value, "(?<=: ).+"),
                           value),
             voice_over = if_else(str_detect(character, "VO"), TRUE, FALSE),
             season = str_extract(title, "^[^x]+"),
             episode_num = str_extract(title, "(?<=x).*?(?=\\s)"),
             episode_name = str_extract(title, "(?<= ).+")) %>%
      mutate(action = str_extract(text, "\\(([^\\)]+)\\)"),
             text = str_replace(text, "\\([^)]*\\)", "")) 
      # filter(!str_detect(character, "Airdate|Written by|Directed by"),
      #        !str_detect(character, "[0-9]x[0-9]|GREY'S ANATOMY|TV Show Transcripts")) %>% 
      # rownames_to_column(var = "line")
    
    # cat("episode title:", out$episode_name[[1]], "\n")
    
  })

ep_scripts <- ep_scripts %>% 
  filter(!str_detect(value, "Airdate|Written by|Directed by"),
         !str_detect(value, "[0-9]x[0-9]|GREY'S ANATOMY|TV Show Transcripts"),
         text != "") %>%
  rownames_to_column(var = "line")

saveRDS(ep_scripts, "data/episode-scripts.rds")

# ep_links <- read_html(url) %>% 
#   html_nodes("a") %>% 
#   html_attr("href") %>% 
#   as_tibble() %>% 
#   rownames_to_column() %>% 
#   filter(str_detect(value, "=11&t=")) %>% 
#   mutate(value = str_extract(value, "(?<==).+"),
#          value = str_extract(value , "(?<==).+"),
#          value = str_extract(value, "^[^&]+"),
#          value = paste0(url, "&t=", value),
#          value = str_replace(value, "viewforum", "viewtopic")) %>% 
#   pull(value)

# read_html(url) %>% 
#   html_table()

# script_url <- "http://transcripts.foreverdreaming.org/viewtopic.php?f=11&t=6737&sid=17b46453311f64b4a0fbe31a32c7f30d"
# 
# ep1_lines <- read_html(script_url) %>% 
#   html_nodes("p") %>% 
#   html_text() %>% 
#   as_tibble() %>% 
#   rownames_to_column()
# 
# ep1_title <- read_html(script_url) %>% 
#   html_nodes("h2") %>% 
#   html_text()
# 
ep1_tidy <- ep1_lines %>%
  # filter(str_detect(value, ":")) %>%
  mutate(character = ifelse(str_detect(value, ":"),
                             str_extract(value, "^[^:]+"),
                             NA),
         text = ifelse(str_detect(value, ":"),
                       str_extract(value, "(?<=: ).+"),
                       value),
         voice_over = if_else(str_detect(character, "VO"), TRUE, FALSE),
         season = str_extract(ep1_title, "^[^x]+"),
         episode_num = str_extract(ep1_title, "(?<=x).*?(?=\\s)"),
         epsidoe_name = str_extract(ep1_title, "(?<= ).+")) %>%
  mutate(action = str_extract(text, "\\(([^\\)]+)\\)"),
         text = str_replace(text, "\\([^)]*\\)", "")) %>%
  filter(!str_detect(character, "Airdate|Written by|Directed by"),
         !str_detect(character, "[0-9]x[0-9]|GREY'S ANATOMY|TV Show Transcripts"))
# 
# 
# ep1_tidy <- ep1_lines %>% 
#   filter(str_detect(value, ":")) %>% 
#   mutate(character = str_extract(value, "^[^:]+"),
#          text = str_extract(value, "(?<=: ).+"),
#          voice_over = if_else(str_detect(character, "VO"), TRUE, FALSE))
# 
# title <- ep1_tidy$value[[1]]
# 
# ep_tidy <- ep1_tidy %>% 
#   mutate(season = str_extract(title, "^[^x]+"),
#          episode_num = str_extract(title, "(?<=x).*?(?=\\s)"),
#          epsidoe_name = str_extract(title, "(?<= ).+")) %>% 
#   mutate(action = str_extract(text, "\\(([^\\)]+)\\)")) 

# ^(.*?)(\\:)


