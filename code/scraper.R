library(tidyverse)
library(stringr)
library(rvest)

# episode links
url = 'https://www.officequotes.net/'
page = read_html(url)
all_links = page %>% html_node('#text-6') %>% html_nodes('a') %>% html_attr('href')
episode_links = paste('https://www.officequotes.net', all_links[str_detect(all_links, 'no')], sep = '')

# lines
datalist = list()
counter = 1
for (i in 1:length(episode_links)) {
  tryCatch({
    Sys.sleep(15)
    print(episode_links[i])
    
    # season and episode number
    r = str_extract(episode_links[i], '\\d-\\d\\d')
    season = as.numeric(substr(r, 1, 1))
    episode = as.numeric(substr(r, 3, 5))
    
    # episode scenes
    page = read_html(episode_links[i])
    scenes = page %>% html_nodes('.quote')
    for (j in 1:length(scenes)) {
      lines = scenes[j] %>% html_text2() %>% strsplit(., "\\n") %>% unlist()
      temp = tibble(
        season = season,
        episode = episode,
        scene = j,
        text = lines
      ) %>%
        mutate(line = row_number())
      datalist[[counter]] = temp
      counter = counter + 1
      print(paste(season, episode, j, sep = ' : '))
    }
  },
  error = function(error_message) {
    message(error_message)
    return(NA)
  })
}
raw = do.call(rbind, datalist)

# missing season 4, episode 8
# missing season 5, episode 18

# clean
the_office = raw %>%
  separate(text, into = c('speaker', 'text'), sep = ': ') %>%
  mutate(clean_text = trimws(str_remove_all(text, '\\[.*?\\]'))) %>% 
  filter(., is.na(text) == FALSE) %>%
  mutate(id = row_number()) %>%
  select(id, season, episode, scene, line, speaker, text = clean_text)

# export
setwd("~/Downloads")
write_csv(the_office, 'the_office.csv')
