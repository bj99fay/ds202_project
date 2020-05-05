library(ggplot2)
library(tidyverse)

data <- read.csv(file.choose(), header = T)
byAlbum <- data %>% group_by(Album_label) %>% filter(!(is.na(average_valence) | is.na(average_danceability))) %>%
  mutate(acousticness = cut(average_acousticness, c(0, 0.25, 0.5, 0.75, 1))) %>%
  mutate(instrumentalness = cut(average_instrumentalness, c(0, 0.5, 1)))
byAlbum$energy <- byAlbum$average_energy

labels <- data %>% group_by(Fan_Genre) %>% count(Album_label) %>% filter(n >= 7)

byAlbum <- byAlbum %>% filter(Album_label %in% labels$Album_label)

byAlbum %>% ggplot(aes(x=average_danceability, y=average_valence,  color=acousticness, shape=instrumentalness, size=energy)) + 
  geom_point() + 
  labs(title = "Avg Danceability vs Avg Valence") + 
  ylab("Valence") +
  xlab("Danceability")
