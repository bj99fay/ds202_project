setwd('/Users/chase/Desktop/RProjects/ds202_project/')
library(spotifyr)
library(tidyverse)
library(ggplot2)
Sys.setenv(SPOTIFY_CLIENT_ID = 'f951a88939f243038159e4e9f0705fec')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1606f2bddb894c669a220aa5ea8890e3')

access_token <- get_spotify_access_token()


spotifyInfo <- read.csv('./Top_100_by_fans.csv')
spotifyInfo

df <- spotifyInfo

mostVaried <- df %>% group_by(Fan_Genre) %>% summarize(unique = n_distinct(Artist))
mostVaried

ggplot(mostVaried, mapping = aes(x = Fan_Genre, y = unique, fill = Fan_Genre)) + geom_bar(stat = 'Identity') + coord_flip()



noId <- df %>% filter(Album_Id != "")

for (val in seq(1,874,by = 19))
{
  print(val)
  noId[val:(val+19), "Album_release"] = get_albums(noId[val:(val+19),"Album_Id"])$release_date
}

noId <- noId %>% mutate(release_year = substr(Album_release,1,4))


YearTally <- noId %>% group_by(release_year) %>% tally()
YearTally
noId

noIdJoin <- noId %>% inner_join(YearTally, by = 'release_year') %>% transform(release_year = as.integer(release_year))
noIdJoin %>% ggplot(mapping = aes(x = release_year, y = n)) + geom_line() + facet_wrap('Fan_Genre')

noIdJoin %>% group_by(Fan_Genre) %>% summarise(meanRelease = sum(release_year)/n(), earliest = min(release_year), 
                                               newest = max(release_year))


noIdJoin %>% group_by(Fan_Genre) %>% ggplot(noIdJoin, mapping = aes(x = Fan_Genre, y = release_year)) + geom_boxplot()


noIdJoin %>% transform(release_year = release_year%/%10) %>% group_by(Fan_Genre) %>% group_by(release_year) %>% 
  mutate(meanPopularity = mean(Popularity)) %>%
  ggplot(mapping = aes(x = release_year, y = meanPopularity)) + geom_bar(stat = 'identity') + facet_wrap('Fan_Genre')


noIdJoin



for (val in seq(1,874,by = 19))
{
  print(val)
  info <- get_albums(noId[val:(val+19),"Album_Id"])
  noId[val:(val+19), "Album_release"] = info$release_date
  noId[val:(val+19), "Album_length"] = info$tracks.total
  noId[val:(val+19), "Album_label"] = info$label
}




noId <- drop_na(noId)

noId %>% group_by(Fan_Genre) %>% mutate(averageLength = mean(Album_length)) %>% 
  ggplot(mapping = aes(x = Fan_Genre, y = averageLength)) + geom_bar(stat = 'identity')



noId %>% group_by(Album_label) %>% tally() %>% filter(n>10) %>%
  ggplot(mapping = aes(x = Album_label, y = n)) + geom_bar(stat = 'identity') + coord_flip()

labelTally <- noId %>% group_by(Album_label) %>% tally()

noIdJoin <- noId %>% inner_join(labelTally, by ='Album_label') 

noIdJoin %>% filter(n>10) %>% 
  ggplot(mapping = aes(x = Album_label, y = n)) + geom_bar(stat = 'identity') + coord_flip()


noId

write_excel_csv(noId, path = './Album_Label_Year.csv')
?write_excel_csv

read.csv('./Album_Label_Year.csv')


