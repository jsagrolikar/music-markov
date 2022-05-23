library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(dplyr)
library(data.table)
library(fmsb)

cid = '5f8859ea56e84e9ca21c99891b118db6'
secret = '0c9a1dd558fe473bb6c419831ded9011'

Sys.setenv(SPOTIFY_CLIENT_ID = cid)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

access_token <- get_spotify_access_token()

songs_df <- read.csv('https://raw.githubusercontent.com/jsagrolikar/music-markov/main/updated_song_1965_2022.csv')

trial <- songs_df[1:100, ]

get_radar_plot <- function(playlist) {
  trial <- playlist
  drop <- c("year", "artists", "name", "id", "genres")
  trial <- trial[, !(names(trial) %in% drop)]
  avg <- summarise_all(trial, mean)
  maxi <- summarise_all(trial, max)
  mini <- summarise_all(trial, min)
  
  trial <- rbind(trial, avg)
  trial <- rbind(trial, maxi)
  trial <- rbind(trial, mini)
  
  rownames(trial) <- c(1:nrow(playlist), "avg", "max", "min")
  radar <- trial[c("max", "min", "avg"),]
  
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  radarchart(radar  , axistype=1 , 
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="gray", caxislabels=seq(0,1,.25), cglwd=0.8,
             #custom labels
             vlcex=0.8 )
}

get_bar_plot <- function(playlist) {
  
  trial <- playlist
  drop <- c("year", "artists", "name", "id", "genres")
  trial <- trial[, !(names(trial) %in% drop)]
  avg <- summarise_all(trial, mean)
  maxi <- summarise_all(trial, max)
  mini <- summarise_all(trial, min)
  
  class(unlist(avg[1,]))
  df <- data.frame("Variable"=c(colnames(avg)), "Value"=c(unlist(avg[1,])))
  
  trial <- rbind(trial, avg)
  trial <- rbind(trial, maxi)
  trial <- rbind(trial, mini)
  
  ggplot(df, aes(x=Variable, y=Value)) + geom_bar(stat="identity") + ylim(-2, 2) #can't get the colors to work for some reason
  
  }




get_lines <- function(dob) {
  
  dob$'song #' = c(1:nrow(dob))
  valence <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=valence), color='dark red') + ylim(-2, 2)
  acousticness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=acousticness), color='dark red') + ylim(-2, 2)
  danceability <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=danceability), color='dark red') + ylim(-2, 2)
  energy <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=energy), color='dark red') + ylim(-2, 2)
  instrumentalness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=instrumentalness), color='dark red') + ylim(-2, 2)
  key <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=key), color='dark red') + ylim(-2, 2)
  liveness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=liveness), color='dark red') + ylim(-2, 2)
  loudness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=loudness), color='dark red') + ylim(-2, 2)
  popularity <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=popularity), color='dark red') + ylim(-2, 2)
  speechiness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=speechiness), color='dark red') + ylim(-2, 2)
  tempo <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=tempo), color='dark red') + ylim(-2, 2)
  ggarrange(valence, acousticness, danceability, energy, instrumentalness, key, liveness, loudness, popularity, speechiness,
            tempo, ncol=3, nrow=4)
  
}


