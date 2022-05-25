library(shiny)
library(DT)
library(ggplot2)
library(tibble)
library(dplyr)
library(data.table)
library(fmsb)
library(ggpubr)

net_distance <- function(song1, song2) {
  dist <- sqrt(((song1$valence)-song2$valence)^2+(as.vector(song1$acousticness)-song2$acousticness)^2+
         (as.vector(song1$danceability)-song2$danceability)^2+(as.vector(song1$energy)-song2$energy)^2+
         (as.vector(song1$instrumentalness)-song2$instrumentalness)^2+
         (as.vector(song1$liveness)-song2$liveness)^2+(as.vector(song1$loudness)-song2$loudness)^2+
         (as.vector(song1$popularity)-song2$popularity)^2+(as.vector(song1$speechiness)-song2$speechiness)^2+
         (as.vector(song1$tempo)-song2$tempo)^2)
  return(dist)
}

sq_df1 <- read.csv("https://raw.githubusercontent.com/jsagrolikar/music-markov/main/updated_song_1920_1964.csv")
sq_df2 <- read.csv("https://raw.githubusercontent.com/jsagrolikar/music-markov/main/updated_song_1965_2022.csv")
all_song_quant_df <- rbind(sq_df1, sq_df2)

all_gen_quant_df <- read.csv("https://raw.githubusercontent.com/jsagrolikar/music-markov/main/upload_gen_df.csv")
all_gen_quant_df <- all_gen_quant_df[all_gen_quant_df$genres%in%(unique(all_song_quant_df$genres)),]

all_song_quant_df <- all_song_quant_df[, !(names(all_song_quant_df)=="key")]
all_gen_quant_df <- all_gen_quant_df[, !(names(all_gen_quant_df)=="key")]

gen_ss_mat <- matrix(nrow = nrow(all_gen_quant_df), ncol = nrow(all_gen_quant_df))
gen_ss_list <- list()
for (gen_id in 1:nrow(all_gen_quant_df)) {
    spec_gen_df <- all_gen_quant_df[gen_id,]
    gen_ss <- sqrt((as.vector(spec_gen_df$valence)-all_gen_quant_df$valence)^2+(as.vector(spec_gen_df$acousticness)-all_gen_quant_df$acousticness)^2+
                       (as.vector(spec_gen_df$danceability)-all_gen_quant_df$danceability)^2+(as.vector(spec_gen_df$energy)-all_gen_quant_df$energy)^2+
                       (as.vector(spec_gen_df$instrumentalness)-all_gen_quant_df$instrumentalness)^2+
                       (as.vector(spec_gen_df$liveness)-all_gen_quant_df$liveness)^2+(as.vector(spec_gen_df$loudness)-all_gen_quant_df$loudness)^2+
                       (as.vector(spec_gen_df$popularity)-all_gen_quant_df$popularity)^2+(as.vector(spec_gen_df$speechiness)-all_gen_quant_df$speechiness)^2+
                       (as.vector(spec_gen_df$tempo)-all_gen_quant_df$tempo)^2)
    gen_ss_mat[gen_id,] <- (max(gen_ss)-gen_ss)/(sum(max(gen_ss)-gen_ss))
}
trans_prob_mat <- gen_ss_mat
colnames(trans_prob_mat) <- all_gen_quant_df$genres
rownames(trans_prob_mat) <- all_gen_quant_df$genres

tra_fun <- function(input_gen, input_song) {
    spec_gen_prob_df <- trans_prob_mat[input_gen,]
    rep_count_vec <- (ceiling(spec_gen_prob_df*1000000))+1
    rep_list <- list()
    for (gen_id in 1:nrow(all_gen_quant_df)) {
        rep_list[[gen_id]] <- (rep(all_gen_quant_df[gen_id,]$genres, rep_count_vec[gen_id]))
    }
    select_gen_df <- all_song_quant_df[all_song_quant_df$genres==sample(unlist(rep_list),1),]
    
    select_song <- select_gen_df[1,]
    for (i in 1:nrow(select_gen_df)) {
      if (net_distance(input_song, select_gen_df[i, ]) < net_distance(input_song, select_song)) {
        select_song <- select_gen_df[i, ]
      }
    }
    #select_song <- select_gen_df[sample(nrow(select_gen_df),1),]
    return(select_song)
}

markovplaylist_fun <- function(input_song, playlist_length){
    init_gen <- input_song$genres
    play_gen_list <- list()
    play_song_list <- list()
    play_gen_list[[1]] <- init_gen
    play_song_list[[1]] <- input_song$id
    for (it_id in 2:(playlist_length)) {
        select_song <- tra_fun(play_gen_list[[it_id-1]], input_song)
        play_gen_list[[it_id]] <- select_song$genres
        play_song_list[[it_id]] <- select_song$id
    }
    returned_playlist_full_info_df <- (all_song_quant_df[all_song_quant_df$id %in% unlist(play_song_list),])
    order_df <- data.frame(id=unlist(play_song_list), order=c(1:playlist_length))
    returned_playlist_full_info_df <- merge(returned_playlist_full_info_df, order_df, by=c("id"))
    returned_playlist_full_info_df <- returned_playlist_full_info_df[order(returned_playlist_full_info_df$order),]
    display_df <- returned_playlist_full_info_df[, c(14, 13, 12)]
    return(display_df)
}

markovDay_fun <- function(valence_coeff, acousticness_coeff,danceability_coeff,energy_coeff,instrumentalness_coeff, liveness_coeff,loudness_coeff,
                          popularity_coeff,speechiness_coeff, tempo_coeff, playlist_length){
    all_song_inp_ss <- sqrt((valence_coeff-all_song_quant_df$valence)^2+(acousticness_coeff-all_song_quant_df$acousticness)^2+
             (danceability_coeff-all_song_quant_df$danceability)^2+(energy_coeff-all_song_quant_df$energy)^2+
             (instrumentalness_coeff-all_song_quant_df$instrumentalness)^2+
             (liveness_coeff-all_song_quant_df$liveness)^2+(loudness_coeff-all_song_quant_df$loudness)^2+
             (popularity_coeff-all_song_quant_df$popularity)^2+(speechiness_coeff-all_song_quant_df$speechiness)^2+
             (tempo_coeff-all_song_quant_df$tempo)^2)
    all_song_inp_ss[is.na(all_song_inp_ss)] <- Inf 
    initial_song <- all_song_quant_df[which(all_song_inp_ss==min(all_song_inp_ss)),]
    return_df <- markovplaylist_fun(initial_song, playlist_length)
    row.names(return_df) <- NULL
    return(return_df)
    
}
markovDiagram_fun <- function(event_table, playlist_length) {
    event_data <- event_table
    
    data <- tibble(x= 1:100, y= 1:100)
    data %>% 
        ggplot(aes(x, y)) +
        scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
        scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
        theme_linedraw() ->
        p
    
    p1 <- p +  geom_rect(xmin = 36, xmax=64, ymin=94, ymax=100, color='black',
                         size=0.25, fill="red4", alpha=0.01) +
        annotate('text', x= 50, y=97,label= event_data[1,1], size=2.5)+theme_void()
    p2 <- p1 +  geom_rect(xmin = 36, xmax=64, ymin=80, ymax=86, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=83,label= event_data[2,1], size=2.5)+geom_segment(
            x=50, xend=50, y=94, yend=86, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p3 <- p2 +  geom_rect(xmin = 36, xmax=64, ymin=66, ymax=72, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=69,label= event_data[3,1], size=2.5)+geom_segment(
            x=50, xend=50, y=80, yend=72, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p4 <- p3 +  geom_rect(xmin = 36, xmax=64, ymin=52, ymax=58, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=55,label= event_data[4,1], size=2.5)+geom_segment(
            x=50, xend=50, y=66, yend=58, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p5 <- p4 +  geom_rect(xmin = 36, xmax=64, ymin=38, ymax=44, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=41,label= event_data[5,1], size=2.5)+geom_segment(
            x=50, xend=50, y=52, yend=44, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p6 <- p5 +  geom_rect(xmin = 36, xmax=64, ymin=24, ymax=30, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=27,label= event_data[6,1], size=2.5)+geom_segment(
            x=50, xend=50, y=38, yend=30, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p7 <- p6 +  geom_rect(xmin = 36, xmax=64, ymin=10, ymax=16, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=13,label= event_data[7,1], size=2.5)+geom_segment(
            x=50, xend=50, y=24, yend=16, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p8 <- p7 +  geom_rect(xmin = 36, xmax=64, ymin=-4, ymax=2, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-1,label= event_data[8,1], size=2.5)+geom_segment(
            x=50, xend=50, y=10, yend=2, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p9 <- p8 +  geom_rect(xmin = 36, xmax=64, ymin=-18, ymax=-12, color='black',
                          fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-15,label= event_data[9,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-4, yend=-12, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p10 <- p9 +  geom_rect(xmin = 36, xmax=64, ymin=-32, ymax=-26, color='black',
                           fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-29,label= event_data[10,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-18, yend=-26, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p11 <- p10 +  geom_rect(xmin = 36, xmax=64, ymin=-46, ymax=-40, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-43,label= event_data[11,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-32, yend=-40, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p12 <- p11 +  geom_rect(xmin = 36, xmax=64, ymin=-60, ymax=-54, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-57,label= event_data[12,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-46, yend=-54, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p13 <- p12 +  geom_rect(xmin = 36, xmax=64, ymin=-74, ymax=-68, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-71,label= event_data[13,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-60, yend=-68, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p14 <- p13 +  geom_rect(xmin = 36, xmax=64, ymin=-88, ymax=-82, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-85,label= event_data[14,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-74, yend=-82, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p15 <- p14 +  geom_rect(xmin = 36, xmax=64, ymin=-102, ymax=-96, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-99,label= event_data[15,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-88, yend=-96, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p16 <- p15 +  geom_rect(xmin = 36, xmax=64, ymin=-116, ymax=-110, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-113,label= event_data[16,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-102, yend=-110, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p17 <- p16 +  geom_rect(xmin = 36, xmax=64, ymin=-130, ymax=-124, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-127,label= event_data[17,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-116, yend=-124, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p18 <- p17 +  geom_rect(xmin = 36, xmax=64, ymin=-144, ymax=-138, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-141,label= event_data[18,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-130, yend=-138, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p19 <- p18 +  geom_rect(xmin = 36, xmax=64, ymin=-158, ymax=-152, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-155,label= event_data[19,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-144, yend=-152, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    p20 <- p19 +  geom_rect(xmin = 36, xmax=64, ymin=-172, ymax=-166, color='black',
                            fill='red4', alpha=0.01, size=0.25) +
        annotate('text', x= 50, y=-169,label= event_data[19,1], size=2.5)+geom_segment(
            x=50, xend=50, y=-158, yend=-166, 
            size=0.15, linejoin = "mitre", lineend = "butt",
            arrow = arrow(length = unit(1, "mm"), type= "closed"))+theme_void()
    
    if (playlist_length==3) {
        diagram <- p3
    } else if (playlist_length==4) {
        diagram <- p4
    } else if (playlist_length==5) {
        diagram <- p5
    } else if (playlist_length==6) {
        diagram <- p6
    } else if (playlist_length==7) {
        diagram <- p7
    } else if (playlist_length==8) {
        diagram <- p8
    } else if (playlist_length==9) {
        diagram <- p9
    } else if (playlist_length==10) {
        diagram <- p10
    } else if (playlist_length==11) {
        diagram <- p11
    } else if (playlist_length==12) {
        diagram <- p12
    } else if (playlist_length==13) {
        diagram <- p13
    } else if (playlist_length==14) {
        diagram <- p14
    } else if (playlist_length==15) {
        diagram <- p15
    } else if (playlist_length==16) {
        diagram <- p16
    } else if (playlist_length==17) {
        diagram <- p17
    } else if (playlist_length==18) {
        diagram <- p18
    } else if (playlist_length==19) {
        diagram <- p19
    } else if (playlist_length==20) {
        diagram <- p20
    }
    return(diagram)
}

playlist_vals_gen <- function(df) {
  xxx <- df
  yyy <- subset(all_song_quant_df, name %in% xxx$name)
  zzz <- yyy %>% distinct(name, .keep_all=TRUE)
  return(zzz)
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
  
  diagram <- ggplot(df, aes(x=Variable, y=Value)) + geom_bar(stat="identity", color="#00B8E7", fill="#00B8E7") + ylim(-2, 2) + coord_flip() #can't get the colors to work for some reason
  return(diagram)
  
}

comparison_bar <- function(allsongs, playlist) {
  
  trial <- playlist
  drop <- c("year", "artists", "name", "id", "genres")
  trial <- trial[, !(names(trial) %in% drop)]
  avg <- summarise_all(trial, mean)
  maxi <- summarise_all(trial, max)
  mini <- summarise_all(trial, min)
  
  class(unlist(avg[1,]))
  df <- data.frame("Variable"=c(colnames(avg)), "Value"=c(unlist(avg[1,])))
  
  trial2 <- allsongs
  trial2 <- na.omit(trial2)
  trial2 <- trial2[, !(names(trial2) %in% drop)]
  avg2 <- summarise_all(trial2, mean)
  
  bar <- as.data.frame(matrix(0, ncol=1, nrow=20))
  bar$set <- unlist(c(rep("playlist", 11), rep("dataset", 11)))
  bar$variable <- unlist(c(df$Variable, df$Variable))
  bar$value <- unlist(c(df$Value, avg2))
  bar <- bar[, !(names(bar)=="V1")]
  
  diagram <- ggplot(bar, aes(fill=set, x=variable, y=value)) + geom_bar(position="dodge", stat="identity") + ylim(-1, 1) + coord_flip() #can't get the colors to work for some reason
  return(diagram)
  
}



get_lines <- function(dob) {
  
  dob$'song #' = c(1:nrow(dob))
  valence <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=valence), color='#0CB702') + ylim(-2, 2)
  acousticness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=acousticness), color="#F8766D" ) + ylim(-2, 2)
  danceability <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=danceability), color="#00A9FF") + ylim(-2, 2)
  energy <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=energy), color= "#00BA38") + ylim(-2, 2)
  instrumentalness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=instrumentalness), color= "#00BFC4" ) + ylim(-2, 2)
  
  liveness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=liveness), color="#619CFF") + ylim(-2, 2)
  loudness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=loudness), color= "#F564E3") + ylim(-2, 2)
  popularity <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=popularity), color='#C77CFF') + ylim(-2, 2)
  speechiness <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=speechiness), color='#FF61CC') + ylim(-2, 2)
  tempo <- ggplot(dob, aes(x=`song #`)) + geom_line(aes(y=tempo), color='#8494FF') + ylim(-2, 2)
  diagram <- ggarrange(valence, acousticness, danceability, energy, instrumentalness,liveness, loudness, popularity, speechiness,
            tempo, ncol=5, nrow=2)
  return(diagram)
  
}

comparison_radar <- function(allsongs, playlist) {
  
  trial <- playlist
  trial <- na.omit(trial)
  drop <- c("year", "artists", "name", "id", "genres")
  trial <- trial[, !(names(trial) %in% drop)]
  avg <- summarise_all(trial, mean)
  maxi <- summarise_all(trial, max)
  mini <- summarise_all(trial, min)
  
  trial <- rbind(trial, avg)
  trial <- rbind(trial, maxi)
  trial <- rbind(trial, mini)
  
  trial2 <- allsongs
  trial2 <- na.omit(trial2)
  trial2 <- trial2[, !(names(trial2) %in% drop)]
  avg2 <- summarise_all(trial2, mean)
  
  trial <- rbind(trial, avg2)
  radar <- tail(trial, n=4)
  rownames(radar) <- c("avg", "max", "min", "avg2")
  #rownames(trial) <- c(1:nrow(na.omit(playlist)), "avg", "max", "min", "avg2")
  #radar <- trial[c("max", "min", "avg", "avg2"),]
  
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  chart <- radarchart( radar  , axistype=1 , 
                       #custom polygon
                       pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       #custom the grid
                       cglcol="grey", cglty=1, axislabcol="gray", caxislabels=seq(0,1,.25), cglwd=0.8,
                       #custom labels
                       vlcex=0.8 )
  
  return(chart)
  
  
}



ui <- fluidPage(
    
    
    # Application title
    titlePanel("A Markov Mix"), p("Synthesizing Happy Accidents", style = "font-size:15px;"),
    
    # Sidebar with a slider input 
    sidebarLayout(
        sidebarPanel(
            sliderInput("playlist_length",
                        "How Many Songs Would You Like in the Playlist?:",
                        min = 3,
                        max = 50,
                        value = 10), br(), h4("Rate The Importance of Each Factor from -1 to 1"), br(),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("valence_coeff",
                                                                                            "Valence",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("acousticness_coeff",
                                                                                            "Acousticness",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("danceability_coeff",
                                                                                            "Danceability",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("instrumentalness_coeff",
                                                                                            "Instrumentalness",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("energy_coeff",
                                                                                            "Energy",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            
            
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("liveness_coeff",
                                                                                            "Liveness",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("loudness_coeff",
                                                                                            "Loudness",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("popularity_coeff",
                                                                                            "Popularity",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0,step = 0.01,  width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("speechiness_coeff",
                                                                                            "Speechiness",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            div(style="display: inline-block;vertical-align:top; width: 112px;",sliderInput("tempo_coeff",
                                                                                            "Tempo",
                                                                                            min = -1,
                                                                                            max = 1,
                                                                                            value = 0, step = 0.01, width = 105, ticks=F)),
            plotOutput("bar")
        ), 
        
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(tabPanel("Main",
            actionButton("button", "Generate Mix"), br(), br(), br(), DT::dataTableOutput("playlist_table"), 
            # fluidRow(
            #   splitLayout(cellWidths = c("100%", "0%"), plotOutput("lines"), plotOutput("bar"))
            # )
            plotOutput("lines")),
            tabPanel("About",
                     h2("Instructions", align="center"), h3("Use the sliders on the left panel to toggle the inputs for the seed of the Markov Chain. 
                                                            At each stage, the cost function in the algorithm will seek to minimize the distance between those
                                                            inputs and the segment of the dataset produced by a probability distribution. Happy listening!"), 
                     h2("Data", align="center"), h3("The data for this project was collected using Spotify's public API. The characteristics on the previous page
                                                    is precisely the data that Spotify decides to collect. Spotify does collect 'key' data on songs, which we chose to omit
                                                    since a clear gradient cannot be formed. The goal of the generator is to create a diverse set of songs using a combination
                                                    of initial input and randomness. For revenue purposes, music streaming services are heavily incentivized to pigeonhole
                                                    listeners into their current tastes, which heavily loses the ability to diversify one's music tastes (without
                                                    actively seeking to do so, that is). Hopefully, this makes it easier."),
                     h2("Visuals", align="center"), h3("The bar plot on the side panel gives an idea of your generated mix's deviation from the entire Spotify dataset. For
                                                       example, an 'acousticness' bar stretching far to the right means that your playlist has a much higher than average
                                                       acousticness compared to the dataset as a whole. The plots below the playlist showcase how each Spotify
                                                       characteristic changes over the length of the playlist. Longer playlists tend to give more time for trends
                                                       to form in this regard, especially since there are chances data was not collected for particular songs."))
        )),
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # observeEvent(input$button, {
    #    output$event_table = DT::renderDataTable({
    #      markovDay_fun(input$currentEvents_coeff,input$education_coeff,input$dance_coeff,input$finance_coeff,input$healthMedicine_coeff,
    #                    input$international_coeff,input$law_coeff,input$literature_coeff,input$music_coeff,input$socialActivism_coeff,
    #                    input$sports_coeff,input$stem_coeff,input$theater_coeff,input$visualArts_coeff,input$volunteering_coeff, input$playlist_length)
    #   })
    # })
    selectedData <- reactive({
        markovDay_fun(input$valence_coeff, input$acousticness_coeff, input$danceability_coeff,input$energy_coeff,input$instrumentalness_coeff,input$liveness_coeff,input$loudness_coeff,
                      input$popularity_coeff,input$speechiness_coeff, input$tempo_coeff, input$playlist_length)
    })
    observeEvent(input$button, {
        output$playlist_table = DT::renderDataTable({
            selectedData()
        })
    })
    # observeEvent(input$button, {
    #     output$markovDiagram = renderPlot(
    #         markovDiagram_fun(selectedData(), input$playlist_length)
    #     , width=500, height=800)
    # })
    
    observeEvent(input$button, {
      output$lines = renderPlot(get_lines(playlist_vals_gen(selectedData())))
    })
    
    observeEvent(input$button, {
      output$bar = renderPlot(get_bar_plot(playlist_vals_gen(selectedData())))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

