folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')
library(ggplot2)
library(scales)
library(gridExtra)

# Obtenemos los tweets de los hashtags
q <- paste('SELECT h.hashtag, CAST(h.created_at AS DATE) as date, CAST(h.created_at AS TIME) as time, h.user_screen_name, h.user_time_zone, h.user_followers_count
           FROM hashtags_time as h
           WHERE ommit = 0
           ORDER BY h.hashtag, h.created_at',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Armamos la tabla de frecuencia
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i) , ]
  
  # Creamos los contadores
  contador.leaders <- 0
  contador.orgs <- 0
  contador.movs <- 0
  contador.medios <- 0
  contador.famosos <- 0
  contador.people <- 0
  
  # Creamos la tabla
  table.hashtag <- c()
  
  # Analizamos cada tweet, y vamos sumando a la frecuencia
  for(j in 1:nrow(tweets.hashtag)){
    tweet <- tweets.hashtag[j, ]   
    row <- c()
    row$date <- tweet$date
    row$time <- tweet$time
    
    # Vamos caso
    if(tweet$user_screen_name %in% leaders) {
      row$type <- "leaders"
      row$contador <- contador.leaders
      contador.leaders <- contador.leaders + 1
    }
    else if(tweet$user_screen_name %in% orgs){
      row$type <- "orgs"
      row$contador <- contador.orgs
      contador.orgs <- contador.orgs + 1
    }
    else if(tweet$user_screen_name %in% movs){
      row$type <- "movs"
      row$contador <- contador.movs
      contador.movs <- contador.movs + 1
    }
    else if(tweet$user_screen_name %in% medios){
      row$type <- "media"
      row$contador <- contador.medios  
      contador.medios <- contador.medios + 1
    }
    else if(tweet$user_screen_name %in% famosos){
      row$type <- "celebrities"
      row$contador <- contador.famosos
      contador.famosos <- contador.famosos + 1
    }
    else {
      row$type <- "people"
      row$contador <- contador.people
      contador.people <- contador.people + 1
    }
    
    row <- as.data.frame(row)
    table.hashtag <- rbind(table.hashtag, row)
    
    rm(row)
  }
  
  # Colocamos el nombre
  colnames(table.hashtag) <- c('date', 'time', 'type', 'count')
  
  # Generamos la fecha
  table.hashtag$datetime = as.POSIXct(paste(table.hashtag$date, table.hashtag$time), format="%Y-%m-%d %H:%M:%S")  
  
  if(nrow(table.hashtag) > 1)
  {
    # Plot
    p1 <- ggplot()
    
    if(nrow(table.hashtag[(table.hashtag$type == "people"), ]) > 1){
      p1 <- p1 + geom_line(data = table.hashtag[(table.hashtag$type == "people"), ], aes(x = datetime, y = count, color = "People"))
    } else {
      p1 <- p1 + geom_blank(data = NULL, aes(color = "People")) 
    }  
    
    if(nrow(table.hashtag[(table.hashtag$type == "leaders"), ]) > 1){
      p1 <- p1 + geom_line(data = table.hashtag[(table.hashtag$type == "leaders"), ], size = 1.4, aes(x = datetime, y = count, color = "Leaders"))
    } else {
      p1 <- p1 + geom_blank(data = NULL, aes(color = "Leaders")) 
    }  
    
    if(nrow(table.hashtag[(table.hashtag$type == "orgs"), ]) > 1){
      p1 <- p1 + geom_line(data = table.hashtag[(table.hashtag$type == "orgs"), ], size = 1.4, aes(x = datetime, y = count, color = "Organizations"))
    } else {
      p1 <- p1 + geom_blank(data = NULL, aes(color = "Organizations")) 
    }
    
    if(nrow(table.hashtag[(table.hashtag$type == "movs"), ]) > 1){
      p1 <- p1 + geom_line(data = table.hashtag[(table.hashtag$type == "movs"), ], aes(x = datetime, y = count, color = "Movements"))
    } else {
      p1 <- p1 + geom_blank(data = NULL, aes(color = "Movements")) 
    } 
    
    if(nrow(table.hashtag[(table.hashtag$type == "media"), ]) > 1){
      p1 <- p1 + geom_line(data = table.hashtag[(table.hashtag$type == "media"), ], aes(x = datetime, y = count, color = "Media"))
    } else {
      p1 <- p1 + geom_blank(data = NULL, aes(color = "Media")) 
    }
    
    if(nrow(table.hashtag[(table.hashtag$type == "celebrities"), ]) > 1){
      geom_line(data = table.hashtag[(table.hashtag$type == "celebrities"), ], aes(x = datetime, y = count, color = "Celebrities"))
    } else {
      p1 <- p1 + geom_blank(data = NULL, aes(color = "Celebrities")) 
    } 
    
    p1 <- p1 + xlab("") + ylab("Accumulated tweets per group (log scale)") + labs(colour = "Types") + scale_y_log10() +
      theme(legend.position="bottom",legend.direction="horizontal", panel.background = element_rect(fill = "white")) +
      ggtitle(i)
    
    # Guardamos el grafico  
    ggsave(paste0("../data/plots/starting/",i,".pdf",sep = ""), plot = p1)
    assign(paste("plot.", i, sep = ""), p1)
  }
  
  # Guardamos la tabla
  assign(paste("table.hashtag", i, sep = ""), table.hashtag)
  write.csv(table.hashtag, paste("../data/enero2016/starting/", i, ".csv", sep = ""), row.names=TRUE)
  
  # Removemos las variables
  rm(tweets.hashtag, table.hashtag, contador.leaders, contador.movs, contador.orgs, contador.people, contador.famosos, contador.medios)  
}


# Vemos a los líderes
diputados <- c('GiorgioJackson', 'camila_vallejo', 'gabrielboric')
q <- paste("SELECT CAST(h.created_at AS DATE) as date, CAST(h.created_at AS TIME) as time, h.user_screen_name, h.user_followers_count
           FROM hashtags_time as h
           WHERE ommit = 0 AND h.user_screen_name IN ('GiorgioJackson', 'camila_vallejo', 'gabrielboric')
           ORDER BY h.user_screen_name, h.created_at",sep="")
tweets.diputados <- dbGetQuery(con, q)
tweets.diputados <- na.omit(tweets.diputados)
tweets.diputados$datetime = as.POSIXct(paste(tweets.diputados$date, tweets.diputados$time), format="%Y-%m-%d %H:%M:%S")

p2 <- ggplot(data = tweets.diputados, aes(x = datetime, y = user_followers_count, colour=user_screen_name)) +
      geom_line() + xlab("") + ylab("Number of followers") + labs(colour = "Types") + 
      theme(legend.position="bottom",legend.direction="horizontal", panel.background = element_rect(fill = "white")) +
      ggtitle("Evolution followers upon time")
p2
