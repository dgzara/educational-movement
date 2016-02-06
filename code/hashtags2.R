folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')

library(openair)
library(ggplot2)
library(scales)

# Obtenemos los tweets de los hashtags
q <- paste('SELECT *  
           FROM hashtag',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$account, use.names = FALSE)))

# Reordenamos
colnames(tweets) <- c("id", "hashtag", "user", "tweet", "date")
tweets$id <- NULL

# Dejamos solo los que nos interesan
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i) , ]
  
  # Generamos y ordenamos el arreglo de las fechas
  dates <- unique(unlist(tweets.hashtag$date, use.names = FALSE))
  dates <- dates[order(format(as.Date(dates),"%m%d"))]
  
  table.hashtag <- c()
  
  # Generamos la separacion de fechas
  for(j in 1:length(dates))
  {
    tweets.hashtag.date <- tweets.hashtag[(tweets.hashtag$date == dates[j]) , ]
    
    # Obtenemos los lideres
    tweets.leaders <- tweets.hashtag.date[(tweets.hashtag.date$user %in% leaders) , ]
    users.leaders <- unique(unlist(tweets.leaders$user, use.names = FALSE))
    
    # Obtenemos las organizaciones
    tweets.orgs <-  tweets.hashtag.date[(tweets.hashtag.date$user %in% orgs) , ]
    users.orgs <- unique(unlist(tweets.orgs$user, use.names = FALSE))
    
    # Obtenemos los movimientos
    tweets.movs <-  tweets.hashtag.date[(tweets.hashtag.date$user %in% movs) , ]
    users.movs <- unique(unlist(tweets.movs$user, use.names = FALSE))
    
    # Obtenemos los medios
    tweets.medios <- tweets.hashtag.date[(tweets.hashtag.date$user %in% medios) , ]
    users.medios <- unique(unlist(tweets.medios$user, use.names = FALSE))
    
    # Obtenemos las famosos
    tweets.celebrities <-  tweets.hashtag.date[(tweets.hashtag.date$user %in% famosos) , ]
    users.celebrities <- unique(unlist(tweets.celebrities$user, use.names = FALSE))
    
    # Obtenemos el resto
    tweets.people <- tweets.hashtag.date[(!(tweets.hashtag.date$user %in% orgs) && !(tweets.hashtag.date$user %in% leaders) && !(tweets.hashtag.date$user %in% movs) && !(tweets.hashtag.date$user %in% medios) && !(tweets.hashtag.date$user %in% famosos)), ]
    users.people <- unique(unlist(tweets.people$user, use.names = FALSE))
    
    # Armamos la fila
    row <- c()
    row$date <- dates[j]
    row$leaders <- if (length(users.leaders) > 0) nrow(tweets.leaders)/length(users.leaders) else 0
    row$orgs <- if (length(users.orgs) > 0) nrow(tweets.orgs)/length(users.orgs) else 0
    row$people <- if (length(users.people) > 0) nrow(tweets.people)/length(users.people) else 0
    row$movs <- if (length(users.movs) > 0) nrow(tweets.movs)/length(users.movs) else 0
    row$medios <- if (length(users.medios) > 0) nrow(tweets.medios)/length(users.medios) else 0
    row$celebrities <- if (length(users.celebrities) > 0) nrow(tweets.celebrities)/length(users.celebrities) else 0
    row <- as.data.frame(row, stringsAsFactors = FALSE)
    
    # Generamos la tabla
    table.hashtag <- rbind(table.hashtag, row)
    
    # Removemos
    rm(tweets.hashtag.date, tweets.people, tweets.orgs, tweets.leaders, tweets.movs, tweets.celebrities, tweets.medios, users.leaders, users.orgs, users.medios, users.people, users.celebrities, users.movs, row)
  }
  
  # Plot
  p1 <- ggplot() + 
    geom_line(data = table.hashtag, linetype="dashed", aes(x = date, y = people, color = "People")) +
    geom_line(data = table.hashtag, linetype="solid", size = 1.4, aes(x = date, y = leaders, color = "Leaders")) +
    geom_line(data = table.hashtag, linetype="dotdash", size = 1.4, aes(x = date, y = orgs, color = "Organizations")) +
    geom_line(data = table.hashtag, linetype="longdash", aes(x = date, y = movs, color = "Movements")) +
    geom_line(data = table.hashtag, linetype="twodash", aes(x = date, y = medios, color = "Media")) +
    geom_line(data = table.hashtag, linetype="solid", aes(x = date, y = celebrities, color = "Celebrities")) +
    #scale_x_date(breaks = date_breaks(width = "10 day")) + 
    xlab("") + ylab("Dialy tweets per user") +  labs(colour = "Types") +
    #scale_y_log10() +
    theme(legend.position="bottom",legend.direction="horizontal", panel.background = element_rect(fill = "white")) +
    ggtitle(i)
  
  ggsave(paste0("../data/plots/tweetsperuser/",i,".pdf",sep = ""), plot = p1)
  
  # Guardamos la tabla
  assign(paste("table.hashtag", i, sep = ""), table.hashtag)
  write.csv(format(table.hashtag, digits=5), paste("../data/enero2016/tweetsperuser/", i, ".csv", sep = ""), row.names=TRUE)
  
  # Removemos las variables
  rm(tweets.hashtag, dates, table.hashtag)  
}