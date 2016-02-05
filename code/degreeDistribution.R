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
           FROM hashtag_user_network',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$hashtag, use.names = FALSE)))

# Reordenamos
tweets$id <- NULL


# Dejamos solo los que nos interesan
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i) , ]
  tweets.hashtag.network <- tweets.hashtag[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweets.hashtag.network, directed=TRUE)
  
  # Obtenemos el resto
  tweets.people <- V(network)[!((V(network)$name %in% orgs) | (V(network)$name %in% leaders) | (V(network)$name %in% movs) | (V(network)$name %in% medios) | (V(network)$name %in% famosos))]$name
  people <- unique(unlist(tweets.people, use.names = FALSE))
  
  # Obtenemos la distribución
  degree.in <- degree(network, V(network), mode="in")
  
  # Obtenemos las distribuciones separadas
  degree.in.leaders <- na.omit(degree.in[leaders])
  degree.in.orgs <- na.omit(degree.in[orgs])
  degree.in.people <- na.omit(degree.in[people])
  degree.in.movs <- na.omit(degree.in[movs])
  degree.in.medios <- na.omit(degree.in[medios])
  degree.in.famosos <- na.omit(degree.in[famosos])
  
  degree.in.leaders[degree.in.leaders==0] <- NA
  degree.in.orgs[degree.in.orgs==0] <- NA
  degree.in.people[degree.in.people==0] <- NA
  degree.in.movs[degree.in.movs==0] <- NA
  degree.in.medios[degree.in.medios==0] <- NA
  degree.in.famosos[degree.in.famosos==0] <- NA
  
  degree.in.leaders <- na.omit(degree.in.leaders)
  degree.in.orgs <- na.omit(degree.in.orgs)
  degree.in.people <- na.omit(degree.in.people)
  degree.in.movs <- na.omit(degree.in.movs)
  degree.in.medios <- na.omit(degree.in.medios)
  degree.in.famosos <- na.omit(degree.in.famosos)
  
  # Creamos el grafico
  p1 <- ggplot()
    
  # Degree de los líderes
  if(length(degree.in.leaders) > 0){
    degree.in.leaders.df <- data.frame(table(degree=factor(degree.in.leaders, levels=seq_len(max(degree.in.leaders)))))
    degree.in.leaders.df$degree <- as.numeric(as.character(degree.in.leaders.df$degree))
    degree.in.leaders.df <- degree.in.leaders.df[(degree.in.leaders.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.leaders.df, aes(x=degree, y=Freq, color = "Leaders", shape="Leaders")) 
  } 
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Leaders", shape="Leaders")) 
  }
  
  # Degree de las organizaciones
  if(length(degree.in.orgs) > 0){
    degree.in.orgs.df <- data.frame(table(degree=factor(degree.in.orgs, levels=seq_len(max(degree.in.orgs)))))
    degree.in.orgs.df$degree <- as.numeric(as.character(degree.in.orgs.df$degree))
    degree.in.orgs.df <- degree.in.orgs.df[(degree.in.orgs.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.orgs.df, aes(x=degree, y=Freq, color = "Organizations", shape="Organizations"))
  }
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Organizations", shape="Organizations")) 
  }
  
  # Degree de los movimientos
  if(length(degree.in.movs) > 0){
    degree.in.movs.df <- data.frame(table(degree=factor(degree.in.movs, levels=seq_len(max(degree.in.movs)))))
    degree.in.movs.df$degree <- as.numeric(as.character(degree.in.movs.df$degree))
    degree.in.movs.df <- degree.in.movs.df[(degree.in.movs.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.movs.df, aes(x=degree, y=Freq, color = "Movements", shape="Movements"))
  } 
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Movements", shape="Movements")) 
  }
  
  # Degree de los medios
  if(length(degree.in.medios) > 0){
    degree.in.medios.df <- data.frame(table(degree=factor(degree.in.medios, levels=seq_len(max(degree.in.medios)))))
    degree.in.medios.df$degree <- as.numeric(as.character(degree.in.medios.df$degree))
    degree.in.medios.df <- degree.in.medios.df[(degree.in.medios.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.medios.df, aes(x=degree, y=Freq, color = "Media", shape="Media"))
  }
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Media", shape="Media")) 
  }
  
  # Degree de los famosos
  if(length(degree.in.famosos) > 0){
    degree.in.famosos.df <- data.frame(table(degree=factor(degree.in.famosos, levels=seq_len(max(degree.in.famosos)))))
    degree.in.famosos.df$degree <- as.numeric(as.character(degree.in.famosos.df$degree))
    degree.in.famosos.df <- degree.in.famosos.df[(degree.in.famosos.df$Freq != 0),] 
    p1 <- p1 + geom_point(data = degree.in.famosos.df, aes(x=degree, y=Freq, color = "Celebrities", shape="Celebrities"))
  }
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "Celebrities", shape="Celebrities")) 
  }
  
  # Degree de los normales
  if(length(degree.in.people) > 0){
    degree.in.people.df <- data.frame(table(degree=factor(degree.in.people, levels=seq_len(max(degree.in.people)))))
    degree.in.people.df$degree <- as.numeric(as.character(degree.in.people.df$degree))  
    degree.in.people.df <- degree.in.people.df[(degree.in.people.df$Freq != 0),]
    p1 <- p1 + geom_point(data = degree.in.people.df, aes(x=degree, y=Freq, color = "People", shape="People"))
  } 
  else {
    p1 <- p1 + geom_blank(data = NULL, aes(color = "People", shape="People")) 
  }
  
  # Plot
  p1 <- p1 +
  xlab("Indegree") + ylab("Freq") + labs(colour = "Types", shape = "Types") +
  scale_y_log10() +
  theme(legend.position="bottom", legend.direction="horizontal") +
  ggtitle(i) 
  
  # Guardamos la tabla
  data.degree <- data.frame(user=names(degree.in), degree=degree.in, row.names=NULL)
  data.degree <- data.degree[order(-data.degree$degree),]
  #assign(paste("degree.", i, sep = ""), data.degree) 
  write.csv(format(data.degree, digits=5), paste("../data/enero2016/degree.", i, ".csv", sep = ""), row.names=TRUE)
  
  ggsave(paste0("../data/plots/hashtags/",i,".pdf",sep = ""), plot = p1)
  
  # Removemos las variables
  rm(tweets.hashtag, tweets.hashtag.network, tweets.people, network, degree.in.people, degree.in.orgs, degree.in.leaders, degree.in.famosos, degree.in.medios, degree.in.movs, degree.in.people.df, degree.in.orgs.df, degree.in.leaders.df, degree.in.famosos.df, degree.in.medios.df, degree.in.movs.df, degree.in, data.degree)  
}