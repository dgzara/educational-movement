folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')

# Obtenemos los tweets dirigidos
q <- paste('SELECT *  
             FROM menciones',sep="")
tweets <- dbGetQuery(con, q)

# Formato
tweets$account <- substring(tweets$account, 2) # Eliminamos el @ del principio
tweets <- tweets[c("id", "tweetuser", "account", "tweet", "tweetdate")]
colnames(tweets) <- c("id", "source", "target", "tweet", "date")

# Obtenemos los usuarios
users.source <- sort(unique(unlist(tweets$source, use.names = FALSE)))
users.target <- sort(unique(unlist(tweets$target, use.names = FALSE)))
users <- c(users.source, users.target)
users <- as.data.frame(sort(unique(unlist(users, use.names = FALSE))))
colnames(users) <- c("username")
rm(users.source, users.target)
users <- na.omit(users)

# Revisamos por año
networks <- c()
descriptive <- c()
descriptive.acumulado <- c()
types.metrics.m.acumulado <- c()
types.metrics.sd.acumulado <- c()
users.metrics <- c()

for(i in 2010:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Sacamos los valores
  vcount <- vcount(network) # Number of nodes
  ecount <- ecount(network) # Number of edges
  density <- graph.density(network)
  reciprocity <- reciprocity(network, mode="default")
  degree <- degree(network)
  
  # Calculamos las metricas por usuarios
  users.metrics <- getUsersNetworkMetrics2(network)
  
  # Imprimimos las estadisticas descriptivas
  descriptive <- c()
  descriptive <- rbind(descriptive, c(i, vcount, ecount, density, reciprocity))
  descriptive <- as.data.frame(descriptive)
  colnames(descriptive) <- c("year", "vcount", "ecount", "density", "reciprocity")
  
  # Calculamos los valores descriptivos por grupos
  leaders.metrics <- na.omit(users.metrics[(users.metrics$username %in% leaders),])
  orgs.metrics <- na.omit(users.metrics[(users.metrics$username %in% orgs),])
  celebrities.metrics <- na.omit(users.metrics[(users.metrics$username %in% famosos),])
  movs.metrics <- na.omit(users.metrics[(users.metrics$username %in% movs),])
  media.metrics <- na.omit(users.metrics[(users.metrics$username %in% medios),])
  people.metrics <- na.omit(users.metrics[!(users.metrics$username %in% leaders | users.metrics$username %in% orgs | users.metrics$username %in% famosos | users.metrics$username %in% movs | users.metrics$username %in% medios),])
  
  # Calculamos para los grupos
  leaders.metrics$username <- NULL
  orgs.metrics$username <- NULL
  media.metrics$username <- NULL
  movs.metrics$username <- NULL
  celebrities.metrics$username <- NULL
  people.metrics$username <- NULL
  
  # Generamos la tabla de medias
  types.m <- c()
  types.m <- rbind(types.m, colMeans(leaders.metrics))
  types.m <- rbind(types.m, colMeans(orgs.metrics))
  types.m <- rbind(types.m, colMeans(people.metrics))
  types.m <- rbind(types.m, colMeans(media.metrics))
  types.m <- rbind(types.m, colMeans(movs.metrics))
  types.m <- rbind(types.m, colMeans(celebrities.metrics))
  types.m <- as.data.frame(types.m)
  rownames(types.m) <- c("leaders", "orgs", "people", "media", "movs", "celebrities")
  
  # Generamos la tabla de desviacion 
  types.sd <- c()
  types.sd <- rbind(types.sd, apply(leaders.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(orgs.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(media.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(people.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(movs.metrics, 2, sd))
  types.sd <- rbind(types.sd, apply(celebrities.metrics, 2, sd))
  types.sd <- as.data.frame(types.sd)
  rownames(types.sd) <- c("leaders", "orgs", "people", "media", "movs", "celebrities")
  
  # Asignamos las variables
  assign(paste("users.metrics.", i, sep = ""), users.metrics)
  assign(paste("descriptive", i, sep = ""), descriptive)
  assign(paste("types.m.", i, sep = ""), types.m)
  assign(paste("types.sd.", i, sep = ""), types.sd)
  
  # Cerramos
  descriptive.acumulado <- rbind(descriptive.acumulado, descriptive)
  types.metrics.m.acumulado <- rbind(types.metrics.m.acumulado, types.m)
  types.metrics.sd.acumulado <- rbind(types.metrics.sd.acumulado, types.sd)
  rm(descriptive, users.metrics, types.m, types.sd)
}

# Escribimos
write.csv(round(orderByPageRank(users.metrics.2010), 3),"../data/enero2016/table2/complete.2010.csv",row.names=TRUE)
write.csv(round(orderByPageRank(users.metrics.2011), 3),"../data/enero2016/table2/complete.2011.csv",row.names=TRUE)
write.csv(round(orderByPageRank(users.metrics.2012), 3),"../data/enero2016/table2/complete.2012.csv",row.names=TRUE)
write.csv(round(orderByPageRank(users.metrics.2013), 3),"../data/enero2016/table2/complete.2013.csv",row.names=TRUE)

# Escribimos
write.csv(round(types.m.2010, 3),"../data/enero2016/table2/type.metrics.m.2010.csv",row.names=TRUE)
write.csv(round(types.m.2011, 3),"../data/enero2016/table2/type.metrics.m.2011.csv",row.names=TRUE)
write.csv(round(types.m.2012, 3),"../data/enero2016/table2/type.metrics.m.2012.csv",row.names=TRUE)
write.csv(round(types.m.2013, 3),"../data/enero2016/table2/type.metrics.m.2013.csv",row.names=TRUE)

# Escribimos
write.csv(round(types.sd.2010, 3),"../data/enero2016/table2/type.metrics.sd.2010.csv",row.names=TRUE)
write.csv(round(types.sd.2011, 3),"../data/enero2016/table2/type.metrics.sd.2011.csv",row.names=TRUE)
write.csv(round(types.sd.2012, 3),"../data/enero2016/table2/type.metrics.sd.2012.csv",row.names=TRUE)
write.csv(round(types.sd.2013, 3),"../data/enero2016/table2/type.metrics.sd.2013.csv",row.names=TRUE)

# Escribimos los acumulados
write.csv(round(types.metrics.m.acumulado, 3),"../data/enero2016/table2/types.metrics.m.acumulado.csv",row.names=TRUE)
write.csv(round(types.metrics.sd.acumulado, 3),"../data/enero2016/table2/types.metrics.sd.acumulado.csv",row.names=TRUE)
xtable(format(types.metrics.m.acumulado, 3))
xtable(format(types.metrics.sd.acumulado, 3))

# Escribimos
write.csv(round(descriptive.acumulado, 3),"../data/enero2016/table2/descriptive.csv",row.names=TRUE)
xtable(round(descriptive.acumulado, 3))

# Guardamos la tabla
final <- matrix(0, nrow = ncol(types.metrics.m.acumulado)*6, ncol = 5)
final <- as.data.frame(final)
colnames(final) <- c("group", "2010", "2011", "2012", "2013")
#colnames(final) <- colnames(types.metrics.m.acumulado)

for(i in 1:nrow(types.metrics.m.acumulado))
{
  for(j in 1:ncol(types.metrics.m.acumulado))
  {
    new_i <- 1 + (i-1)%%6 + (j-1)*6
    new_j <- 2 + trunc((i-1)/6)
    
    if(new_j < 3){
      final[new_i, 1] <- rownames(types.metrics.m.acumulado[i,])
    }
    
    final[new_i,new_j] <- paste("$ ", round(types.metrics.m.acumulado[i,j],3)," pm{",round(types.metrics.sd.acumulado[i,j],3), "} $", sep="")
  }
}

# Damos el formato final a la tabla
final$metric <- colnames(types.metrics.m.acumulado)[1]
final[7:12, ]$metric <- colnames(types.metrics.m.acumulado)[2]
final[13:18, ]$metric <- colnames(types.metrics.m.acumulado)[3]
final[19:24, ]$metric <- colnames(types.metrics.m.acumulado)[4]
final[25:30, ]$metric <- colnames(types.metrics.m.acumulado)[5]
final[31:36, ]$metric <- colnames(types.metrics.m.acumulado)[6]
final <- final[, c("metric", "group", "2010", "2011", "2012", "2013")]
xtable(final)
