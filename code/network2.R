folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')

# Obtenemos los tweets dirigidos
q <- paste('SELECT *  
             FROM menciones',sep="")
tweets <- dbGetQuery(con, q)

# Obtenemos los usuarios
q <- paste('SELECT *  
           FROM users',sep="")
users <- dbGetQuery(con, q)

# Formato
tweets$account <- substring(tweets$account, 2) # Eliminamos el @ del principio
tweets <- tweets[c("id", "tweetuser", "account", "tweet", "tweetdate")]
colnames(tweets) <- c("id", "source", "target", "tweet", "date")

# Revisamos por año
networks <- c()
descriptive <- c()
descriptive.acumulado <- c()
types.metrics.m.acumulado <- c()
types.metrics.sem.acumulado <- c()
users.metrics <- c()
leaders <- c()

for(i in 2010:2013){
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Sacamos los valores
  vcount = vcount(network) # Number of nodes
  ecount = ecount(network) # Number of edges
  density <- graph.density(network)
  reciprocity <- reciprocity(network, mode="default")
  degree <- degree(network)
  
  # Vemos los grupos
  leaders <- users[(users$leader_year <= i & users$leader_year != 0) , ]
  institutions <- users[(users$institution == 1) , ]
  people <- users[(users$institution == 0 & users$leader_year == 0) , ]
  
  # Calculamos las metricas por usuarios
  users.metrics <- getUsersNetworkMetrics(network)
  
  # Si es institución, lo seteamos como 2
  V(network)$type <- as.integer(users$institution[match(V(network)$name, users$name)]) + 1   
  
  # Calculamos la asortatividad
  V(network)$type[is.na(V(network)$type)] <- 1
  assortativity.institutions <- assortativity.nominal(network, types=V(network)$type)
  
  # Determinamos quienes fueron líderes ese año, los seteamos como 3.
  for(j in 1:nrow(leaders)){
    V(network)[V(network)$name == leaders[j, ]$name ]$type <- 3
  }
  
  # Calculamos la asortatividad
  V(network)$type[is.na(V(network)$type)] <- 1
  assortativity.leaders <- assortativity.nominal(network, types=V(network)$type)
  
  # Imprimimos las estadisticas descriptivas
  descriptive <- c()
  descriptive <- rbind(descriptive, c(i, vcount, ecount, density, reciprocity, assortativity.institutions, assortativity.leaders))
  descriptive <- as.data.frame(descriptive)
  colnames(descriptive) <- c("year", "vcount", "ecount", "density", "reciprocity", "assortativity.institutions", "assortativity.leaders")
  
  # Calculamos los valores descriptivos por grupos
  leaders.metrics <- na.omit(users.metrics[match(leaders$name, users.metrics$username),])
  institutions.metrics <- na.omit(users.metrics[match(institutions$name, users.metrics$username),])
  people.metrics <- na.omit(users.metrics[match(people$name, users.metrics$username),])
  
  leaders.metrics$username <- NULL
  leaders.metrics.m <- colMeans(leaders.metrics)
  leaders.metrics.sem <- apply(leaders.metrics, 2, sem)
  
  institutions.metrics$username <- NULL
  institutions.metrics.m <- colMeans(institutions.metrics)
  institutions.metrics.sem <- apply(institutions.metrics, 2, sem)
  
  people.metrics$username <- NULL
  people.metrics.m <- colMeans(people.metrics)
  people.metrics.sem <- apply(people.metrics, 2, sem)
  
  # Generamos la tabla de medias
  types.m <- c()
  types.m <- rbind(types.m, leaders.metrics.m)
  types.m <- rbind(types.m, institutions.metrics.m)
  types.m <- rbind(types.m, people.metrics.m)
  types.m <- as.data.frame(types.m)
  
  # Generamos la tabla de errores 
  types.sem <- c()
  types.sem <- rbind(types.sem, leaders.metrics.sem)
  types.sem <- rbind(types.sem, institutions.metrics.sem)
  types.sem <- rbind(types.sem, people.metrics.sem)
  types.sem <- as.data.frame(types.sem)
  
  # Asignamos las variables
  assign(paste("users.metrics.", i, sep = ""), users.metrics)
  assign(paste("descriptive", i, sep = ""), descriptive)
  assign(paste("types.m.", i, sep = ""), types.m)
  assign(paste("types.sem.", i, sep = ""), types.sem)
  
  # Cerramos
  descriptive.acumulado <- rbind(descriptive.acumulado, descriptive)
  types.metrics.m.acumulado <- rbind(types.metrics.m.acumulado, types.m)
  types.metrics.sem.acumulado <- rbind(types.metrics.sem.acumulado, types.sem)
  rm(descriptive, users.metrics, types.m, types.sem)
}

# Escribimos
write.csv(format(orderByPageRank(users.metrics.2010), digits=2),"../data/december/complete.2010.csv",row.names=TRUE)
write.csv(format(orderByPageRank(users.metrics.2011), digits=2),"../data/december/complete.2011.csv",row.names=TRUE)
write.csv(format(orderByPageRank(users.metrics.2012), digits=2),"../data/december/complete.2012.csv",row.names=TRUE)
write.csv(format(orderByPageRank(users.metrics.2013), digits=2),"../data/december/complete.2013.csv",row.names=TRUE)

# Escribimos
write.csv(format(types.m.2010, digits=5),"../data/december/type.metrics.m.2010.csv",row.names=TRUE)
write.csv(format(types.m.2011, digits=5),"../data/december/type.metrics.m.2011.csv",row.names=TRUE)
write.csv(format(types.m.2012, digits=5),"../data/december/type.metrics.m.2012.csv",row.names=TRUE)
write.csv(format(types.m.2013, digits=5),"../data/december/type.metrics.m.2013.csv",row.names=TRUE)

# Escribimos
write.csv(format(types.sem.2010, digits=5),"../data/december/type.metrics.sem.2010.csv",row.names=TRUE)
write.csv(format(types.sem.2011, digits=5),"../data/december/type.metrics.sem.2011.csv",row.names=TRUE)
write.csv(format(types.sem.2012, digits=5),"../data/december/type.metrics.sem.2012.csv",row.names=TRUE)
write.csv(format(types.sem.2013, digits=5),"../data/december/type.metrics.sem.2013.csv",row.names=TRUE)

# Escribimos
write.csv(format(descriptive.acumulado, digits=5),"../data/december/descriptive.csv",row.names=TRUE)

