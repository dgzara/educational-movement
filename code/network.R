folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')

# Obtenemos los tweets dirigidos
q <- paste('SELECT *  
             FROM menciones',sep="")
tweets <- dbGetQuery(con, q)

# Formato
tweets$account <- substring(tweets$account, 2) # Eliminamos el @ del principio
tweets <- tweets[c("id", "tweetuser", "account", "tweet", "tweetdate")]
colnames(tweets) <- c("id", "source", "target", "tweet", "date")

# Revisamos por año
networks <- c()
descriptive <- c()
users <- c()

for(i in 2009:2013){
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  
  # Sacamos los valores
  vcount = vcount(network) # Number of nodes
  ecount = ecount(network) # Number of edges
  clique <- clique.number(network) # Size of the largest clique
  density <- graph.density(network)
  reciprocity <- reciprocity(network, mode="default")
  degree <- degree(network)
  
  # Imprimimos las estadisticas descriptivas
  descriptive <- rbind(descriptive, c(i, vcount, ecount, clique, density, reciprocity))
  
  # Calculamos las metricas por usuarios
  users <- getUsersNetworkMetrics(network)
  assign(paste("users", i, sep = ""), users)
  write.csv(users, paste0(paste0("../data/users.", i), ".csv"),row.names=TRUE)
  
  # Degree distribution for a network - Original Scale
  h <- hist(log(degree),col="blue", xlab="Degree", ylab="Frequency", main= paste0("Degree Distribution ", i))
  h.log <- hist(degree,col="blue", xlab="Degree", ylab="Frequency", main= paste0("Degree Distribution x-log scale ", i))
  
  # Degree distribution for a network - log/log scale
  dd.network <- degree.distribution(network)
  d <- 1:max(degree)-1
  ind <- (dd.network != 0)
  plot(d[ind], dd.network[ind], log="xy", col="blue", xlab=c("Log-Degree"), ylab=c("Log-Intensity"), main=paste0("Log-Log Degree Distribution ", i))
  
  # Average neighbor degree versus vertex degree (log–log scale)
  network.simplified <- simplify(network)
  a.nn.deg.network <- graph.knn(network.simplified,V(network.simplified))$knn 
  d.network.simplified <- degree(network.simplified)
  plot(d.network.simplified, a.nn.deg.network, log="xy", col="goldenrod", xlab=c("Log Vertex Degree"), ylab=c("Log Average Neighbor Degree"), main=paste0("Average neighbor degree versus vertex degree ", i))
}

# Escribimos
descriptive <- as.data.frame(descriptive)
colnames(descriptive) <- c("year", "vcount", "ecount", "clique", "density", "reciprocity")
write.csv(descriptive,"../data/descriptive.csv",row.names=TRUE)

# Obtenemos los TopTen
write.csv(format(getTopTenPageRank(users2009), digits=2),"../data/top2009.csv",row.names=TRUE)
write.csv(format(getTopTenPageRank(users2010), digits=2),"../data/top2010.csv",row.names=TRUE)
write.csv(format(getTopTenPageRank(users2011), digits=2),"../data/top2011.csv",row.names=TRUE)
write.csv(format(getTopTenPageRank(users2012), digits=2),"../data/top2012.csv",row.names=TRUE)
write.csv(format(getTopTenPageRank(users2013), digits=2),"../data/top2013.csv",row.names=TRUE)