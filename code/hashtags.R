folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')

# Obtenemos los tweets de los hashtags
q <- paste('SELECT source, target, hashtag, type  
           FROM hashtag_user_network',sep="")
tweets <- dbGetQuery(con, q)

# Obtenemos los hashtags
hashtags <- unique(unlist(tweets$hashtag, use.names = FALSE))
types <- unique(unlist(tweets$type, use.names = FALSE))

# Variables iniciales
hashtagResume <- c()
RatioInAllNetwork <- c()

# Calculamos las métricas por cada marcha
for(i in hashtags)
{
  for(j in types)
  {
    tweets.protest <- tweets[tweets$hashtag == i & tweets$type == j,]
    
    if(nrow(tweets.protest) > 0)
    {
      network <- graph.data.frame(tweets.protest, directed=TRUE)
      
      # Sacamos los valores
      vcount <- vcount(network) # Number of nodes
      ecount <- ecount(network) # Number of edges
      
      # Sacamos los coeficientes por conferencia
      degree <- mean(degree(network))
      indegree <- mean(degree(network,mode="in"))
      outdegree <- mean(degree(network,mode="out"))
      closeness <- mean(closeness(network))
      betweenness <- mean(betweenness(network,normalized=TRUE))
      hub <- mean(hub.score(network)$vector)
      authority <- mean(authority.score(network)$vector)
      clusterCoefficient <- mean(transitivity(network, type="local"),na.rm=TRUE)
      egenvector.centrality <- mean(evcent(network)$vector)
      page.rank <- mean(page.rank(network)$vector)
      density <- mean(graph.density(network))
      reciprocity <- mean(reciprocity(network))
      wcc <- mean(clusters(network, mode="weak")$no)
      
      RatioInNetwork <- cbind.data.frame(i, j, vcount, ecount, degree, indegree, outdegree, closeness, betweenness, hub, authority, clusterCoefficient, egenvector.centrality, page.rank, density, reciprocity, wcc)
      RatioInNetwork <- as.data.frame(RatioInNetwork)
      RatioInNetwork[is.na(RatioInNetwork)] <- 0   
      
      if(nrow(RatioInNetwork) > 0){
        colnames(RatioInNetwork) <- c("hashtag", "type", "vcount", "ecount", "degree", "indegree", "outdegree", "closeness", "betweenness", "hub", "authority", "clusterCoefficient", "egenvector.centrality", "page.rank", "density", "reciprosity", "wcc")
        hashtagResume <- rbind(hashtagResume, RatioInNetwork)
      }
      
      # Sacamos las estadísticas descriptivas
      degree <- as.data.frame(degree(network))
      indegree <- as.data.frame(degree(network,mode="in"))
      outdegree <- as.data.frame(degree(network,mode="out"))
      closeness <- as.data.frame(closeness(network))
      betweenness <- as.data.frame(betweenness(network,normalized=TRUE))
      hub <- as.data.frame(hub.score(network)$vector)
      authority <- as.data.frame(authority.score(network)$vector)
      clusterCoefficient <- as.data.frame(transitivity(network, type="local"))
      egenvector.centrality <- as.data.frame(evcent(network)$vector)
      page.rank <- as.data.frame(page.rank(network)$vector)
      
      # Vemos por los usuarios
      user <- rownames(degree)    
      degree <- (degree[,1] - min(degree)) / (max(degree) - min(degree))
      indegree <-  (indegree[,1] - min(indegree)) / (max(indegree) - min(indegree))
      outdegree <-  (outdegree[,1] - min(outdegree)) / (max(outdegree) - min(outdegree))
      closeness <- (closeness[,1] - min(closeness)) / (max(closeness) - min(closeness))
      betweenness <- (betweenness[,1] - min(betweenness)) / (max(betweenness) - min(betweenness))
      hub <- (hub[,1] - min(hub)) / (max(hub) - min(hub))
      authority <- (authority[,1] - min(authority)) / (max(authority) - min(authority))
      clusterCoefficient[is.na(clusterCoefficient),1] <- 0
      clusterCoefficient <- (clusterCoefficient[,1] - min(clusterCoefficient)) / (max(clusterCoefficient) - min(clusterCoefficient))
      egenvector.centrality <- (egenvector.centrality[,1] - min(egenvector.centrality)) / (max(egenvector.centrality) - min(egenvector.centrality))
      page.rank <- (page.rank[,1] - min(page.rank)) / (max(page.rank) - min(page.rank))
      
      # Construimos la red
      RatioInNetwork <- cbind.data.frame(user, degree, indegree, outdegree, closeness, betweenness, hub, authority, clusterCoefficient, egenvector.centrality, page.rank)
      RatioInNetwork <- as.data.frame(RatioInNetwork)
      RatioInNetwork[is.na(RatioInNetwork)] <- 0   
      
      if(nrow(RatioInNetwork) > 0){
        RatioInNetwork$hashtag <- as.character(i)
        RatioInNetwork$type <- as.character(j)
        colnames(RatioInNetwork) <- c("user", "degree", "indegree", "outdegree", "closeness", "betweenness", "hub", "authority", "clusterCoefficient", "egenvector.centrality", "page.rank", "hashtag", "type")
        RatioInAllNetwork <- rbind(RatioInAllNetwork,RatioInNetwork)
      }
    }
  }
}

write.csv(format(hashtagResume, digits=5), "../data/december/descriptive/descriptive.csv", row.names=TRUE)
write.csv(format(RatioInAllNetwork, digits=5), "../data/december/descriptive/users.csv", row.names=TRUE)
