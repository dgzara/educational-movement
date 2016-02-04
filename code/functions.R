getNetworkMetrics <- function(x) {
  RatioInAllNetwork <- c()
  x <- x[,c(1,2)]
  if(nrow(x) > 0){
    network <- graph.data.frame(x,directed=TRUE)
    
    # Sacamos los valores
    vcount = vcount(network) # Number of nodes
    ecount = ecount(network) # Number of edges
    
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
    
    RatioInNetwork <- cbind.data.frame(vcount, ecount, degree, indegree, outdegree, closeness, betweenness, hub, authority, clusterCoefficient, egenvector.centrality, page.rank, density, reciprocity, wcc)
    RatioInNetwork <- as.data.frame(RatioInNetwork)
    RatioInNetwork[is.na(RatioInNetwork)] <- 0   
    
    if(nrow(RatioInNetwork) > 0){
      RatioInNetwork$conference <- as.character(i)
      colnames(RatioInNetwork) <- c("vcount", "ecount", "degree", "indegree", "outdegree", "closeness", "betweenness", "hub", "authority", "clusterCoefficient", "egenvector.centrality", "page.rank", "density", "reciprosity", "wcc")
      RatioInAllNetwork <- rbind(RatioInAllNetwork, RatioInNetwork)
    }
    
    return(RatioInAllNetwork)
  }
}

getUsersNetworkMetrics <- function (network){
  
  RatioInAllNetwork <- c()
  
  # Calculamos la red
  degree <- as.data.frame(degree(network))
  indegree <- as.data.frame(degree(network,mode="in"))
  outdegree <- as.data.frame(degree(network,mode="out"))
  closeness <- as.data.frame(closeness(network))
  betweenness <- as.data.frame(betweenness(network, normalized=TRUE))
  hub <- as.data.frame(hub.score(network)$vector)
  authority <- as.data.frame(authority.score(network)$vector)
  clusterCoefficient <- as.data.frame(transitivity(network, type="local"))
  egenvector.centrality <- as.data.frame(evcent(network)$vector)
  page.rank <- as.data.frame(page.rank(network)$vector)
  
  # Calculamos las métricas por usuario
  username <- rownames(degree)    
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
  
  # Armamos la tabla
  RatioInNetwork <- cbind.data.frame(username, degree, indegree,outdegree,closeness,betweenness,hub,authority,clusterCoefficient,egenvector.centrality,page.rank)
  RatioInNetwork <- as.data.frame(RatioInNetwork)
  RatioInNetwork[is.na(RatioInNetwork)] <- 0   
  
  if(nrow(RatioInNetwork) > 0){
    colnames(RatioInNetwork) <- c("username","degree","indegree","outdegree","closeness","betweenness", "hub", "authority", "clusterCoefficient", "egenvector.centrality", "page.rank")
    RatioInAllNetwork <- rbind(RatioInAllNetwork, RatioInNetwork)
  }
  
  return(as.data.frame(RatioInAllNetwork))
}

getTopTenPageRank <- function(x){
  x <- x[order(-x$page.rank),]
  x <- x[1:10, ]
  return(x)
}

orderByPageRank <- function(x){
  x <- x[order(-x$page.rank),]
  return(x)
}

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

sem <- function(x) sd(x)/sqrt(length(x))

normalizar <- function(x) {
  if(length(x) > 0 && (max(x,na.rm=TRUE) - min(x, na.rm=TRUE)) != 0){
    return((x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE)))
  } 
  return(x)
}