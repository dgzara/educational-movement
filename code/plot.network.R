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

for(i in 2010:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)
  network <- simplify(network)  
  
  # Vemos los grupos
  leaders <- users[(users$leader_year <= i & users$leader_year != 0) , ]
  institutions <- users[(users$institution == 1) , ]
  people <- users[(users$institution == 0 & users$leader_year == 0) , ]
  
  # Si es institución, lo seteamos como 2
  V(network)$type <- as.integer(users$institution[match(V(network)$name, users$name)]) + 1   
  
  # Calculamos la asortatividad
  V(network)$type[is.na(V(network)$type)] <- 1
  assortativity.institutions <- assortativity.nominal(network, types=V(network)$type)
  
  # Determinamos quienes fueron líderes ese año, los seteamos como 3.
  for(j in 1:nrow(leaders)){
    V(network)[V(network)$name == leaders[j, ]$name ]$type <- 3
  }
  
  # Determinamos el tamaño 
  V(network)$size = log(degree(network))
  p <- quantile(degree(network), c(.999))
  
  # Generamos un plot más pequeño
  network.small <- delete.vertices(network,which(degree(network) < p))
  print(vcount(network.small))
  
  # Generamos los gráficos
  types.nums.f <- as.factor(V(network.small)$type) 
  types.nums <- as.numeric(types.nums.f)
  l <- layout.lgl(network.small)
  pdf(paste("../images/network.", i,".pdf",sep = ""),10,10)
  plot(network.small, layout=l, main=i, vertex.label= NA, edge.width = 0, edge.color = NA, edge.arrow.size=0, edge.arrow.width=0, vertex.frame.color = NA, vertex.color=types.nums, vertex.size= V(network.small)$size)
  #plot(network.small, layout=l, main=i, vertex.label = V(network.small)$name, vertex.color=types.nums, vertex.size= V(network.small)$size)
  #plot(network.small, layout=l, main=i, vertex.label = ifelse(degree(network.small) > 1, V(network.small)$name, NA), vertex.color=types.nums, vertex.size= V(network.small)$size)
  dev.off()
}

