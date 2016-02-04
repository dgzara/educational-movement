folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
library(grid)
library(ggplot2)
library(scales)

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

# Realizamos el for
for(i in 2010:2013)
{
  tweetsYear <- selectByDate(tweets, year = i)
  tweetsYear <- tweetsYear[,c("source","target")]
  
  # Generamos el grafo
  network <- graph.data.frame(tweetsYear, directed=TRUE)

  # Average neighbor degree versus vertex degree (log–log scale)
  network.simplified <- simplify(network)
  a.nn.deg.network <- graph.knn(network.simplified,V(network.simplified))$knn 
  d.network.simplified <- degree(network.simplified)
  
  # Genero los datos
  x <- as.numeric(d.network.simplified)
  y <- as.numeric(a.nn.deg.network)
  d <- data.frame(x, y)
  
  # Construyo el grafico
  a <- ggplot(d, aes(x=x, y=y)) + geom_point() +
    scale_x_log10(limits = c(1, NA), 
                  labels = trans_format("log10", math_format(10^.x)),
                  breaks=trans_breaks("log10", function(x) 10^x, n=4)) +
    scale_y_log10(limits = c(1, NA),
                  labels = trans_format("log10", math_format(10^.x)),
                  breaks=trans_breaks("log10", function(x) 10^x, n=4)) +
    theme_bw() + theme(panel.grid.minor = element_line(color="blue", linetype="dotted"), panel.grid.major = element_line(color="blue", linetype="dotted")) +
    xlab("Log Vertex Degree") +
    ylab("Log Average Neighbor Degree") +
    annotation_logticks(base = 10) + ggtitle(i)
  
  #Asigno el grafico a una variable
  assign(paste("plot", i, sep = ""), a)
}

#merge all three plots within one grid (and visualize this)
png("neighbor.png", width = 4, height = 4, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot2010, vp = vplayout(1, 1))
print(plot2011, vp = vplayout(1, 2))
print(plot2012, vp = vplayout(2, 1))
print(plot2013, vp = vplayout(2, 2))
dev.off()

#save
