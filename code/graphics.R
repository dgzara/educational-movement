folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')
library(grid)
library(ggplot2)
library(scales)
library(gridExtra)

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

  # Asignamos a todos como people
  V(network)$group <- "people"
  
  # Asignamos los grupos
  V(network)[V(network)$name %in% leaders]$group <- "leaders"
  V(network)[V(network)$name %in% medios]$group <- "media"
  V(network)[V(network)$name %in% famosos]$group <- "celebrities"
  V(network)[V(network)$name %in% orgs]$group <- "organizations"
  V(network)[V(network)$name %in% movs]$group <- "movements"
  
  # Average neighbor degree versus vertex degree (log–log scale)
  network.simplified <- simplify(network)
  a.nn.deg.network <- graph.knn(network.simplified,V(network.simplified))$knn 
  d.network.simplified <- degree(network.simplified)
  
  # Genero los datos
  x <- as.numeric(d.network.simplified)
  y <- as.numeric(a.nn.deg.network)
  z <- V(network.simplified)$group
  d <- data.frame(x, y, z)
  
  # Construyo el grafico
  a <- ggplot(d, aes(x=x, y=y, group=z, color=z)) + geom_point() +
    scale_x_log10(limits = c(1, NA), 
                  labels = trans_format("log10", math_format(10^.x)),
                  breaks=trans_breaks("log10", function(x) 10^x, n=4)) +
    scale_y_log10(limits = c(1, NA),
                  labels = trans_format("log10", math_format(10^.x)),
                  breaks=trans_breaks("log10", function(x) 10^x, n=4)) +
    theme_bw() + theme(panel.grid.minor = element_line(color="blue", linetype="dotted"), panel.grid.major = element_line(color="blue", linetype="dotted")) +
    xlab("Log Vertex Degree") +
    ylab("Log Average Neighbor Degree") +
    labs(colour = "Type") +
    annotation_logticks(base = 10) + ggtitle(i)
  
  #Asigno el grafico a una variable
  assign(paste("plot", i, sep = ""), a)
}

#merge all three plots within one grid (and visualize this)
png("neighbor2.png", width = 4, height = 4, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot2010, vp = vplayout(1, 1))
print(plot2011, vp = vplayout(1, 2))
print(plot2012, vp = vplayout(2, 1))
print(plot2013, vp = vplayout(2, 2))
dev.off()

#save

# Creamos la leyenda
g <- ggplotGrob(plot2010 + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

#Plot final
pdf("../data/plots/neighbor/final.pdf",10,10)
plot.final <- grid.arrange(arrangeGrob(plot2010 + theme(legend.position="none"),
                                       plot2011 + theme(legend.position="none"),
                                       plot2012 + theme(legend.position="none"),
                                       plot2013 + theme(legend.position="none"),
                                       nrow=2),
                           legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight)) 
dev.off()