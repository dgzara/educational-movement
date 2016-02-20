folder <- paste0(getwd(),"/code")
setwd(folder)
#setwd('/Users/dgzara/Documents/PUC/Investigación/Second Life/second-life/code')
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')
library(ggplot2)
library(gridExtra)
library(grid)

# Cargamos los datos
data.sentiment <- read.csv2("../data/sentiment/2011@camila_vallejo.txt", header = TRUE, sep = "|", dec = ".")
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2012@camila_vallejo.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2013@camila_vallejo.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2011@gabrielboric.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2012@gabrielboric.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2013@gabrielboric.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2011@GiorgioJackson.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2012@GiorgioJackson.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment <- rbind(data.sentiment, read.csv2("../data/sentiment/2013@GiorgioJackson.txt", header = TRUE, sep = "|", dec = "."))

# Creamos el date
data.sentiment$date <- as.Date(paste(data.sentiment$year,"-",sprintf("%02d", data.sentiment$month),"-01",sep=""), format="%Y-%m-%d")

# Ploteamos
deputies <- c("@camila_vallejo","@gabrielboric","@GiorgioJackson")
for(i in deputies){
  data.deputy <- data.sentiment[(data.sentiment$account == i),]
  data.deputy.positive <- data.deputy[,c("account", "date", "posemo")]
  data.deputy.negative <- data.deputy[,c("account", "date", "negemo")]
  colnames(data.deputy.positive) <- c("account", "date", "value")
  colnames(data.deputy.negative) <- c("account", "date", "value")
  data.deputy.positive$type <- "positive"
  data.deputy.negative$type <- "negative"
  data.deputy.negative$value <- abs(data.deputy.negative$value)
  data.deputy <- rbind(data.deputy.positive, data.deputy.negative)
  data.deputy$type <- factor(data.deputy$type, levels = c("positive", "negative"))
  rm(data.deputy.positive, data.deputy.negative)
  
  p1 <- ggplot(data = data.deputy, aes(x=date, y = value, group = type, color=type)) +
    xlab("") + ylab("Sentiment") + labs(colour = "Type") +
    geom_line() + 
    ggtitle(i) +
    theme(panel.background = element_rect(fill = "white")) +
    coord_cartesian(ylim=c(0.8,3.25 )) +
    #facet_wrap(~type, ncol = 1, scales = "free_y") +
    scale_color_manual(values=c("#56B4E9", "#D55E00"))
  ggsave(paste0("../data/plots/sentiment/",i,".pdf",sep = ""), plot = p1)
  
  assign(paste("plot.", gsub("@", "", i), sep = ""), p1)
}

g <- ggplotGrob(plot.camila_vallejo + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

# Guardamos
pdf("../data/plots/sentiment/final.pdf",14,4)
plot.final <- grid.arrange(arrangeGrob(plot.camila_vallejo + theme(legend.position="none"),
                         plot.gabrielboric + theme(legend.position="none"),
                         plot.GiorgioJackson + theme(legend.position="none"),
                         nrow=1),
             legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight)) 
dev.off()

# Exportamos
data.final.positive <- data.sentiment[,c("account", "date", "posemo")]
data.final.negative <- data.sentiment[,c("account", "date", "negemo")]
colnames(data.final.positive) <- c("account", "date", "value")
colnames(data.final.negative) <- c("account", "date", "value")
data.final.positive$type <- "positive"
data.final.negative$type <- "negative"
data.final.negative$value <- abs(data.final.negative$value)
data.final <- rbind(data.final.positive, data.final.negative)
write.csv(data.final, "../data/enero2016/sentiment/data.final.csv", row.names=TRUE)

# Cargamos los datos del sentimiento propio generado
data.sentiment.self <- read.csv2("../data/sentiment_self/2011camila_vallejo2.txt", header = TRUE, sep = "|", dec = ".")
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2012camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2013camila_vallejo2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2011gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2012gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2013gabrielboric2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2011GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2012GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))
data.sentiment.self <- rbind(data.sentiment.self, read.csv2("../data/sentiment_self/2013GiorgioJackson2.txt", header = TRUE, sep = "|", dec = "."))

# Creamos el date
data.sentiment.self$date <- as.Date(paste(data.sentiment.self$year,"-",sprintf("%02d", data.sentiment.self$month),"-01",sep=""), format="%Y-%m-%d")

# Ploteamos
deputies <- c("camila_vallejo","gabrielboric","GiorgioJackson")
for(i in deputies){
  data.deputy <- data.sentiment.self[(data.sentiment.self$account == i),]
  data.deputy.positive <- data.deputy[,c("account", "date", "posemo")]
  data.deputy.negative <- data.deputy[,c("account", "date", "negemo")]
  colnames(data.deputy.positive) <- c("account", "date", "value")
  colnames(data.deputy.negative) <- c("account", "date", "value")
  data.deputy.positive$type <- "positive"
  data.deputy.negative$type <- "negative"
  data.deputy.negative$value <- abs(data.deputy.negative$value)
  data.deputy <- rbind(data.deputy.positive, data.deputy.negative)
  data.deputy$type <- factor(data.deputy$type, levels = c("positive", "negative"))
  rm(data.deputy.positive, data.deputy.negative)
  
  p1 <- ggplot(data = data.deputy, aes(x=date, y = value, group = type, color=type)) +
    xlab("") + ylab("Sentiment") + labs(colour = "Type") +
    geom_line() + 
    ggtitle(i) +
    theme(panel.background = element_rect(fill = "white")) +
    coord_cartesian(ylim=c(0.8,3.25 )) +
    #facet_wrap(~type, ncol = 1, scales = "free_y") +
    scale_color_manual(values=c("#56B4E9", "#D55E00"))
  ggsave(paste0("../data/plots/sentiment_self/",i,".pdf",sep = ""), plot = p1)
  
  assign(paste("plot.", gsub("@", "", i), sep = ""), p1)
}

g <- ggplotGrob(plot.camila_vallejo + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

# Guardamos
pdf("../data/plots/sentiment_self/final.pdf",14,4)
plot.final <- grid.arrange(arrangeGrob(plot.camila_vallejo + theme(legend.position="none"),
                                       plot.gabrielboric + theme(legend.position="none"),
                                       plot.GiorgioJackson + theme(legend.position="none"),
                                       nrow=1),
                           legend, nrow=2,heights=unit.c(unit(1, "npc") - lheight, lheight)) 
dev.off()