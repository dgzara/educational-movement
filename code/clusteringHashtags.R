folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')
library(tm)
library(proxy)
library(topicmodels)
library(ggplot2)
library(ggdendro)

# Obtenemos los tweets de los hashtags
q <- paste('SELECT *  
           FROM hashtag',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$account, use.names = FALSE)))

# Reordenamos
colnames(tweets) <- c("id", "hashtag", "user", "tweet", "date")
tweets$id <- NULL

# Generamos un documento para cada hashtag
texts <- c()
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i), ]
  
  # Creamos el corpus
  vs <- VectorSource(tweets.hashtag$tweet)
  corpus <- Corpus(vs, readerControl = list(reader = readPlain, language = "es", load = TRUE))
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  
  ## Creamos la matriz
  tdm <- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE, removeNumbers = TRUE, weighting = weightTf, stopwords = TRUE, tolower = TRUE, bounds = list(global = c(50,Inf))))
  
  # Vemos la matriz
  rowTotals <- apply(tdm , 1, sum) #Find the sum of words in each Document
  tdm.new   <- tdm[rowTotals> 0, ]
  
  # Creamos LDA con 10 tópicos
  lda <- LDA(tdm.new, 10)
  terms <- terms(lda, 20)
  
  # Juntamos
  string <- paste(terms, collapse=" ")
  row <- as.data.frame(as.character(string))
  rownames(row) <- c(i)
  texts <- rbind(texts, row)
  
  # Eliminamos las variables
  rm(vs, corpus, tdm, lda, t, tdm.new, rowTotals, string, terms, row)
}

# Escribimos el archivo
write.csv(texts, paste("../data/enero2016/lda.csv", sep = ""), row.names=TRUE)

# Creamos el corpus de todas las conferencias
vs <- VectorSource(texts$texts)
corpus <- Corpus(vs, readerControl = list(reader = readPlain, language = "es", load = TRUE))

## Creamos la matriz
tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, weighting = weightTfIdf))

# Obtenemos y transponemos la matriz de pesos
m <- t(weightTfIdf(tdm))
rownames(m) <- rownames(texts)

# Creamos el dendograma
d <- dist(as.matrix(m))
hc <- hclust(d, method="ward.D")
dhc <- as.dendrogram(hc)

# Ploteamos
ddata <- dendro_data(dhc, type = "rectangle")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(1, 0)) +
  theme_dendro() +
  geom_text(aes(x = x, y = y, label = label, angle = 0, hjust = 0), data= label(ddata), size=4)
p
