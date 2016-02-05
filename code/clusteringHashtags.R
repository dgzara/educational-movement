folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')
library(tm)
library(proxy)
library(topicmodels)

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

# Generamos un documento para cada conferencia
texts <- c()
for(i in hashtags){
  tweets.hashtag <- tweets[(tweets$hashtag == i) , ]
  string <- paste(tweets.hashtag$tweet, collapse=";")
  texts <- rbind(texts, as.character(string))
  rm(tweets.hashtag, string)
}

# Creamos el corpus
vs <- VectorSource(texts)
corpus <- Corpus(vs, readerControl = list(reader = readPlain, language = "es", load = TRUE))

## Remove Stopwords
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

## do tfxidf
dtm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, removeNumbers = TRUE, weighting = weightTfIdf, stopwords = TRUE, tolower = TRUE))
dtm2 <- TermDocumentMatrix(corpus, control = list(minDocFreq=100, removePunctuation = TRUE, removeNumbers = TRUE, weighting = weightTfIdf, stopwords = TRUE, tolower = TRUE))

### don't forget to normalize the vectors so Euclidean makes sense
g = LDA(dtm, 10, method = 'VEM', control=NULL, model=NULL)
