folder <- paste0(getwd(),"/code")
setwd(folder)
#setwd('/Users/dgzara/Documents/PUC/Investigación/Second Life/second-life/code')
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')
library("markovchain")
library(plyr)

# Obtengo la bd
q <- paste('SELECT *  
           FROM hashtag_user_network',sep="")
hashtags.users <- dbGetQuery(con, q)
hashtags.users <- na.omit(hashtags.users)

# Creamos los usuarios
users.source <- sort(unique(unlist(hashtags.users$source, use.names = FALSE)))
users.target <- sort(unique(unlist(hashtags.users$target, use.names = FALSE)))
users <- c(users.source, users.target)
users <- as.data.frame(sort(unique(unlist(users, use.names = FALSE))))
colnames(users) <- c("username")
users$username <- as.character(users$username)
users$grupo<- "people"

# Obtenemos los match
users.celebrities <- users[(users$username %in% famosos), ]
users.movs <- users[(users$username %in% movs), ]
users.orgs <- users[(users$username %in% orgs), ]
users.leaders <- users[(users$username %in% leaders), ]
users.media <- users[(users$username %in% medios), ]
users.people <- users[!(users$username %in% medios | users$username %in% movs | users$username %in% orgs | users$username %in% leaders | users$username %in% famosos), ]

# Hacemos la clasificación
users.celebrities$grupo <- "celebrity"
users.movs$grupo <- "movs"
users.orgs$grupo <- "orgs"
users.leaders$grupo <- "leaders"
users.media$grupo <- "media"
users.people$grupo<- "people"

# Juntamos todo
users <- rbind(users.celebrities, users.movs, users.orgs, users.leaders, users.media, users.people)

# Generamos la red
network <- hashtags.users[,c("source", "target", "type")]

n1 <- merge(network, users, by.x="source", by.y="username")
colnames(n1) <- c("source", "target", "type", "grupo_source")
n1 <- merge(n1, users, by.x="target", by.y="username")
colnames(n1) <- c("target", "source", "type", "grupo_source", "grupo_target")
n1 <- n1[c("source", "target", "type", "grupo_source", "grupo_target")]

# Calculamos las probabilidades
proportions <- c()
proportions.totals <- c()
grupos <- c("celebrity", "movs", "orgs", "leaders", "media", "people")

for(i in grupos)
{
  tweetsReply.total <- nrow(n1[(n1$grupo_source == i & n1$type == "reply"), ])
  tweetsMention.total <- nrow(n1[(n1$grupo_source == i & n1$type == "mention"), ])
  tweetsRetweet.total <- nrow(n1[(n1$grupo_source == i & n1$type == "retweet"), ])
  tweetsTotal.total <- nrow(n1[(n1$grupo_source == i), ])
  
  for(j in grupos)
  {
    tweetsReply <- nrow(n1[(n1$grupo_source == i & n1$grupo_target == j & n1$type == "reply"), ])
    tweetsMention <- nrow(n1[(n1$grupo_source == i & n1$grupo_target == j & n1$type == "mention"), ])
    tweetsRetweet <- nrow(n1[(n1$grupo_source == i & n1$grupo_target == j & n1$type == "retweet"), ])
    tweetsTotal <- nrow(n1[(n1$grupo_source == i & n1$grupo_target == j), ])
    
    row <- c()
    row$source <- i
    row$target <- j
    row$proportion.reply <- tweetsReply/tweetsReply.total
    row$proportion.mention <- tweetsMention/tweetsMention.total
    row$proportion.retweet <- tweetsRetweet/tweetsRetweet.total
    row$proportion.total <- tweetsTotal/tweetsTotal.total
    proportions <- rbind(proportions, as.data.frame(row))
    
    row <- c()
    row$source <- i
    row$target <- j
    row$reply <- tweetsReply
    row$mention <- tweetsMention
    row$retweet <- tweetsRetweet
    row$total <- tweetsTotal
    proportions.totals <- rbind(proportions.totals, as.data.frame(row))
  }
}
rm(tweetsMention, tweetsMention.total, tweetsReply, tweetsReply.total, tweetsRetweet, tweetsRetweet.total, row)
write.csv(proportions, paste("../data/enero2016/proportions/proportions.csv", sep = ""), row.names=TRUE)
write.csv(proportions.totals, paste("../data/enero2016/proportions/total.csv", sep = ""), row.names=TRUE)

# Calculamos el número de usuarios
numberUsers <- c()
numberUsers$group <- c("celebrity", "movs", "orgs", "leaders", "media", "people")
numberUsers$number <- c(nrow(users.celebrities), nrow(users.movs), nrow(users.orgs), nrow(users.leaders), nrow(users.media), nrow(users.people))
numberUsers <- as.data.frame(numberUsers)
numberUsers$group <- as.character(numberUsers$group)
write.csv(numberUsers, paste("../data/enero2016/proportions/number.csv", sep = ""), row.names=TRUE)

# Generamos el archivo para el gephi
proportions.mention <- proportions[,c('source', 'target', 'proportion.mention')]
proportions.mention <- merge(proportions.mention, numberUsers, by.x=c('source'), by.y = c('group'))
colnames(proportions.mention) <- c('from', 'to', 'weight', 'from_size')
write.csv(proportions.mention, paste("../data/enero2016/proportions/markov_mention.csv", sep = ""), row.names=TRUE)

proportions.retweet <- proportions[,c('source', 'target', 'proportion.retweet')]
proportions.retweet <- merge(proportions.retweet, numberUsers, by.x=c('source'), by.y = c('group'))
colnames(proportions.retweet) <- c('from', 'to', 'weight', 'from_size')
write.csv(proportions.retweet, paste("../data/enero2016/proportions/markov_retweet.csv", sep = ""), row.names=TRUE)

proportions.reply <- proportions[,c('source', 'target', 'proportion.reply')]
proportions.reply <- merge(proportions.reply, numberUsers, by.x=c('source'), by.y = c('group'))
colnames(proportions.reply) <- c('from', 'to', 'weight', 'from_size')
write.csv(proportions.reply, paste("../data/enero2016/proportions/markov_reply.csv", sep = ""), row.names=TRUE)

# MARKOV
# nodeSizes <- setNames(log10(numberUsers$number)*20, numberUsers$group)
# groups <- c("celebrity", "movs", "orgs", "leaders", "media", "people")
# proportions.reply <- proportions[,c('source', 'target', 'proportion.reply')]
# proportions.reply <- dcast(proportions.reply, source~target, value.var="proportion.reply")
# proportions.reply$source <- NULL
# transitionMatrix.reply <- data.matrix(proportions.reply)
# transitionMatrix.reply[,6] <- transitionMatrix.reply[,6]+ c(0, -0.001, 0.007, 0.001, 0.01, -0.0036)
# mcReply = new("markovchain", states = groups, byrow = TRUE, transitionMatrix = transitionMatrix.reply, name = "Reply")
# plot(mcReply, edge.arrow.size=0.5, edge.size = transitionMatrix.reply, vertex.size=as.matrix(nodeSizes), main="Replies proportions", edge.curved=seq(-0.5, 0.5, length = 36))
# 
# 
# proportions.mention <- proportions[,c('source', 'target', 'proportion.mention')]
# proportions.mention <- dcast(proportions.mention, source~target, value.var="proportion.mention")
# proportions.mention$source <- NULL
# 
# proportions.retweet <- proportions[,c('source', 'target', 'proportion.retweet')]
# proportions.retweet <- dcast(proportions.retweet, source~target, value.var="proportion.retweet")
# proportions.retweet$source <- NULL