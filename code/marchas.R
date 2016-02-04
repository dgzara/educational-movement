folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')

# Obtenemos los tweets
q <- paste('SELECT id, account as marcha, tweetuser as user, tweet, tweetdate as date
             FROM hashtag',sep="")
tweets <- dbGetQuery(con, q)

# Obtenemos la lista de las marchas
q <- paste('SELECT DISTINCT(account) as marcha
             FROM hashtag',sep="")
marchas <- dbGetQuery(con, q)

# Revisamos por hashtag
for(i in 1:nrow(marchas))
{
  tweetsHashtag <- tweets[tweets$marcha == marchas[i, 1],]
  
  print(paste(marchas[i, 1], ': ', nrow(tweetsHashtag)))
}

