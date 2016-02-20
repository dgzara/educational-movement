folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')

# Obtenemos los tweets de los hashtags
q <- paste('SELECT *  
           FROM hashtag',sep="")
hashtags <- dbGetQuery(con, q)
hashtags <- na.omit(hashtags)

q <- paste('SELECT *  
           FROM hashtag_user_network',sep="")
hashtags.users <- dbGetQuery(con, q)
hashtags.users <- na.omit(hashtags.users)

q <- paste('SELECT *  
           FROM hashtags_time',sep="")
hashtags.time <- dbGetQuery(con, q)
hashtags.time <- na.omit(hashtags.time)

q <- paste('SELECT *  
           FROM menciones',sep="")
menciones <- dbGetQuery(con, q)
menciones <- na.omit(menciones)

q <- paste('SELECT *  
           FROM tweets',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

q <- paste('SELECT *  
           FROM tweets_network',sep="")
tweets.network <- dbGetQuery(con, q)
tweets.network <- na.omit(tweets.network)

q <- paste('SELECT *  
           FROM users',sep="")
users <- dbGetQuery(con, q)
users <- na.omit(users)

q <- paste('SELECT CAST(created_at AS DATE) as date, CAST(created_at AS TIME) as time, favorited, in_reply_to_screen_name, permanent_link, source, text, user_profile_description, user_profile_location, user_followers_count, user_name, user_screen_name, user_time_zone
           FROM users_topsy',sep="")
users.topsy <- dbGetQuery(con, q)
users.topsy <- na.omit(users.topsy)